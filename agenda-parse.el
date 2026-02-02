;;; agenda-parse.el --- Parsing, test and collect functions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/agenda.txt
;; Parent-Package: agenda

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;; Commentary:

;; This module provides all parsing and test functions for processing
;; agenda source file.
;;
;; It handles:
;;   - Identification of entries via regular expressions.
;;   - Various tests on string or entries
;;   - Convertion of raw text lines into entry
;;   - Conversion of entries field into string
;;   - Collect entries based on a filter function
;;   - Compute 30mn slots for a given date
;;
;; All collect functions use a cache whose invalidation is based on
;; buffer modified tick. Cache can be bypassed with the force
;; parameter in relevant functions.

;;; Code:
(require 'rx)

(defconst agenda-entry-re
  (rx line-start
      ;; --- DATE BLOCK ---
      (submatch-n 1 ;; date (e.g., "2026-01-17 W02 Sat")
        (submatch-n 2 ;; date-iso (e.g., "2026-01-17")
          (submatch-n 3 (repeat 4 digit)) "-" ;; date-y
          (submatch-n 4 (repeat 2 digit)) "-" ;; date-m
          (submatch-n 5 (repeat 2 digit)))    ;; date-d
        (optional (one-or-more space) (any "Ww") (one-or-more digit))
        (optional (one-or-more space) (one-or-more letter)))
      (one-or-more space)
      ;; --- TIME BLOCK ---
      (optional
       (submatch-n 6 ;; time (e.g., "08:30-10:00")
         (submatch-n 7 ;; time-start (e.g., "08:30")
           (submatch-n 8 (repeat 2 digit)) ":" ;; time-start-h
           (submatch-n 9 (repeat 2 digit)))    ;; time-start-m
         (optional "-" 
                   (submatch-n 10 ;; time-end (e.g., "10:00")
                     (submatch-n 11 (repeat 2 digit)) ":" ;; time-end-h
                     (submatch-n 12 (repeat 2 digit))))))  ;; time-end-m

      (optional (one-or-more space) (one-or-more "-")) ;;  Aesthetic dashes
      (one-or-more space)
      
      ;; --- CONTENT BLOCK ---
      ;;      (submatch-n 13 (minimal-match (zero-or-more (not (any "@+#")))) 
      ;;                  (not (any "@+# "))) ;; non-empty title
      (submatch-n 13 (zero-or-more (not (any "@+#\n")))) ;; title
      
      (optional (one-or-more space)
                (submatch-n 14 (one-or-more (seq "@" (one-or-more (not (any space "+@#"))) 
                                                (zero-or-more space))))) ;; actions
      (optional (one-or-more space)
                (submatch-n 15 (one-or-more (seq "+" (one-or-more (not (any space "+@#"))) 
                                                 (zero-or-more space))))) ;; tags
      ;; --- UID BLOCK ---
      (optional (one-or-more space)
                (submatch-n 16 "[" (one-or-more (any alnum "-")) "]")) ;; [UID]
      line-end)
  "Matches an entry line with several groups that can be accessed using the
'agenda-match' function.")

(defconst agenda-data-re
  (rx line-start
      "["
      (submatch-n 1 ;; Full data ref (UID:KEY)
        (submatch-n 2 (one-or-more (any alnum "-"))) ;; UID
        ":"
        (submatch-n 3 (one-or-more alpha))) ;; KEY
      "]:"
      (zero-or-more space)
      (submatch-n 4 (zero-or-more not-newline)) ;; Content (value)
      line-end)
  "Matches a data line with several groups that can be accessed using
the 'agenda-match' function.")

(defconst agenda-header-re
  (rx line-start
      (submatch-n 1 (one-or-more "#")
                    (one-or-more space))
      (group (one-or-more (not (any ":" "\n")))) ;; No colons allowed
      line-end)
  "Matches a header line.")

(defconst agenda-comment-re
  (rx line-start
      "#" (one-or-more space)
      (group (zero-or-more not-newline))
      line-end)
  "Matches a comment line.")

(defconst agenda-group-map
  '(;; All
    (all . 0)
    ;; Entry line (1-16) 
    (date         . 1) (date-iso     . 2)
    (date-y       . 3)  (date-m       . 4) (date-d       . 5)
    (time         . 6)
    (time-start   . 7)  (time-start-h . 8) (time-start-m . 9)
    (time-end     . 10) (time-end-h  . 11) (time-end-m  . 12)
    (title        . 13) (actions     . 14) (tags        . 15)
    (uid          . 16) (ref         . 16) 
    ;; Data line (1-4)
    (data . 0)
    (data-ref . 1)  (data-uid . 2) (data-key . 3) 
    (data-value . 4))
  "Fixed mapping of semantic names to group indices for both
'agenda-entry-re' and 'agenda-data-re'")

(defun agenda-group (type)
  "Get the group number for TYPE (from the 'agenda-group map')."
  (or (cdr (assoc type agenda-group-map))
      (error "Unknown agenda group: %s" type)))

(defun agenda-match (type &optional string)
  "Return (MATCH-STRING . (BEG . END)) for group TYPE and optional STRING
by the previous search or regexp operation."
  (let ((group (agenda-group type)))
    (when (match-beginning group)
      (cons (match-string-no-properties group string)
            (cons (match-beginning group) 
                  (match-end group))))))

(defun agenda-groups-at-point ()
  "Return list of groups at point, sorted by descending specificity."
  (save-excursion
    (let ((point (point)))
      (beginning-of-line)
      (when-let* ((line-type (cond ((looking-at agenda-entry-re) :entry)
                                   ((looking-at agenda-data-re)  :data)
                                   (t nil))))
        (let (matches)
          (dolist (entry agenda-group-map)
            (let* ((sym (car entry))
                   (group (cdr entry))
                   (is-data (string-prefix-p "data-" (symbol-name sym))))
              (when (eq (if is-data :data :entry) line-type)
                (let ((beg (match-beginning group))
                      (end (match-end group)))
                  (when (and beg end (>= point beg) (<= point end))
                    (push (cons sym (- end beg)) matches))))))
          (mapcar #'car (sort matches (lambda (a b) (> (cdr a) (cdr b))))))))))

(defun agenda-lookup-data (uid)
  "Scan buffer backwards for data lines matching UID and return an alist
of (key . value)."
  (save-match-data
    (save-excursion
      (goto-char (point-max))
      (let ((results `((uid . ,uid))))
        (while (re-search-backward agenda-data-re nil t)
          (when (string= (car (agenda-match 'data-uid)) uid)
            (push (cons (intern (downcase (car (agenda-match 'data-key))))
                        (car (agenda-match 'data-value)))
                  results)))
        results))))

(defun agenda-entry-from-match ()
  "Return an entry from previous search or regexp operation."
  (let* ((date (list (string-to-number (car (agenda-match 'date-y)))
                     (string-to-number (car (agenda-match 'date-m)))
                     (string-to-number (car (agenda-match 'date-d)))))
         (time-start (when-let ((h (car (agenda-match 'time-start-h)))
                                (m (car (agenda-match 'time-start-m))))
                       (list (string-to-number h) (string-to-number m))))
         (time-end   (when-let ((h (car (agenda-match 'time-end-h)))
                                (m (car (agenda-match 'time-end-m))))
                       (list (string-to-number h) (string-to-number m))))
         (title   (when-let ((title (car (agenda-match 'title))))
                    (string-trim title)))
         (actions (when-let ((actions (car (agenda-match 'actions))))
                    (save-match-data
                      (split-string (string-trim actions) "@" t))))
         (tags    (when-let ((tags (car (agenda-match 'tags))))
                    (save-match-data
                      (split-string (string-trim tags) "+" t))))
         (marker (save-excursion
                   (goto-char (match-beginning 0))
                   (point-marker)))
         (uid (when-let ((uid (car (agenda-match 'uid))))
                (substring uid +1 -1))) ;; Remove [ and ]
         (data (when uid (agenda-lookup-data uid))))
    `((date       . ,date)
      (marker     . ,marker)
      (time-start . ,time-start)
      (time-end   . ,time-end)
      (title      . ,title)
      (actions    . ,actions)
      (tags       . ,tags)
      (data       . ,data))))

(defun agenda-entry-at-point ()
  "Get entry at point (if any)."
  (save-excursion
    (beginning-of-line)
    (when (looking-at agenda-entry-re)
      (agenda-entry-from-match))))

(defun agenda-data-at-point ()
  "Get data at point (if any)."
  (save-excursion
    (beginning-of-line)
    (when (looking-at agenda-data-re)
      (let ((uid (car (agenda-match 'data-uid)))
            (key (intern (downcase (car (agenda-match 'data-key)))))
            (val (car (agenda-match 'data-value))))
        (cons uid (cons key val))))))

(defun agenda-entry-date-string (entry &optional format)
  "Return ENTRY date as a formatted string using FORMAT.
Default format is YYYY-MM-DD wXX Day."
  (let* ((format (or format "%Y-%m-%d w%V %a"))
         (date (cdr (assoc 'date entry)))
         (year (nth 0 date))
         (month (nth 1 date))
         (day (nth 2 date))
         (timestamp (encode-time (list 0 0 0 day month year))))
    (format-time-string format timestamp)))

(defun agenda-entry-date-iso-string (entry)
  "Return ENTRY date as an ISO date format YYYY-MM-DD."
  (agenda-entry-date-string entry "%Y-%m-%d"))

(defun agenda-entry-time-start-string (entry)
  "Return ENTRY time start as a formatted string HH:MM or nil."
  (when-let* ((time (cdr (assoc 'time-start entry)))
              (time-h (nth 0 time))
              (time-m (nth 1 time)))
    (format "%02d:%02d" time-h time-m)))

(defun agenda-entry-time-end-string (entry)
  "Return ENTRY time end ad a formatted string HH:MM or nil."
  (when-let* ((time (cdr (assoc 'time-end entry)))
              (time-h (nth 0 time))
              (time-m (nth 1 time)))
    (format "%02d:%02d" time-h time-m)))

(defun agenda-entry-time-string (entry)
  "Convert ENTRY time as a formatted string [HH:MM[-HH:MM]]."
  (let* ((time-start (agenda-entry-time-start-string entry))
         (time-end   (agenda-entry-time-end-string entry)))
    (cond ((and time-start time-end) (format "%s-%s" time-start time-end))
          (time-start                time-start))))

(defun agenda-entry-tags-string (entry)
  "Return ENTRY tags formatted for display."
  (when-let* ((tags (cdr (assoc 'tags entry))))
    (mapconcat (lambda (tag) (concat "+" tag)) tags "")))

(defun agenda-entry-actions-string (entry)
  "Return ENTRY actions formatted for display."
  (when-let* ((actions (cdr (assoc 'actions entry))))
    (mapconcat (lambda (action) (concat "@" action)) actions "")))

(defun agenda-entry-title-string (entry)
  "Return ENTRY title formatted for display."
  (cdr (assoc 'title entry)))

(defun agenda-entry-less-p (entry-1 entry-2)
  "Return non-nil if ENTRY1 is before ENTRY2 (lexicographic order
based on entries date+time)."
  (let ((entry-1-str (concat (agenda-entry-date-iso-string entry-1) " "
                            (agenda-entry-time-string entry-1)))
        (entry-2-str (concat (agenda-entry-date-iso-string entry-2) " "
                            (agenda-entry-time-string entry-2))))
    (string< entry-1-str entry-2-str)))

(defun agenda-line-marked-p ()
  "Return non-nil if current line has a margin-mark overlay."
  (seq-find (lambda (ov) (overlay-get ov 'agenda-mark)) 
            (overlays-in (line-beginning-position) (line-end-position))))

(defun agenda-line-future-p ()
  "Return non-nil if line starts with a date >= today."
  (let ((today (format-time-string "%Y-%m-%d"))
        (date (buffer-substring (line-beginning-position) 
                                (+ (line-beginning-position) (length today)))))
    (or (string= date today)
        (string> date today))))

(defun agenda-line-match-p (regex)
  "Return non-nil if REGEX matches the current line."
  (let ((line (buffer-substring-no-properties (line-beginning-position) 
                                              (line-end-position)))
        (case-fold-search t))
    (string-match-p regex line)))

(defun agenda-date-today-p (date-str)
  "Return t if DATE-STR (formatted as 'YYYY-MM-DD') is today."
  (when (stringp date-str)
    (string-prefix-p (format-time-string "%Y-%m-%d") date-str)))

(defun agenda-date-time-now-p (date-str time-str)
  "Return t if DATE-STR (formatted as 'YYYY-MM-DD') is today and
current time is within TIME-STR range (formatted as 'HH:MM-HH:MM')"
  (when (and (stringp date-str) (stringp time-str)
             (agenda-date-today-p date-str))
    (save-match-data
      (when-let* ((now (current-time))
                  (times (string-split time-str "-"))
                  (start-str (nth 0 times))
                  (end-str (or (nth 1 times) start-str)) 
                  (time-start (date-to-time (concat date-str " " start-str)))
                  (time-end (date-to-time (concat date-str " " end-str))))
        (and (not (time-less-p now time-start))
             (time-less-p now time-end))))))

(defun agenda-date-inweek-p (date-str)
  "Return t if DATE-STR (formatted as 'YYYY-MM-DD wXX Day') is in current week."
  (when (stringp date-str)
    (save-match-data
      (string-match-p (format-time-string "[wW]%V") date-str))))

(defun agenda-tags-cancel-p (tags-str)
  "Check if +CANCEL is present in TAGS-STR."
  (when (stringp tags-str)
    (save-match-data
      (string-match-p "+CANCEL" tags-str))))

(defun agenda-tags-deadline-p (tags-str)
  "Check if +DEADLINE is present in TAGS-STR."
  (when (stringp tags-str)
    (save-match-data
      (string-match-p "+DEADLINE" tags-str))))

(defun agenda-tags-active-deadline-p (tags-str)
  "Check if +DEADLINE is present in TAGS-STR but not +DONE."
  (when (stringp tags-str)
    (save-match-data
      (and (string-match-p "+DEADLINE" tags-str)
           (not (string-match-p "+DONE" tags-str))))))

(defun agenda-tags-done-p (tags-str)
  "Check if +DONE is present in TAGS-STR."
  (when (stringp tags-str)
    (save-match-data
      (string-match-p "+DONE" tags-str))))

(defun agenda-tags-personal-p (tags-str)
  "Check if +PERSONAL is present in TAGS-STR."
  (when (stringp tags-str)
    (save-match-data
      (string-match-p "+PERSONAL" tags-str))))

(defun agenda-tags-birthday-p (tags-str)
  "Check if +BIRTHDAY is present in TAGS-STR."
  (when (stringp tags-str)
    (save-match-data
      (string-match-p "+BIRTHDAY" tags-str))))

(defun agenda-entry-inweek-p (entry &optional week year)
  "Return t if entry is in WEEK (str) of YEAR (str)."
  (let* ((date (agenda-entry-date-string entry))
         (week (or week (string-to-number (format-time-string "%V"))))
         (week (format "[wW]%02d" week))
         (year (or year (string-to-number (format-time-string "%Y"))))
         (year (format "%04d" year)))
    (and (string-prefix-p year date)
         (string-match-p week date))))

(defun agenda-entry-between-p (entry &optional beg end)
  "Return t if entry is between BEG and END (YYYY-MM-DD format).
  If BEG is nil, there is no lower bound. If END is nil, there is no upper bound."
  (let ((date (agenda-entry-date-string entry "%Y-%m-%d")))
    (and date
         (or (null beg) (not (string< date beg)))
         (or (null end) (not (string< end date))))))

(defun agenda-entry-strictly-between-p (entry &optional beg end)
  "Return t if entry is strictly between BEG and END (strings in YYYY-MM-DD format).
  If BEG is nil, there is no lower bound. If END is nil, there is no upper bound."
  (let ((date (agenda-entry-date-string entry "%Y-%m-%d")))
    (and date
         (or (null beg) (string< beg date))
         (or (null end) (string< date end)))))

(defun agenda-entry-after-p (entry &optional date)
  "Return t if entry is on or after DATE (YYYY-MM-DD).
  If DATE is nil, defaults to today."
  (let ((after-date (or date (format-time-string "%Y-%m-%d")))
        (entry-date (agenda-entry-date-string entry "%Y-%m-%d")))
    (not (string< entry-date after-date))))

(defun agenda-entry-strictly-after-p (entry &optional date)
  "Return t if entry is strictly after DATE (YYYY-MM-DD).
  If DATE is nil, defaults to today."
  (let ((after-date (or date (format-time-string "%Y-%m-%d")))
        (entry-date (agenda-entry-date-string entry "%Y-%m-%d")))
    (string< after-date entry-date)))

(defun agenda-entry-before-p (entry &optional date)
  "Return t if entry is on or before DATE (YYYY-MM-DD).
  If DATE is nil, defaults to today."
  (let ((before-date (or date (format-time-string "%Y-%m-%d")))
        (entry-date (agenda-entry-date-string entry "%Y-%m-%d")))
    (not (string< before-date entry-date))))

(defun agenda-entry-strictly-before-p (entry &optional date)
  "Return t if entry is strictly before DATE (YYYY-MM-DD).
  If DATE is nil, defaults to today."
  (let ((before-date (or date (format-time-string "%Y-%m-%d")))
        (entry-date (agenda-entry-date-string entry "%Y-%m-%d")))
    (string< entry-date before-date)))

(defun agenda-entry-date-p (entry &optional date)
  "Return t if entry falls on DATE (YYYY-MM-DD).
  If DATE is nil, defaults to today."
    (string= (or date (format-time-string "%Y-%m-%d"))
             (agenda-entry-date-iso-string entry)))

(defun agenda-entry-today-p (entry)
  "Return t if entry is today."
  (agenda-entry-date-p entry))

(defun agenda-entry-now-p (entry)
  "Return t if entry is now."
  (let ((time-start-hm (agenda-entry-time-start-string entry))
        (time-end-hm (agenda-entry-time-end-string entry))
        (now-hm (format-time-string "%H:%M"))
        (is-today (agenda-entry-today-p entry)))
    (and is-today time-start-hm
         (not (string< now-hm time-start-hm))
         (or (not time-end-hm)
             (string< now-hm time-end-hm)))))

(defun agenda-entry-cancel-p (entry)
  "Return t if entry has CANCEL in tags"
  (when-let ((tags (cdr (assoc 'tags entry))))
    (member "CANCEL" tags)))

(defun agenda-entry-deadline-p (entry)
  "Return t if entry has DEADLINE in tags"
  (when-let ((tags (cdr (assoc 'tags entry))))
    (member "DEADLINE" tags)))

(defun agenda-entry-active-deadline-p (entry)
  "Return t if entry has DEADLINE in tags"
  (when-let ((tags (cdr (assoc 'tags entry))))
    (and (member "DEADLINE" tags)
         (not (member "DONE" tags)))))

(defun agenda-entry-todo-p (entry)
  "Return t if entry has TODO in actions"
  (when-let ((actions (cdr (assoc 'actions entry))))
    (member "DONE" actions)))

(defun agenda-entry-done-p (entry)
  "Return t if entry has DONE in tags"
  (when-let ((tags (cdr (assoc 'tags entry))))
    (member "DONE" tags)))

(defun agenda-entry-birthday-p (entry &optional week day)
  "Return t if entry has BIRTHDAY in tags and week is WEEK and DAY."
  (when-let* ((date (agenda-entry-date-string entry))
              (entry-date (cdr (assoc 'date entry)))
              (tags (cdr (assoc 'tags entry)))
              (week (or week (string-to-number (format-time-string "%V"))))
              (week (format "[wW]%02d" week)))
    (and (member "BIRTHDAY" tags)
         (or (not day)
             (eq day (nth 2 entry-date)))
         (string-match-p week date))))

(defvar-local agenda-source-buffer nil
  "Agenda source buffer")

(defun agenda-source-buffer ()
  "Return the agenda source buffer."
  (unless (and agenda-source-buffer (buffer-live-p agenda-source-buffer))
    (unless (and agenda-file (file-exists-p agenda-file))
      (error "Agenda file not found: %s. " agenda-file))
    (setq agenda-source-buffer (find-file-noselect agenda-file)))
  agenda-source-buffer)

(defvar-local agenda--entries-cache nil
  "Cache storage for entries as (TICK . ENTRIES).")

(defun agenda-collect-entries (&optional force)
  "Retrieve all entries from the source buffer.
If FORCE is non-nil, ignore the cache and re-scan."
  (with-current-buffer (agenda-source-buffer)
    (let ((current-tick (buffer-chars-modified-tick (agenda-source-buffer))))
      (if (and (not force)
               agenda--entries-cache
               (eq (car agenda--entries-cache) current-tick))
          (cdr agenda--entries-cache)
        (let ((entries nil))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward agenda-entry-re nil t)
              (push (agenda-entry-from-match) entries)))
          (setq agenda--entries-cache (cons current-tick entries))
          entries)))))

(defun agenda-filter-entries (filter-fn &rest args)
  "Collect and return a sorted list of entries that satisfy FILTER-FN.
FILTER-FN is called on each entry with supplementary ARGS."
  (let* ((filtered nil))
    (dolist (entry (agenda-collect-entries))
      (when (apply filter-fn entry args)
        (push entry filtered)))
    (sort filtered #'agenda-entry-less-p)))

(defvar-local agenda--uids-cache nil
  "Cache storage for UIDs as (TICK . UIDS).")

(defun agenda-collect-uids (&optional force)
  "Scan agenda buffer for all UIDs (entry or data lines).
If FORCE is non-nil, ignore the cache and re-scan."
  (let ((current-tick (buffer-chars-modified-tick (agenda-source-buffer))))
    (if (and (not force)
             agenda--uids-cache 
             (eq (car agenda--uids-cache) current-tick))
        (cdr agenda--uids-cache)
      (let (uids)
        (save-excursion
          ;; Entry lines
          (goto-char (point-min))
          (while (re-search-forward agenda-entry-re nil t)
            (when-let ((uid-str (agenda-match 'uid)))
              (push (substring (car uid-str) 1 -1) uids)))
          ;; Data lines
          (goto-char (point-min))
          (while (re-search-forward agenda-data-re nil t)
            (when-let ((uid-str (agenda-match 'data-uid)))
              (push (car uid-str) uids))))
        (let ((result (delete-dups uids)))
          (setq agenda--uids-cache (cons current-tick result))
          result)))))

(defvar-local agenda--slots-cache nil
  "Cache storage for slots as (TICK . ((DATE . SLOTS) ...)).")

(defun agenda-collect-slots (date &optional force)
  "Compute 30-minute slots for a given calendar DATE (month day year).
If FORCE is non-nil, ignore the cache and re-scan."
  (let ((current-tick (buffer-chars-modified-tick (agenda-source-buffer))))
    (unless (and (not force)
                 agenda--slots-cache 
                 (eq (car agenda--slots-cache) current-tick))
      (setq agenda--slots-cache (cons current-tick nil)))
    (let* ((date-cache (cdr agenda--slots-cache))
           (cached-entry (assoc date date-cache)))
      (if cached-entry
          (cdr cached-entry)
        (let* ((date-iso (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date)))
               (entries (agenda-filter-entries
                         (lambda (entry date)
                           (and (not (agenda-entry-cancel-p entry))
                                (agenda-entry-date-p entry date)))
                         date-iso))
               (slots (make-list 48 nil)))
          (dolist (entry entries)
            (let* ((start (cdr (assoc 'time-start entry)))
                   (start-h (nth 0 start))
                   (start-m (nth 1 start))
                   (end (cdr (assoc 'time-end entry)))
                   (end-h (cond (end   (nth 0 end))
                                (start start-h)))
                   (end-m (cond (end   (nth 1 end))
                                (start (+ start-m 30)))))
              (when (and start-h start-m end-h end-m)
                (let* ((slot-start (/ (+ (* start-h 60) start-m) 30))
                       (slot-end   (/ (+ (* end-h 60) end-m) 30)))
                  (dotimes (i 48)
                    (when (and (>= i slot-start) (< i slot-end))
                      (push entry (nth i slots))))))))
          (setcdr agenda--slots-cache 
                  (cons (cons date slots) date-cache))
          slots)))))

(provide 'agenda-parse)
;;; agenda-parse.el ends here
