;;; agenda-edit.el --- Agenda edit mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/agenda.txt
;; Parent-Package: agenda

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides interactive commands to modify the agenda text file.
;; It includes functions for:
;;   - Filtering entries
;;   - Modifying date and time
;;   - Creating repeated entries.
;;   - Updating entry metadata

;;; Code:
(require 'agenda-parse)
(require 'agenda-faces)

(defun agenda-highlight (beg end)
  "Move the highlight overlay to cover BEG to END."
  (unless agenda--highlight-overlay
    (setq agenda--highlight-overlay (make-overlay (point) (point)))
    (overlay-put agenda--highlight-overlay 'face 'agenda-edit-highlight))
  (move-overlay agenda--highlight-overlay beg end)
  (add-hook 'pre-command-hook #'agenda-highlight-clear nil t))

(defun agenda-highlight-clear ()
  "Clear highlight only if the last event was a keyboard event."
  (let ((event last-command-event))
    (unless (or (listp event)
                (memq event '(wheel-up wheel-down
                              mouse-wheel-up-event 
                              mouse-wheel-down-event)))
      (when agenda--highlight-overlay
        (move-overlay agenda--highlight-overlay (point) (point)))
      (remove-hook 'pre-command-hook #'agenda-highlight-clear t))))

(defun agenda-mark (what)
  "Return mark associated with WHAT"
  (cdr (assoc what agenda-marks)))

(defun agenda--set-mark (char &optional face)
  "Add CHAR in the left margin using FACE."
  (remove-overlays (line-beginning-position) (line-end-position))
  (let* ((start (line-beginning-position))
         (overlay (make-overlay start start nil nil t)))
    (overlay-put overlay 'agenda-mark t)
    (overlay-put overlay 'before-string 
                 (propertize " " 'display 
                             `((margin left-margin) 
                               ,(propertize (concat " " char)
                                            'face (or face 'default)))))))

(defun agenda-mark-entry-at-point ()
  "Set margin mark for entry at point."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (when-let ((entry (agenda-entry-at-point)))
        (cond ((agenda-entry-cancel-p entry)
               (agenda--set-mark (agenda-mark 'cancel) 'agenda-edit-cancel))
              ((agenda-entry-birthday-p entry)
               (agenda--set-mark (agenda-mark 'birthday)))
              ((agenda-entry-today-p entry)
               (agenda--set-mark (agenda-mark 'today)))
              ((agenda-entry-active-deadline-p entry)
               (agenda--set-mark (agenda-mark 'deadline) 'agenda-edit-deadline))
              ((agenda-entry-inweek-p entry)
               (agenda--set-mark (agenda-mark 'inweek)))
              (t
               (remove-overlays (line-beginning-position)
                                (line-end-position)
                                'agenda-mark t)))))))

(defun agenda--set-link (beg end marker &optional face)
  "Mark BEG to END with a clickable button that moves point to MARKER.
Button face can be specified with FACE."

  (let ((map (make-sparse-keymap))
        (callback (lambda (&rest _)
                     (interactive)
                     (push-mark) 
                     (goto-char (marker-position marker))
                     (recenter))))
    (define-key map [mouse-1] callback)
    (define-key map [mouse-2] callback)
    (define-key map (kbd "RET") callback)
    (with-silent-modifications
      (add-text-properties beg end
                           `(mouse-face highlight
                             face link
                             font-lock-face link
                             follow-link t
                             keymap ,map)))))

(defun agenda-link-data-at-point ()
  "Add link to date ref for entry at point."
  (save-excursion
    (save-match-data
      (when-let* ((ref (agenda-match 'ref))                 
                  (ref-beg (1+ (cadr ref)))
                  (ref-end (1- (cddr ref)))
                  (ref (substring (car ref) +1 -1))
                  (ref-marker (save-excursion
                                (goto-char ref-beg)
                                (point-marker))))
        (goto-char (point-min))
        (when (search-forward-regexp (format "^\\[\\(%s\\):.*\\]:.*$" ref))
          (let* ((data-beg (match-beginning 1))
                (data-end (match-end 1))
                (data-marker (save-excursion
                               (goto-char data-beg)
                               (point-marker))))
            (agenda--set-link ref-beg ref-end data-marker)
            (agenda--set-link data-beg data-end ref-marker)))))))

(defun agenda-parse-buffer ()
  "Parse buffer."
  (interactive)
  (save-excursion
    (remove-overlays (point-min) (point-max))
    (font-lock-flush)
    (font-lock-ensure)

    ;; Process entries
    (goto-char (point-min))
    (while (re-search-forward agenda-entry-re nil t)
      (let ((entry (agenda-entry-at-point)))
        ;; Mark entry
        (save-excursion
          (goto-char (cadr (agenda-match 'date)))
          (agenda-mark-entry-at-point))
        ;; Create button on data
        (save-excursion
          (goto-char (cadr (agenda-match 'date)))
          (agenda-link-data-at-point))))
  
    ;; Process headers
    (with-silent-modifications
      (goto-char (point-min))
      (while (re-search-forward agenda-header-re nil t)
        (let* ((start (match-beginning 0))
               (full-prefix (match-string 1))
               (prefix (format "%2s" (string-trim full-prefix))))
          (put-text-property start (+ start (length full-prefix))
                             'display `((margin left-margin)
                                        ,(propertize prefix
                                                     'face 'agenda-edit-header-prefix))))))))

(defun agenda-date-change-at-point (delta)
  "Change date at point by DELTA '(year month day).
  This methods highlights the modified date and update the margin marker."
  (save-excursion
    (when-let* ((entry (agenda-entry-at-point))
                (date-group (agenda-match 'date))
                (date-value (cdr (assoc 'date entry)))
                (date-beg (cadr date-group))
                (date-end (cddr date-group))
                (timestamp (encode-time 0 0 0 
                                          (+ (nth 2 date-value) (nth 2 delta))
                                          (+ (nth 1 date-value) (nth 1 delta))
                                          (+ (nth 0 date-value) (nth 0 delta))))
                  (date (format-time-string "%Y-%m-%d w%V %a" timestamp)))
        (goto-char (cadr date-group))
        (delete-region date-beg date-end)
        (insert date)
        (agenda-mark-entry-at-point)
        (agenda-highlight date-beg date-end))))

(defun agenda-time-change-at-point (delta)
  "Change time or time-range at point by DELTA '(hour minute).
  This methods highlights the modified date and update the margin marker."
  (save-excursion
    (when-let* ((point (point))
                (entry (agenda-entry-at-point))
                (time-group (agenda-match 'time))
                (time-start (cdr (assoc 'time-start entry)))
                (time-start-group (agenda-match 'time-start)))
      (let* ((time-end (cdr (assoc 'time-end entry)))
             (time-end-group (agenda-match 'time-end))
             (point-on-time-start (and time-start-group
                                       (>= point (cadr time-start-group))
                                       (<  point (cddr time-start-group))))
             (point-on-time-end (and time-end-group
                                     (>= point (cadr time-end-group))
                                     (<  point (cddr time-end-group))))
             (time-start-ts (encode-time 0 
                                         (+ (nth 1 time-start)
                                            (if point-on-time-start (nth 1 delta) 0))
                                         (+ (nth 0 time-start)
                                            (if point-on-time-start (nth 0 delta) 0))
                                         1 1 1970))
             (time-end-ts (encode-time 0
                                       (+ (nth 1 (or time-end time-start))
                                          (if (or point-on-time-start point-on-time-end)
                                              (nth 1 delta) 0))
                                       (+ (nth 0 (or time-end time-start))
                                          (if (or point-on-time-start point-on-time-end)
                                              (nth 0 delta) 0))
                                       1 1 1970))
             (time-str (cond (point-on-time-end
                              (format-time-string "%H:%M" time-end-ts))
                             ((and point-on-time-start time-end)
                              (format "%s-%s" 
                                      (format-time-string "%H:%M" time-start-ts)
                                      (format-time-string "%H:%M" time-end-ts)))
                             (point-on-time-start
                              (format-time-string "%H:%M" time-start-ts))))
             (time-region (cond (point-on-time-end
                                 (cdr time-end-group))
                                ((and point-on-time-start time-end)
                                 (cdr time-group))
                                (point-on-time-start
                                 (cdr time-start-group)))))          
        (when (and time-str time-region)
          (delete-region (car time-region) (cdr time-region))
          (goto-char (car time-region))
          (insert time-str)
          (agenda-mark-entry-at-point)
          (agenda-highlight (car time-region) (cdr time-region)))))))

(defmacro agenda--with-undoable-edit (&rest body)
  "Execute BODY as a buffer-modifying operation with undo and point safety.

  This macro ensures:

  1. The cursor (point) is preserved after BODY runs.
  2. All changes are recorded in an undo change group.
  3. If the user repeatedly invokes the same command, 
  all edits are merged into a single undo step.

  Intended for use with commands that repeatedly modify
  dates, times, or other buffer content in-place."
  `(let ((orig-point (point))
         (handle (if (eq last-command this-command)
                     agenda--last-handle
                   (prepare-change-group))))
     (setq agenda--last-handle handle)
     (unwind-protect
         (progn
           (activate-change-group handle)
           ,@body)
       (goto-char orig-point)
       (when (eq last-command this-command)
         (undo-amalgamate-change-group handle)))))

(defun agenda--shift-up ()
  "Increment date or time at point, based on the cursor position."
  (let ((groups (agenda-groups-at-point)))
    (cond
     ((memq 'date-y groups) (agenda-date-change-at-point '(+1 0 0)))
     ((memq 'date-m groups) (agenda-date-change-at-point '(0 +1 0)))
     ((memq 'date-d groups) (agenda-date-change-at-point '(0 0 +7)))     
     ((memq 'time-start-h groups) (agenda-time-change-at-point '(+1 0)))
     ((memq 'time-start-m groups) (agenda-time-change-at-point '(0 +5)))
     ((memq 'time-end-h   groups) (agenda-time-change-at-point '(+1 0)))
     ((memq 'time-end-m   groups) (agenda-time-change-at-point '(0 +5))))))

(defun agenda--shift-down ()
  "Decrement date or time at point, based on the cursor position."
  (let ((groups (agenda-groups-at-point)))
    (cond
     ((memq 'date-y groups) (agenda-date-change-at-point '(-1 0 0)))
     ((memq 'date-m groups) (agenda-date-change-at-point '(0 -1 0)))
     ((memq 'date-d groups) (agenda-date-change-at-point '(0 0 -7)))     
     ((memq 'time-start-h groups) (agenda-time-change-at-point '(-1 0)))
     ((memq 'time-start-m groups) (agenda-time-change-at-point '(0 -5)))
     ((memq 'time-end-h   groups) (agenda-time-change-at-point '(-1 0)))
     ((memq 'time-end-m   groups) (agenda-time-change-at-point '(0 -5))))))

(defun agenda--shift-right ()
  "Increment date or time at point by 1 day or 1 hour."
  (let ((groups (agenda-groups-at-point)))
    (cond
     ((memq 'date groups) (agenda-date-change-at-point '(0 0 +1)))
     ((memq 'time groups) (agenda-time-change-at-point '(+1 0))))))

(defun agenda-shift--left ()
  "Decrement date or time at point by 1 day or 1 hour."
  (let ((groups (agenda-groups-at-point)))
    (cond
     ((memq 'date groups) (agenda-date-change-at-point '(0 0 -1)))
     ((memq 'time groups) (agenda-time-change-at-point '(-1 0))))))

(defun agenda-shift-down ()
  "Decrement date or time at point, based on the cursor position."
  (interactive)
  (agenda--with-undoable-edit (agenda--shift-down)))

(defun agenda-shift-up ()
  "Increment date or time at point, based on the cursor position."
  (interactive)
  (agenda--with-undoable-edit (agenda--shift-up)))

(defun agenda-shift-right ()
  "Increment date or time at point by 1 day or 1 hour."
  (interactive)
  (agenda--with-undoable-edit (agenda--shift-right)))

(defun agenda-shift-left ()
  "Decrement date or time at point by 1 day or 1 hour."
  (interactive)
  (agenda--with-undoable-edit (agenda-shift--left)))

(defun agenda-filter (regexp &optional strip-comment)
  "Hide lines not matching REGEXP. Supports keywords like 'future'.
  With optional STRIP-COMMENT (prefix arg), comment lines are hidden."
    (interactive
     (let* ((keywords '("today" "this week" "this month" 
                        "tomorrow" "next week" "next month" 
                        "future" "marked"))
            (all (append keywords agenda-filter-history))
            (input (completing-read "Filter: " 
                                    all nil nil nil 'agenda-filter-history)))
       (list input current-prefix-arg)))

  (save-excursion
    (outline-show-all)
    (let* ((now (current-time))
           (keywords
            `(("today"      . ,(format-time-string "%Y-%m-%d" now))
              ("tomorrow"   . ,(format-time-string "%Y-%m-%d" (time-add now (days-to-time 1))))
              ("this week"  . ,(format-time-string "w%V" now))
              ("this month" . ,(format-time-string "%Y-%m" now))
              ("next week"  . ,(format-time-string "w%V" (time-add now (days-to-time 7))))
              ("future"     . "__agenda_future__")
              ("marked"     . "__agenda_mark__")))
           (actual-regexp (or (cdr (assoc (downcase (or regexp "")) keywords)) regexp)))

      (with-silent-modifications
        (remove-text-properties (point-min) (point-max) '(invisible nil))
        (if (string-blank-p actual-regexp)
            (progn
              (setq buffer-read-only nil)
              (outline-minor-mode 1))
          (progn
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((is-comment (looking-at-p "#"))
                     (matches
                      (cond 
                       ((string= actual-regexp "__agenda_mark__")
                        (agenda-line-marked-p))
                       ((string= actual-regexp "__agenda_future__")
                        (agenda-line-future-p))
                       (t
                        (agenda-line-match-p actual-regexp)))))
                (unless (or matches (and (not strip-comment) is-comment))
                  (put-text-property (line-beginning-position)
                                     (line-beginning-position 2)
                                     'invisible t))
                (forward-line 1)))            
            (setq buffer-read-only t)
            (outline-minor-mode -1)))))))

(defun agenda-cleanup ()
  "Reparse buffer and recenter."
  (interactive)
  (set-window-margins nil 3)
  (setq buffer-read-only nil)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(invisible nil)))
  (agenda-parse-buffer)
  (let ((agenda-edit-mode nil))
    (call-interactively
     (key-binding (this-command-keys)))))

(defun agenda-outline-cycle ()
  "Toggle visibility and show hidden count by hijacking the last character."
  (interactive)
  (when outline-minor-mode
    (condition-case nil
        (save-excursion
          (outline-back-to-heading)
          (let* ((line-end (line-end-position))
                 (subtree-end (save-excursion (outline-end-of-subtree) (point)))
                 (count 0))
            (remove-overlays (line-beginning-position) (1+ line-end) 'agenda-count t)
            (if (outline-invisible-p line-end)
                (progn 
                  (outline-show-entry)
                  (outline-show-children))
              (save-excursion
                (goto-char line-end)
                (while (re-search-forward agenda-entry-re subtree-end t)
                  (setq count (1+ count))))
              (outline-hide-subtree)
              (when (> count 0)
                (let* ((char-pos (1- line-end))
                       (char-at-pos (char-to-string (char-after char-pos)))
                       (ov (make-overlay char-pos line-end)))
                  (overlay-put ov 'display 
                               (concat char-at-pos 
                                       (propertize (format " (%d entries) " count) 
                                                   'face 'agenda-edit-header-summary)))
                  (overlay-put ov 'agenda-count t)
                  (overlay-put ov 'priority 1000))))))
      (error (indent-for-tab-command)))))

(defun agenda-outline-cycle-all ()
  "Toggle visibility for the whole buffer. 
  If any heading is hidden, show all. Otherwise, collapse to Level 1 and show counts."
  (interactive)
  (when outline-minor-mode
    (let ((any-folded nil))
      (save-excursion
        (goto-char (point-min))
        (while (and (not any-folded) 
                    (re-search-forward outline-regexp nil t))
          (when (outline-invisible-p (line-end-position))
            (setq any-folded t))))
      (if any-folded
          (progn
            (remove-overlays (point-min) (point-max) 'agenda-count t)
            (outline-show-all))
        (outline-hide-sublevels 1)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^# +.*$" nil t)
            (let* ((line-end (point))
                   (subtree-end (save-excursion (outline-end-of-subtree) (point)))
                   (count 0))
              (remove-overlays (line-beginning-position) (1+ line-end) 'agenda-count t)
              (save-excursion
                (while (re-search-forward agenda-entry-re subtree-end t)
                  (setq count (1+ count))))
              (when (> count 0)
                (let* ((char-pos (1- line-end))
                       (char-at-pos (char-to-string (char-after char-pos)))
                       (ov (make-overlay char-pos line-end)))
                  (overlay-put ov 'display 
                               (concat char-at-pos 
                                       (propertize (format " (%d entries) " count) 
                                                   'face 'agenda-edit-header-summary)))
                  (overlay-put ov 'agenda-count t)
                  (overlay-put ov 'priority 1000))))))))))

(defun agenda--goto-date (target &optional exact)
  "Go to the TARGET date (month day year). If EXACT is non-nil, only stops on a perfect match.
 Otherwise, stops on the closest date equal to or older than selected date."
  (let* ((target-time (encode-time 0 0 0 (nth 1 target) (nth 0 target) (nth 2 target)))
         (target-days (time-to-days target-time))
         (best-pos nil))
    (save-excursion
      (goto-char (point-max))
      (catch 'found
        (while (re-search-backward agenda-entry-re nil t)
          (let* ((entry-date-str (car (agenda-match 'date-iso)))
                 (entry-time-str (or (car (agenda-match 'time-start)) "00:00"))
                 (entry-days (time-to-days
                              (date-to-time (concat entry-date-str " " entry-time-str)))))
            (if exact
                (cond
                 ((= entry-days target-days)
                  (setq best-pos (match-beginning 0))
                  (throw 'found t))
                 
                 ((< entry-days target-days)
                  (setq best-pos nil)
                  (throw 'found nil)))
              (setq best-pos (match-beginning 0))
              (when (<= entry-days target-days)
                (throw 'found t)))))))
    (if best-pos
        (progn
          (goto-char best-pos)
          (beginning-of-line)))))

(defun agenda-goto-date (&optional exact)
  "Open the calendar and go to the selected date when RET is pressed.
If EXACT is non-nil, only stops on a perfect match.  Otherwise, stops on
the closest date equal to or older than selected date."
  (interactive (list current-prefix-arg))
   (agenda-with-calendar-select date
     (agenda--goto-date date)))

(defun agenda-insert-entry (&optional here)
  "Open the calendar and insert a new entry for the selected date.
If HERE is t,  insert entry at point, else, search for closest date."
  (interactive (list current-prefix-arg))
  (agenda-with-calendar-select date
    (unless here
      (agenda--goto-date date))
    (end-of-line)
    (unless (= (line-beginning-position) (line-end-position))
      (insert "\n"))
    (let* ((timestamp (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
           (hour (string-to-number (format-time-string "%H"))))
      (insert (concat (format-time-string "%Y-%m-%d w%V %a" timestamp)
                      "  " (format "%02d:00" hour) "-"
                      (format "%02d:00  " (min (1+ hour) 24)))))))

(defun agenda-repeat-entry (frequency count)
  "Read current entry and create COUNT copies incremented by FREQUENCY.
  FREQUENCY is a string like '+1w', '+3d', '+1m', or '+1y'."
  (interactive "sFrequency (e.g., +1w): \nnCount: ")
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at agenda-entry-re))
        (user-error "Not on a valid agenda entry")
      (let* ((year  (string-to-number (car (agenda-match 'date-y))))
             (month (string-to-number (car (agenda-match 'date-m))))
             (day   (string-to-number (car (agenda-match 'date-d))))
             (rest  (buffer-substring (cddr (agenda-match 'date))
                                      (line-end-position)))
             (val   (string-to-number (substring frequency 1 -1)))
             (unit  (substring frequency -1))
             (delta (pcase unit
                      ("d" (list val 0 0))
                      ("w" (list (* val 7) 0 0))
                      ("m" (list 0 val 0))
                      ("y" (list 0 0 val))
                      (_ (user-error "Unknown unit: %s" unit)))))        
        (end-of-line)
        (dotimes (i count)
          (let* ((multiplier (1+ i))
                 (new-time (encode-time 0 0 0 
                                        (+ day   (* multiplier (nth 0 delta)))
                                        (+ month (* multiplier (nth 1 delta)))
                                        (+ year  (* multiplier (nth 2 delta))))))
            (insert (format-time-string "\n%Y-%m-%d w%V %a" new-time)
                    rest)))))))

(defun agenda-collect-uids ()
  "Scan agenda buffer for all UIDs (entry & data lines)."
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
    (delete-dups uids)))

(defun agenda-entry-generate-uid (entry)
  "Generate a unique id for ENTRY.
Checks YYYYMMDDTHHMM, then YYYYMMDDT0001, T0002, etc."
  (let* ((uid-data (cdr (assoc 'uid (assoc 'data entry))))
         (used (delete uid-data (agenda-collect-uids)))
         (date (cdr (assoc 'date entry)))
         (time (or (cdr (assoc 'time-start entry)) '(0 1)))
         (uid-prefix (format "%04d%02d%02dT" (nth 0 date) (nth 1 date) (nth 2 date)))
         (uid-suffix (format "%02d%02d" (nth 0 time) (nth 1 time)))              
         (uid-auto (or uid-data (concat uid-prefix uid-suffix)))
         (uid-uniq (if (not (member uid-auto used))
                       uid-auto
                     (let ((count 1) (res nil))
                       (while (not res)
                         (let ((uid-temp (format "%s%04d" uid-prefix count)))
                           (if (not (member uid-temp used))
                               (setq res uid-temp)
                             (setq count (1+ count)))))
                       res)))
         (uid-user (read-string "UID: " uid-uniq)))    
    (while (member uid-user used)
      (setq uid-user (read-string (format "UID '%s' already used! New UID: " uid-user) 
                                  uid-uniq)))
    uid-user))

(defun agenda-entry-set-data (uid key value)
  "Update or create metadata for entry at point.
  If C-u is provided, prompts to confirm/modify the UID.
  Metadata is stored at the END of the buffer.

  [NOTE]: Co-written with Gemini."
  (interactive
   (let* ((entry (or (agenda-entry-at-point) (user-error "No entry at point")))
          (data-alist (cdr (assoc 'data entry)))
          ;; 1. Handle UID: Prompt ONLY if prefix argument (C-u) is active
          (final-uid (if current-prefix-arg
                         (agenda-entry-generate-uid entry)
                       (or (cdr (assoc 'uid data-alist))
                           ;; Silent generation if no UID exists
                           (let ((uid-data (cdr (assoc 'uid data-alist)))
                                 (used (agenda-collect-uids))
                                 (date (cdr (assoc 'date entry)))
                                 (time (or (cdr (assoc 'time-start entry)) '(0 1))))
                             (let* ((prefix (format "%04d%02d%02dT" (nth 0 date) (nth 1 date) (nth 2 date)))
                                    (suffix (format "%02d%02d" (nth 0 time) (nth 1 time)))
                                    (base (concat prefix suffix)))
                               (if (not (member base used)) base
                                 (let ((c 1) (res nil))
                                   (while (not res)
                                     (let ((cand (format "%s%04d" prefix c)))
                                       (if (not (member cand used)) (setq res cand)
                                         (setq c (1+ c)))))
                                   res)))))))
          ;; 2. Select Key
          (selected-key (completing-read "Key: " '("URL" "MAP" "NOTE") nil t))

          ;; 3. Extract existing value from data alist
          (key-sym (intern (downcase selected-key)))
          (current-val (or (cdr (assoc key-sym data-alist)) "")))
     (list final-uid 
           selected-key 
           (read-string (format "Value (%s): " selected-key) current-val))))

  (let ((entry-pos (copy-marker (line-beginning-position))))
    ;; 1. Sync the UID on the entry line
    (save-excursion
      (goto-char entry-pos)
      (let ((existing-uid (cdr (assoc 'uid (assoc 'data (agenda-entry-at-point))))))
        (unless (string= uid existing-uid)
          (when (re-search-forward " +\\[[^]]+\\]" (line-end-position) t)
            (replace-match ""))
          (goto-char (line-end-position))
          (insert (format " [%s]" uid)))))

    ;; 2. Update or delete the data line at the end of the buffer
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (let ((search-re (format "^\\[%s:%s\\]:" (regexp-quote uid) (regexp-quote key)))
            (found-pos nil))
        (save-excursion
          (when (re-search-backward search-re nil t)
            (setq found-pos (line-beginning-position))))
        (cond
         ((and found-pos (string-empty-p value))
          (goto-char found-pos)
          (delete-region (line-beginning-position) (progn (forward-line 1) (point))))
         ((and found-pos (not (string-empty-p value)))
          (goto-char found-pos)
          (delete-region (line-beginning-position) (line-end-position))
          (insert (format "[%s:%s]: %s" uid key value)))
         ((not (string-empty-p value))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "[%s:%s]: %s\n" uid key value))))))

    ;; 3. Cleanup Header
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward (format "^\\[%s:[[:alpha:]]+\\]:" (regexp-quote uid)) nil t)
        (goto-char entry-pos)
        (when (re-search-forward (format " +\\[%s\\]" (regexp-quote uid)) (line-end-position) t)
          (replace-match ""))))    
    (set-marker entry-pos nil)))

(defun agenda-insert-holidays (year &optional no-header)
  "Generate and insert holidays for YEAR.
With a prefix argument (C-u), skip the '# Holidays' header."
  (interactive 
   (list (read-number "Year: " (nth 5 (decode-time (current-time))))
         current-prefix-arg))
  (let ((results nil))
    (save-window-excursion
      (list-holidays year year))
    (with-current-buffer "*Holidays*"
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\([^:\n]+\\): +\\(.*\\)$" nil t)
          (let* ((date-raw (match-string 1))
                 (event-name (match-string 2))
                 (parsed (parse-time-string date-raw))
                 (day (nth 3 parsed))
                 (month (nth 4 parsed))
                 (item-year (or (nth 5 parsed) year)))
            (when (and day month)
              (let* ((time-val (encode-time 0 0 0 day month item-year))
                     (full-date (format-time-string "%Y-%m-%d w%V %a" time-val)))
                (push (format "%s  %s +HOLIDAYS\n" full-date event-name) results)))))))
    (when results
      (unless no-header
        (insert (format "## Holidays %d\n\n" year)))
      (dolist (line (reverse results))
        (insert line))
      (unless no-header (insert "\n")))))

(defvar agenda-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<right>") #'agenda-shift-right)
    (define-key map (kbd "S-<left>")  #'agenda-shift-left)
    (define-key map (kbd "S-<down>")  #'agenda-shift-down)
    (define-key map (kbd "S-<up>")    #'agenda-shift-up)
    (define-key map (kbd "C-c g")     #'agenda-goto-date)
    (define-key map (kbd "C-c i")     #'agenda-insert-entry)
    (define-key map (kbd "C-c r")     #'agenda-repeat-entry)
    (define-key map (kbd "C-c :")     #'agenda-entry-set-data)
    (define-key map (kbd "C-c f")     #'agenda-filter)
    (define-key map (kbd "C-l")       #'agenda-cleanup)
    (define-key map (kbd "TAB")       #'agenda-outline-cycle)
    (define-key map (kbd "<backtab>") #'agenda-outline-cycle-all)
    (define-key map (kbd "C-c a")     #'agenda-view)
    (define-key map (kbd "C-c d")     #'agenda-view-day)
    (define-key map (kbd "C-c w")     #'agenda-view-week)
    (define-key map (kbd "C-c m")     #'agenda-view-month-1)
    (define-key map (kbd "C-c t")     #'agenda-view-month-3)
    (define-key map (kbd "C-c y")     #'agenda-view-year)
    map)
  "Keymap for 'agenda-edit-mode'.")

(defconst agenda-font-lock-keywords
  `((,agenda-data-re
     (0 'agenda-edit-data)
     (,(agenda-group 'data-ref) '(agenda-edit-data-ref) prepend t))

    (,agenda-header-re
     (0  'agenda-edit-header))
    
    (,agenda-comment-re
     (0 'agenda-edit-comment))

    (,agenda-entry-re
     (0 (let* ((date (car (agenda-match 'date)))
               (time (car (agenda-match 'time)))
               (tags (car (agenda-match 'tags))))
          (setq-local is-today     (agenda-date-today-p date))
          (setq-local is-now       (agenda-date-time-now-p date time))
          (setq-local is-inweek    (agenda-date-inweek-p (match-string 1)))
          (setq-local is-cancel    (agenda-tags-cancel-p tags))
          (setq-local is-deadline  (and (agenda-tags-deadline-p tags)
                                        (not (agenda-tags-done-p tags))))
          (cond (is-cancel 'agenda-edit-cancel)
                ((and (not is-inweek) (not is-today)) 'agenda-edit-outweek)
                (t nil))))
     (,(agenda-group 'date)
      (cond (is-cancel                  'agenda-edit-cancel)
            (is-deadline                'agenda-edit-deadline)
            ((and is-today is-deadline) 'agenda-edit-deadline)
            (is-today                   'agenda-edit-today)
            (is-inweek                  'agenda-edit-inweek)
            (t                          'agenda-edit-outweek)) prepend)
     
     (,(agenda-group 'time) (cond (is-cancel 'agenda-edit-cancel)
                                  (is-today  'agenda-edit-today)
                                  (is-inweek 'agenda-edit-inweek)
                                  (t         'agenda-edit-outweek)) prepend t)
     
     (,(agenda-group 'title) (cond (is-cancel 'agenda-edit-cancel)
                                   (is-today  'agenda-edit-today)
                                   (is-inweek 'agenda-edit-inweek)
                                   (t         'agenda-edit-outweek)) prepend)
     
     (,(agenda-group 'actions) (cond (is-cancel 'agenda-edit-cancel)
                                     (is-today  '(agenda-edit-action
                                                  agenda-edit-today))
                                     (is-inweek 'agenda-edit-action)
                                     (t         'agenda-edit-outweek)) prepend t)
      
     (,(agenda-group 'tags) (cond (is-cancel 'agenda-edit-cancel)
                                  (is-today  '(agenda-edit-tag
                                               agenda-edit-today))
                                  (is-inweek 'agenda-edit-tag)
                                  (t         'agenda-edit-outweek)) prepend t)
     
     (,(agenda-group 'uid) (cond (is-cancel  'agenda-edit-cancel)
                                 (is-today   '(agenda-edit-reference
                                               agenda-edit-today))
                                 (is-inweek  'agenda-edit-reference)
                                 (t          'agenda-edit-outweek)) prepend t)))
  "Agenda font lock for edit mode.")

;;;###autoload
(define-minor-mode agenda-edit-mode
  "Minor mode for editing agenda files."
  :lighter "Agenda[E]"
  :keymap agenda-edit-mode-map
  (if agenda-edit-mode
      (progn
        (font-lock-add-keywords nil agenda-font-lock-keywords 'append)
        (font-lock-flush)
        (font-lock-ensure)
        (set-window-margins nil 3)
        (setq-local left-margin-width 3)
        (setq-local outline-regexp "^\\(#+\\) "
                    outline-level (lambda () (length (match-string 1)))
                    outline-heading-end-regexp "\n"
                    outline-end-of-heading-function nil)
        (outline-minor-mode 1)
        (agenda-parse-buffer))
    (progn
      (font-lock-remove-keywords nil agenda-font-lock-keywords)
      (set-window-margins nil 0)
      (setq-local left-margin-width 0)
      (agenda-highlight-clear)
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max) '(invisible nil))
        (remove-text-properties (point-min) (point-max) '(display nil))
        (remove-overlays (point-min) (point-max)))
      (font-lock-flush)
      (font-lock-ensure))))

(provide 'agenda-edit)
;;; agenda-edit.el ends here
