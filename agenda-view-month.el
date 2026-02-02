;;; agenda-view-month.el --- Monthly view -*- lexical-binding: t; -*-

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


;;; Code:
(require 'calendar)
(require 'agenda-parse)
(require 'agenda-faces)


(defun agenda-view-month--display-week (date &optional no-newline show-week)
  "Return DATE literal string, prefixed by a newline for Mondays, unless
NO-NEWLINE is t. Week number is shown for Mondays or when SHOW-WEEK it t."
  (let* ((ts (encode-time (list 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
         (weekday (nth 6 (decode-time ts)))
         (is-monday (eq weekday 1))
         (is-weekend (memq weekday '(0 6)))
         (line-prefix (if (and is-monday (not no-newline)) "\n" ""))
         (week-prefix (format-time-string "Week %V, %a, %d %b %Y  " ts))
         (date-prefix (format-time-string "         %a, %d %b %Y  " ts))
         (week-prefix (format-time-string "%a %d " ts))
         (date-prefix (format-time-string "%a %d " ts))
         (face (cond (is-weekend 'shadow)
                     (is-monday  'bold)
                     (t          'shadow))))
    (propertize
     (concat line-prefix
             (if (or is-monday show-week)
                 week-prefix
               date-prefix))
     'font-lock-face face 'face face)))

(defun agenda-view-month--display-entry (entry)
  "Return a display string for ENTRY."
  (let ((time (agenda-entry-time-string entry))
        (title (agenda-entry-title-string entry))
        (actions (agenda-entry-actions-string entry))
        (tags (agenda-entry-tags-string entry)))
    (concat (when time
              (concat (propertize time 'face 'bold) " "))
            title " " actions " " tags)))

(defun agenda-view-month--slot-face (entries)
  "Return face for entries (=slot)."
  (let* ((entry (car entries))
         (inweek (when entry (agenda-entry-inweek-p entry)))
         (face (cond (inweek  'agenda-slot-busy-inweek)
                     (entries 'agenda-slot-busy)
                     (t       'agenda-slot-free))))
    (catch 'found
      (dolist (slot agenda-slot-faces)
        (let* ((slot-key (car slot))
               (slot-face (cdr slot)))
          (dolist (entry entries)
            (let ((tags (agenda-entry-tags-string entry)))
              (when (and (stringp tags) (string-match-p slot-key tags))
                (setq face slot-face)
                (throw 'found face)))))))
    face))

(defun agenda-view-month--display-slot (entries &optional date)
  "Return a one char string for SLOT."
  (let* ((first (car entries))
         (count (length entries))
         (is-deadline (seq-some #'agenda-entry-deadline-p entries))
         (marker (when first (cdr (assoc 'marker first))))
         (help (when entries
                 (mapconcat #'agenda-view-month--display-entry entries "\n")))
         (face (agenda-view-month--slot-face entries))
         (char (cond (is-deadline (agenda-mark 'deadline))
                     ((> count 1) (agenda-mark 'conflict))
                     (t           " "))))
    (propertize char
                'face face
                'agenda-date-marker date
                'agenda-entry-marker marker
                'help-echo help)))

(defun agenda-view-month--display-summary (slots)
  "Given a list of slots, return a single line summary of entries."

  (let ((summaries)
        (entries (nreverse (seq-uniq (seq-mapcat #'identity slots)))))
    (dolist (entry entries)
      (let* ((time (cdr (assoc 'time-start entry)))
             (title (cdr (assoc 'title entry)))
             (inweek (agenda-entry-inweek-p entry))
             (now (agenda-entry-now-p entry))
             (title-face (if inweek 'default 'shadow))
             (time-face  (cond (now    '(:inverse-video t :inherit bold))
                               (inweek 'bold)                               
                               (t '(bold shadow))))
             (data (cdr (assoc 'data entry)))
             (url (cdr (assoc 'url data)))
             (title (if (and inweek url)
                        (agenda-url-button-string title url)
                      (propertize title 'face title-face)))
             (marker (cdr (assoc 'marker entry)))
             (summary (if time
                          (concat
                           (propertize (format (if now " %02d:%02d " "%02d:%02d")
                                               (nth 0 time) (nth 1 time))
                                       'face time-face)
                           " "
                           title)
                        (format "%s" title)))
             (summary (propertize summary 'agenda-entry-marker marker)))
        (push summary summaries)))
    (mapconcat #'identity summaries (propertize " " 'face 'shadow))))

(defun agenda-view-month--display-slots (date &optional hour-start hour-end with-summary)
  "Return slots string for calendar DATE (month day year) between
HOUR-START and HOUR-END. When WITH-SUMMRAY is t, append a summary with all entries."
  (let* ((slots (agenda-collect-slots date))
         (slots-str "")
         (hour-start (or hour-start 8))
         (hour-start (min (max hour-start 0) 23))
         (hour-end (or hour-end 18))
         (hour-end (min (max (1+ hour-start) hour-end) 24))
         (index-start (* hour-start 2))
         (index-max (* hour-end 2))
         (index index-start))
    (while (<= index index-max)
      (setq slots-str (concat slots-str
                             (agenda-view-month--display-slot (nth index slots) date)
                             (agenda-view-month--display-slot (nth (1+ index) slots) date)))
      (setq index (+ index 2)))
    (concat slots-str
            (when with-summary
              (concat "  " (agenda-view-month--display-summary slots))))))

(defun agenda-view-month--display-clock (&optional hour-start hour-end)
  "Return time string for calendar DATE (month day year) between
HOUR-START and HOUR-END."
  (let* ((time "")
         (hour-start (or hour-start 8))
         (hour-end (or hour-end 18))
         (hour-start (min (max hour-start 0) 23))
         (hour-end (min (max (1+ hour-start) hour-end) 24))
         (index-start (* hour-start 2))
         (index-max (* hour-end 2))
         (index index-start))
    (while (<= index index-max)
      (setq time (concat time
                         (if (eq (% (- index index-start) 4) 0)
                             (propertize (concat (format "%02d" (/ index 2)))
                                         'face 'shadow)
                           "  ")))
      (setq index (+ index 2)))
    time))

(defun agenda-view-month--display (month year column &optional hour-start hour-end with-summary)
  ""
  (let* ((year (or year (string-to-number (format-time-string "%Y"))))
         (month (or month (string-to-number (format-time-string "%m"))))
         (today (calendar-current-date))
         (day (list month 1 year))
         (day (calendar-absolute-from-gregorian day))
         (day-last (list month (calendar-last-day-of-month month year) year))
         (day-last (calendar-absolute-from-gregorian day-last))
         (dates (let ((dates))
                  (while (<= day day-last)
                    (push (calendar-gregorian-from-absolute day) dates)
                    (setq day (1+ day)))
                  (nreverse dates))))
    (dolist (date dates)
      (let* ((ts (encode-time (list 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
             (weekday (nth 6 (decode-time ts)))
             (is-today (equal date today))
             (is-inweek (string= (format-time-string "%G-%V" ts)
                                 (format-time-string "%G-%V")))              
             (is-monday (eq weekday 1))
             (is-weekend (memq weekday '(0 6)))
             (is-first (eq date (car dates))))

        ;; Month header
        (when is-first
          (agenda-view--next-line 1 column)
          (insert "        "
                  (propertize (format " %s " (format-time-string "%B %Y" ts))
                              'agena-hl-range t
                              'face 'bold)
                  "  ")
          (agenda-view--next-line 1 column))

        ;; Time header
        (when (or is-monday is-first)
          (agenda-view--next-line 1 column)
          (insert (propertize (format-time-string "Week %V " ts)
                              'agenda-hl-range t
                              'face (cond (is-inweek 'bold)
                                          (t         '(bold shadow))))
                  " "
                  (propertize (agenda-view-month--display-clock hour-start hour-end)
                              ;; 'agenda-hl-range t
                              'face (cond (is-inweek 'default)
                                          (t         'shadow)))
                  "  ")
          (agenda-view--next-line 1 column))

        ;; Day slots
        (insert (propertize (format-time-string " %a %d " ts)
                            'agenda-hl-range t
                            'agenda-date-marker date
                            'face (cond (is-today   '(:inverse-video t :inherit bold))
                                        (is-inweek  'default)
                                        ;; (is-monday  'shadow)
                                        ;; (is-weekend 'shadow)
                                        (t          'shadow)))
                " "
                (agenda-view-month--display-slots date hour-start hour-end with-summary)
                "  ")
        (agenda-view--next-line 1 column)))))

(defun agenda-view-month-1-display (&optional date force)
  "Display month agenda for DATE with summary next to each day.
When FORCE is t, force update."
  (interactive)
  (let* ((date (or date agenda-view-date))
         (month (nth 0 date))
         (year (nth 2 date))
         (hour-start 8)
         (hour-end  18)
         (month-col-size
          (+ 7                                ;; Date prefix
             (* (- hour-end hour-start -1) 2) ;; Slots
             5))                              ;; Spacing between months
         (month (or month
                   (car agenda-view-month-date)
                   (string-to-number (format-time-string "%m"))))
         (year (or year
                   (cdr agenda-view-month-date)
                   (string-to-number (format-time-string "%Y"))))
         (buffer (get-buffer-create agenda-view-buffer)))

    (save-excursion
      (agenda-view-month--display month year
                                  (* 0 month-col-size)
                                  hour-start hour-end t))
    (goto-char (point-min))
    (when-let ((prop (text-property-search-forward 'agenda-date-marker date #'equal)))
      (goto-char (prop-match-beginning prop)))))

(defun agenda-view-month-3-display (&optional date force)
  "Display 3 months agenda for DATE.
When FORCE is t, force update."
  (interactive)
  (let* ((date (or date agenda-view-date))
         (month (nth 0 date))
         (year (nth 2 date))
         (hour-start 8)
         (hour-end  18)
         (month-col-size
          (+ 7                                ;; Date prefix
             (* (- hour-end hour-start -1) 2) ;; Slots
             5)))                             ;; Spacing between months
    (dolist (index '(0 1 2))
      (save-excursion
        (agenda-view-month--display month year
                                    (* index month-col-size)
                                    hour-start hour-end)
        (calendar-increment-month month year 1)))
    (goto-char (point-min))
    (when-let ((prop (text-property-search-forward 'agenda-date-marker date #'equal)))
      (goto-char (prop-match-beginning prop)))))

                         
(provide 'agenda-view-month)
;;; agenda-view-month.el ends here
