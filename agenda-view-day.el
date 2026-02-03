;;; agenda-view-day.el --- Daily view  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/agenda

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
(require 'cal-iso)
(require 'agenda-parse)
(require 'agenda-faces)


(defun agenda-view-day--display-title (date)
  "Return DATE string."
  (let* ((ts (encode-time (list 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))))
    (propertize (format-time-string "%A, %d %B %Y" ts)
                'face 'agenda-view-title)))

(defun agenda-view-day--display-date (date)
  "Return DATE string for day header."
  (let* ((today-iso (format-time-string "%Y-%m-%d"))
         (date-ts (encode-time (list 0 0 0 (nth 2 date) (nth 1 date) (nth 0 date))))
         (date-iso (format-time-string "%Y-%m-%d" date-ts))
         (date-str (format-time-string "%A %d %B %Y" date-ts)))
    (concat (propertize date-str 'face 'agenda-view-date)
            (if (string= date-iso today-iso) 
                (propertize " (today)\n\n" 'face 'agenda-view-date-today)
              "\n\n"))))

(defun agenda-view-day--display-entry (entry &optional time-str)
  "Return ENTRY string, optionally with time replaced by TIME-STR."
  (let* ((today-iso (format-time-string "%Y-%m-%d"))
         (date-iso (agenda-entry-date-string entry "%Y-%m-%d"))
         (time-start-hm (agenda-entry-time-start-string entry))
         (time-end-hm (agenda-entry-time-end-string entry))
         (data (cdr (assoc 'data entry)))
         (url (cdr (assoc 'url data)))
         (title (cdr (assoc 'title entry)))
         (title (if url
                    (agenda-url-button-string title url)
                  title))
        (actions (agenda-entry-actions-string entry))
        (tags (agenda-entry-tags-string entry))                       
        (marker (cdr (assoc 'marker entry)))
        (line-face (if (agenda-entry-cancel-p entry)
                       'agenda-view-cancel
                     nil))
        (time-face (cond ((agenda-entry-now-p entry)
                          'agenda-view-time-now)
                         ((agenda-entry-today-p entry)
                          'agenda-view-time-today)
                         (t 'default)))
        (time-str (if time-str
                      (format " %11s " time-str)
                    (propertize
                     (cond ((and time-start-hm time-end-hm)
                            (format " %s-%s " time-start-hm time-end-hm))
                           (time-start-hm
                            (format " %s       " time-start-hm))
                           (t
                            " ----------- "))
                     'face (or time-face line-face)))))
    (propertize
     (concat  " " time-str
              " " title
              (when actions
                (concat " " (propertize actions 'face (or line-face 'agenda-edit-action))))
              (when tags
                (concat " " (propertize tags 'face (or line-face 'agenda-edit-tag))))
              "")
     'agenda-entry-marker marker)))

(defun agenda-view-day-display (&optional date force)
  "Display the agenda for DATE. When FORCE is t, force update."
  (let* ((date (or date agenda-view-date))
         (year (or (nth 2 date)
                   (string-to-number (format-time-string "%Y"))))
         (month (or (nth 0 date)
                     (string-to-number (format-time-string "%m"))))
         (day (or (nth 1 date)
                  (string-to-number (format-time-string "%d"))))
         (date (list month day year))
         (date-str (format "%4d-%02d-%02d" year month day))
         (ts (encode-time (list 0 0 0 day month year)))
         (week (string-to-number (format-time-string "%V" ts)))
         (today (format-time-string "%Y-%m-%d"))
         (now (format-time-string "%H:%M"))
         
         ;; Entries (filtered by week/year)
         (entries (agenda-filter-entries 
                   (lambda (entry date)
                     (and (not (agenda-entry-cancel-p entry))
                          (agenda-entry-date-p entry date)))
                   date-str))
    
         ;; Birthdays (filtered by week and post-processed)
         (birthdays (agenda-filter-entries 
                     (lambda (entry week year)
                       (and (not (agenda-entry-cancel-p entry))
                            (agenda-entry-birthday-p entry week (nth 1 date))))
                     week year))
         (birthdays (agenda-view--process-birthdays birthdays year))

         ;; Deadlines (future only)
         (deadlines (agenda-filter-entries 
                     (lambda (entry week year)
                       (and (not (agenda-entry-cancel-p entry))
                            (not (agenda-entry-done-p entry))
                            (agenda-entry-deadline-p entry)))
                     week year))

         (entries (sort (append entries birthdays) #'agenda-entry-less-p)))
    
        ;; Calendar
        (save-excursion
          (insert (agenda-view-calendar date t)))
        (agenda-view--next-line 1 24)
        (insert "  " (agenda-view-day--display-title date))
        (agenda-view--next-line 2 24)
        
        ;; Entries and birthdays
        (if entries
            (dolist (entry entries)
              (let ((date (cdr (assoc 'date entry))))
                (insert (agenda-view-day--display-entry entry))
                (agenda-view--next-line 1 24)))
          (progn
            (insert (propertize "  No event found." 'face 'shadow))
            (agenda-view--next-line 1 24)))

        ;; Future deadlines
        (when (> (length deadlines) 0)
          (agenda-view--next-line 1 24)
          (insert (propertize "  Upcoming deadlines" 'face 'agenda-view-date))
          (agenda-view--next-line 2 24)
          (dolist (entry deadlines)
            (let* ((date (cdr (assoc 'date entry)))
                   (time (encode-time (list 0 0 0 (nth 2 date) (nth 1 date) (nth 0 date))))
                   (days (- (time-to-days time)
                            (time-to-days (current-time))))
                   (face (agenda-view--deadline-face days))
                   (time-str (propertize (format "In %2d days " days)
                                         'face face)))
              (when (<= days 21)
                (insert (agenda-view-day--display-entry entry time-str))
                (agenda-view--next-line 1 24)))))
        
        ;; Move point to relevant place on mini-calendar
        (goto-char (point-min))
        (when-let ((prop (text-property-search-forward 'agenda-date-marker date #'equal)))
          (goto-char (prop-match-beginning prop)))))
  
(provide 'agenda-view-day)
;;; agenda-view-day.el ends here
