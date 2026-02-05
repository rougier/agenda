;;; agenda-view-week.el --- Weekly view  -*- lexical-binding: t; -*-

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

(defun agenda-view-week-extent (&optional week year)
  "Return timestamp for first & last days of WEEK YEAR."
  (let* ((year (or year (string-to-number (format-time-string "%Y"))))
         (year (or year (string-to-number (format-time-string "%V"))))
         (abs-mon (calendar-iso-to-absolute (list week 1 year)))
         (mon-greg (calendar-gregorian-from-absolute abs-mon))
         (sun-greg (calendar-gregorian-from-absolute (+ abs-mon 6))))
    (cons 
     (encode-time 0 0 0 (nth 1 mon-greg) (nth 0 mon-greg) (nth 2 mon-greg))
     (encode-time 0 0 0 (nth 1 sun-greg) (nth 0 sun-greg) (nth 2 sun-greg)))))

(defun agenda-view-week--display-title (week year)
  "Return WEEK / YEAR string."
  (let* ((week-extent (agenda-view-week-extent week year))
         (week-start (format-time-string "%d %B %Y" (car week-extent)))
         (week-end (format-time-string "%d %B %Y" (cdr week-extent))))
     (propertize (format "Year %s, week %02d\n" year week) 'face 'agenda-view-title)))

(defun agenda-view-week--display-subtitle (week year)
  "Return WEEK / YEAR extend string."
  (let* ((week-extent (agenda-view-week-extent week year))
         (week-start (format-time-string "%d %B %Y" (car week-extent)))
         (week-end (format-time-string "%d %B %Y" (cdr week-extent))))
    (propertize (format "%s - %s\n" week-start week-end) 'face 'agenda-view-subtitle)))

(defun agenda-view-week--display-date (date)
  "Return DATE string for day header."
  (let* ((today-iso (format-time-string "%Y-%m-%d"))
         (date-ts (encode-time (list 0 0 0 (nth 2 date) (nth 1 date) (nth 0 date))))
         (date-iso (format-time-string "%Y-%m-%d" date-ts))
         (date-str (format-time-string "%A %d %B %Y" date-ts)))
    (concat (propertize date-str
                        'agenda-hl-range t
                        'face 'agenda-view-date
                        )
            (if (string= date-iso today-iso) 
                (propertize " (today)\n\n" 'face 'agenda-view-date-today)
              "\n\n"))))

(defun agenda-view-week--display-entry (entry &optional time-str)
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
                          'agenda-view-time-today)))
        (time-str (if time-str
                      (format " %11s " time-str)
                    (propertize
                     (cond ((and time-start-hm time-end-hm)
                            (format " %s-%s " time-start-hm time-end-hm))
                           (time-start-hm
                            (format " %s       " time-start-hm))
                           (t
                            " ----------- "))
                     'agenda-hl-range t
                     'face (or time-face line-face)))))
    (propertize
     (concat  "  " time-str
              " "  title
              (when actions
                (concat " " (propertize actions 'face (or line-face 'agenda-edit-action))))
              (when tags
                (concat " " (propertize tags 'face (or line-face 'agenda-edit-tag))))
              "\n")
     'agenda-entry-marker marker)))

(defun agenda-view-week-display (&optional date force)
  "Display the agenda for DATE.
When FORCE is t, force update"
  (interactive)
  (let* ((date (or date agenda-view-date))
         (month (nth 0 date))
         (day (nth 1 date))
         (year (nth 2 date))
         (week (car (calendar-iso-from-absolute
                     (calendar-absolute-from-gregorian date))))
         (today (format-time-string "%Y-%m-%d"))
         (now (format-time-string "%H:%M"))
         (point-first nil)
         (point-date nil)
         (buffer (get-buffer-create agenda-view-buffer))
         
         ;; Entries (filtered by week/year)
         (entries (agenda-filter-entries 
                   (lambda (entry week year)
                     (and (not (agenda-entry-cancel-p entry))
                          (agenda-entry-inweek-p entry week year)))
                   week year))
    
         ;; Birthdays (filtered by week and post-processed)
         (birthdays (agenda-filter-entries 
                     (lambda (entry week year)
                       (and (not (agenda-entry-cancel-p entry))
                            (agenda-entry-birthday-p entry week)))
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
    
    (insert (agenda-view-week--display-title week year)
            (agenda-view-week--display-subtitle week year))

    ;; Entries and birthdays
    (let ((current-date nil))
      (dolist (entry entries)
        (let* ((entry-date (cdr (assoc 'date entry)))
               (cal-date (list (nth 1 entry-date) (nth 2 entry-date) (nth 0 entry-date))))
          (unless (equal entry-date current-date)
            (insert "\n" (agenda-view-week--display-date entry-date))
            (setq current-date entry-date))
          (when (and (not point-date) (equal cal-date date))
            (setq point-date (line-beginning-position)))
          (unless point-first
            (setq point-first (line-beginning-position)))
          (insert (propertize (agenda-view-week--display-entry entry)
                              'agenda-date-marker cal-date)))))

    ;; Future deadlines
    (when (> (length deadlines) 0)
      (insert (propertize "\nUpcoming deadlines\n\n" 'face 'agenda-view-date))
      (dolist (entry deadlines)
        (let* ((date (cdr (assoc 'date entry)))
               (time (encode-time (list 0 0 0 (nth 2 date) (nth 1 date) (nth 0 date))))
               (days (- (time-to-days time)
                        (time-to-days (current-time))))
               (face (agenda-view--deadline-face days))
               (time-str (propertize (format " In %2d days " days)
                                     'agenda-hl-range t
                                     'face face)))
          (when (<= days 30)
            (insert (propertize (agenda-view-week--display-entry entry time-str)
                                'agenda-date-marker date))))))
    (goto-char (or point-date point-first (point-min)))))

(provide 'agenda-view-week)
;;; agenda-view-week.el ends here
