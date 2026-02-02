;;; agenda-utils.el --- Utility functions -*- lexical-binding: t; -*-

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

;; This module provides various utility functions.

;;; Code:

(defvar agenda--last-selected-date nil
  "Stores the last selected date (month day year).")

(defmacro agenda-with-calendar-select (var &rest body)
  "Open calendar and bind the selected date to VAR, then execute BODY.
When a date is selected, it is saved for next call."
  (declare (indent 1))
  (let ((callback-sym (make-symbol "callback")))
    `(let ((,callback-sym (lambda (,var) ,@body)))
       (calendar)
       (when agenda--last-selected-date
         (calendar-goto-date agenda--last-selected-date))
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map calendar-mode-map)
         (define-key map (kbd "RET")
           `(lambda ()
              (interactive)
              (let ((selected-date (calendar-cursor-to-date)))
                (setq agenda--last-selected-date selected-date)
                (calendar-exit)
                (funcall ,,callback-sym selected-date))))         
         (define-key map (kbd "C-g")
           (lambda ()
             (interactive)
             (calendar-exit)))
         (use-local-map map)))))

(defun agenda-date-forward (date &optional n unit)
  "Forward calendar DATE (month day year) by N UNIT. 
Supports negative N for moving backward."
  (let* ((n (or n 1))
         (unit (or unit 'day))
         (month (nth 0 date))
         (day (nth 1 date))
         (year (nth 2 date)))
    (pcase unit
      ((or 'day 'week)
       (let ((delta (if (eq unit 'week) (* n 7) n)))
         (calendar-gregorian-from-absolute
          (+ (calendar-absolute-from-gregorian date) delta))))
      ((or 'month 'month-3)
       (let* ((total-months (+ month -1 (if (eq unit 'month-3) (* n 3) n)))
              (new-month (+ (mod total-months 12) 1))
              (new-year (+ year (floor total-months 12)))
              (last-day (calendar-last-day-of-month new-month new-year))
              (new-day (min day last-day)))
         (list new-month new-day new-year)))
      ('year
       (let* ((new-year (+ year n))
              (last-day (calendar-last-day-of-month month new-year))
              (new-day (min day last-day)))
         (list month new-day new-year)))      
      (_ (error "Invalid unit: %s" unit)))))

(defun agenda-url-button-string (text url)
  "Return TEXT as a clickable string that opens URL."
  (let ((map (make-sparse-keymap))
        (callback (lambda (&optional _ignore) 
                    (interactive) 
                    (browse-url url))))
    (define-key map [mouse-1] callback)
    (define-key map [mouse-2] callback)
    (define-key map (kbd "RET") callback)
    (propertize text
                'face 'agenda-view-button
                'font-lock-face 'agenda-view-button
                'mouse-face 'highlight
                'help-echo (concat "Go to "
                                   (propertize url 'face 'agenda-view-button))
                'follow-link t
                'keymap map)))

(provide 'agenda-utils)
;;; agenda-utils.el ends here
