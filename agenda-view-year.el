;;; agenda-view-year.el --- Yearly view  -*- lexical-binding: t; -*-

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

(defvar agenda-view-year--layout '(3 . 4)
  "Year view calendar layout as (ROWS . COLS).")

(defun agenda-view-year-display (&optional date force layout)
  "Display a grid of months for YEAR based on LAYOUT (rows . cols).
Default LAYOUT is (3 . 4). Each month is 24 columns wide and 10 rows high."
  (let* ((date (or date agenda-view-date))
         (year (nth 2 date))
         (month (nth 0 date))
         (day  (nth 1 date))
         (layout (or layout '(3 . 4)))
         (rows (car layout))
         (cols (cdr layout)))
    (setq agenda-view-year--layout layout)
    (dotimes (row rows)
      (let ((months nil))
        (dotimes (col cols)
          (let* ((month-index  (+ 1 (* row cols) col))
                 (month-string (agenda-view-calendar (list month-index day year) 
                                                    (eq month-index month) 
                                                    force)))
            (push (split-string month-string "\n" t) months)))
        (setq months (nreverse months))
        (dotimes (line 10)
          (dolist (month-lines months)
            (insert (nth line month-lines) " "))
          (insert "\n"))))
    (goto-char (point-min))
    (when-let ((prop (text-property-search-forward 'agenda-date-marker date #'equal)))
      (goto-char (prop-match-beginning prop)))))

(defun agenda-view-year-display-auto (&optional date force)
  "Calculate the best (rows . cols) layout based on window width."
  (let* ((width (window-width))
         ;; Each month is 24 chars + 1 char space = 25
         (max-cols (max 1 (/ width 25)))
         (cols (cond ((>= max-cols 12) 12)
                     ((>= max-cols 6)  6)
                     ((>= max-cols 4)  4)
                     ((>= max-cols 3)  3)
                     ((>= max-cols 2)  2)
                     (t 1)))
         (rows (/ 12 cols))
         (layout (cons rows cols)))
    (agenda-view-year-display date force layout)))

(provide 'agenda-view-year)
;;; agenda-view-year.el ends here
