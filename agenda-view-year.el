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

(defun agenda-view-year-display (&optional date force)
  "Display a 3x4 grid of months for YEAR.
Each month is assumed to be exactly 24 columns wide and 10 rows high."
  (let* ((date (or date agenda-view-date))
         (year (nth 2 date))
         (month (nth 0 date))
         (day  (nth 1 date)))
    (dotimes (row 3)
      (let* ((m (+ 1 (* row 4)))
             (m1 (+ m 0))
             (c1 (split-string
                  (agenda-view-calendar (list m1 day year) (eq m1 month) force) "\n" t))
             (m2 (+ m 1))
             (c2 (split-string
                  (agenda-view-calendar (list m2 day year) (eq m2 month) force) "\n" t))
             (m3 (+ m 2)) 
             (c3 (split-string
                  (agenda-view-calendar (list m3 day year) (eq m3 month) force) "\n" t))
             (m4 (+ m 3))
             (c4 (split-string
                  (agenda-view-calendar (list m4 day year) (eq m4 month) force) "\n" t)))
          (dotimes (line 10)
            (insert (nth line c1) " " 
                    (nth line c2) " " 
                    (nth line c3) " "
                    (nth line c4) "\n"))
          (insert "")))

    (goto-char (point-min))
    (when-let ((prop (text-property-search-forward 'agenda-date-marker date #'equal)))
      (goto-char (prop-match-beginning prop)))))

(provide 'agenda-view-year)
;;; agenda-view-year.el ends here
