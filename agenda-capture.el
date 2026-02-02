;;; agenda-capture.el --- Capture framework -*- lexical-binding: t; -*-

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
(defvar agenda-quick-capture-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "S-<up>")    #'agenda-shift-up)
    (define-key map (kbd "S-<down>")  #'agenda-shift-down)
    (define-key map (kbd "S-<left>")  #'agenda-shift-left)
    (define-key map (kbd "S-<right>") #'agenda-shift-right)
    map)
"Keymap for capture in minibuffer.")

;;;###autoload
(defun agenda-capture-with-calendar (&optional with-data)
  "Open a calendar to choose a date then pre-fill an entry with selected date.
If WITH-DATA is non-nil (C-u), also prompt for additional data.

Use arrow keys to shift year/month/day/hour/minute based on 
cursor position in the minibuffer."
  (interactive "P")
  (agenda-with-calendar-select date
    (agenda-capture date with-data)))

;;;###autoload
(defun agenda-capture (&optional date with-data)
  "Prompt for an entry pre-filled with DATE (month day year) or current
date and current time. If WITH-DATA is non-nil (C-u), also prompt for
additional data.

Use arrow keys to shift year/month/day/hour/minute based on 
cursor position in the minibuffer."
  (interactive "P")
  (let* ((timestamp (if date
                        (encode-time (list 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
                      (current-time)))
         (hour (string-to-number (format-time-string "%H")))
         (initial (concat (format-time-string "%Y-%m-%d w%V %a" timestamp)
                          "  " (format "%02d:00" hour)
                          "-"  (format "%02d:00  " (min (1+ hour) 24))))
         (minibuffer-prompt-properties '(cursor-intangible t read-only t))
         (prompt (concat
                  (propertize "Agenda capture"
                              'face '(:inherit (link bold)))
                  (propertize "\n" 'face 'default
                                   'display (format "\n%s " (agenda-mark 'capture)))))
         
         (entry (read-from-minibuffer prompt initial agenda-quick-capture-map)))
    (unless (and agenda-file (file-exists-p agenda-file))
      (error "Agenda file not found: %s. " agenda-file))
    (if (string-match agenda-entry-re entry)
        (let* ((year  (string-to-number (car (agenda-match 'date-y entry))))
               (month (string-to-number (car (agenda-match 'date-m entry))))
               (day   (string-to-number (car (agenda-match 'date-d entry))))
               (source-buffer (find-file-noselect agenda-file)))
          (with-current-buffer source-buffer
            (agenda--goto-date (list month day year))
            (end-of-line)
            (unless (= (line-beginning-position) (line-end-position))
              (insert "\n"))
              (insert entry)
              (when with-data
                (let ((current-prefix-arg nil))
                  (call-interactively #'agenda-entry-set-data)))
              (message (concat (propertize "Entry captured " 'face 'bold)
                               (format "(%s)" entry)))
              (agenda-highlight (line-beginning-position)
                                (line-end-position))))
      (message "Entry format not recognized!"))))

(provide 'agenda-capture)
;;; agenda-capture.el ends here
