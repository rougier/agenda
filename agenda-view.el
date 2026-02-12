;;; agenda-view.el --- Agenda view -*- lexical-binding: t; -*-

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
(require 'agenda-parse)
(require 'agenda-faces)
(require 'agenda-utils)
(require 'agenda-view-day)
(require 'agenda-view-week)
(require 'agenda-view-month)
(require 'agenda-view-year)

(defvar agenda-view-date nil
  "Date currently displayed.")

(defvar agenda-view-unit 'day
  "Unit view (day, week, month, month-3 or year.")

(defun agenda-view--process-birthdays (entries year &optional month)
  "Return a new list of ENTRIES with updated date and title related tp YEAR
and MONTH."
  (mapcar (lambda (entry)
            (let* ((birth-date (cdr (assoc 'date entry)))
                   (birth-year (car birth-date))
                   (age (- year birth-year))
                   (new-date (list year (nth 1 birth-date) (nth 2 birth-date)))
                   (old-title (cdr (assoc 'title entry)))
                   (new-title (concat (agenda-mark 'birthday)
                                      " "
                                      (format old-title age)))
                   (new-entry (assq-delete-all 'date
                                  (assq-delete-all 'title
                                      (copy-sequence entry)))))
              (cons (cons 'date new-date)
                    (cons (cons 'title new-title)
                          new-entry)))) 
          entries))

(defun agenda-view--deadline-face (days)
  "Return face for deadline (=slot)."
    (let ((face 'default))
      (catch 'found
        (dolist (deadline agenda-deadline-faces)
          (let* ((deadline-days (car deadline))
                 (deadline-face (cdr deadline)))
            (unless (< deadline-days days)
              (setq face deadline-face)
              (throw 'found face)))))
    face))

(defun agenda-view--next-line (&optional n column)
  "Move point forward ? line at specified column."
  (dotimes (i (or n 1))
    (if (eobp) (newline) (forward-line 1)))
  (move-to-column (or column 24) t))

(defun agenda-view--display-summary (slots)
  "Given a list of slots, return a single line summary of entries."

  (let ((summaries)
        (entries (nreverse (seq-uniq (seq-mapcat #'identity slots)))))
    (dolist (entry entries)
      (let* ((time (cdr (assoc 'time-start entry)))
             (title (cdr (assoc 'title entry)))
             (data (cdr (assoc 'data entry)))
             (summary (if time
                          (concat
                           (propertize (format "%02d:%02d" (nth 0 time) (nth 1 time))
                                       'face 'bold)
                           " "
                           title)
                        title)))
        (push summary summaries)))
    (mapconcat #'identity summaries " ")))

(defvar agenda-view--calendar-cache (make-hash-table :test 'equal)
  "Cache for agenda calendar strings. Key is (month year active).")

(defun agenda-view-calendar (date &optional active force)
  "Generate a calendar representation for DATE (month day year).
Uses a cache based on month, year, and ACTIVE status unless FORCE is non-nil."
  (let* ((month (nth 0 date))
         (day (nth 1 date))
         (year (nth 2 date))
         (cache-key (list month year active))
         (cache-val (gethash cache-key agenda-view--calendar-cache)))
    
    (if (and (not force) cache-val)
        cache-val
      (let* ((first (list month 1 year))
             (first-day (calendar-absolute-from-gregorian first))
             (first-mon (- first-day (mod (+ (calendar-day-of-week first) 6) 7)))
             (header (format "%s %d" (calendar-month-name month) year))
             (spaces (- 20 (length header)))
             (face (if (not active) 'shadow nil))
             (calendar nil))
        (push (concat (propertize
                       (concat (make-string (/ spaces 2) ?\s)
                               header
                               (make-string (- spaces (/ spaces 2)) ?\s))
                       'face `(:inverse-video t :inherit (,face bold)))
                      " ") calendar)
        (push (concat (propertize "Mo Tu We Th Fr " 'face `(bold ,face))
                      (propertize "Sa Su "'face face)) calendar)
        (dotimes (i 6)
          (let ((line ""))
            (dotimes (j 7)
              (let* ((index (+ (* i 7) j))
                     (day-date (calendar-gregorian-from-absolute (+ first-mon index)))                     
                     (is-today (and (eq (nth 1 day-date) day)
                                    (eq (nth 0 day-date) month)))
                     (is-month (eq (nth 0 day-date) month))
                     (is-weekend (memq (calendar-day-of-week day-date) '(0 6)))
                     (slots (agenda-collect-slots day-date))
                     (entries (seq-find #'identity slots))
                     (has-deadline (seq-some
                                    (lambda (entry)
                                      (member "DEADLINE" (cdr (assoc 'tags entry))))
                                    entries))
                     ;; FIXME: Does that work a slot has several entries?
                     (first-entry (car entries))
                     (marker (cdr (assoc 'marker first-entry)))
                     (help (agenda-view--display-summary slots))
                     (total (/ (seq-count #'identity slots) 2))
                     (palette agenda-calendar-palette)
                     (total (min total (length palette)))
                     (face (cond
;;                            ((and is-month has-deadline)
  ;;                           '(:background "#FFFF00" :foreground "#000000"))
                            ((and is-month (> total 0))
                             `( :inherit ,(if has-deadline 'bold 'default)
                                :foreground ,(car (nth total palette))
                                :background ,(cdr (nth total palette))))
                            ((and is-month is-weekend)
                             'shadow)))
                     (day (if is-month
                              (if has-deadline
                                  (format "%2d%s" (nth 1 day-date) (agenda-mark 'deadline))
                                (format "%2d " (nth 1 day-date)))                            
                            "   ")))
                (setq line (concat line
                                   (propertize day
                                               'agenda-entry-marker marker
                                               'agenda-date-marker (when is-month day-date)
                                               'help-echo help
                                               'face face)))))
            (push line calendar)))
        (let ((result (concat
                       (propertize  "┌──────────────────────┐\n" 'face face)
                       (mapconcat (lambda (line)
                                    (concat (propertize "│ " 'face face)
                                            line
                                            (propertize "│" 'face face)))
                                  (reverse calendar) "\n")
                       (propertize "\n└──────────────────────┘\n" 'face face))))
          (puthash cache-key result agenda-view--calendar-cache)
          result)))))

(defun agenda-view-refresh ()
  "Refresh the current view with forced update."
  (interactive)
  (agenda-view nil nil t))

(defun agenda-view-goto-today ()
  "Jump to the current week."
  (interactive)
  (agenda-view (calendar-current-date)))

(defun agenda-view-goto-date ()
  "Select a date via calendar and open the view for that week."
  (interactive)
  (agenda-with-calendar-select date
    (agenda-view date)))

(defun agenda-view-goto-next ()
  "Go to the next week."
  (interactive)
  (let ((date (agenda-date-forward agenda-view-date +1 agenda-view-unit)))
    (agenda-view date)))

(defun agenda-view-goto-prev ()
  "Go to the next week."
  (interactive)
  (let ((date (agenda-date-forward agenda-view-date -1 agenda-view-unit)))
    (agenda-view date)))

(defun agenda-view-goto-date-at-point ()
  "Select a date via calendar and open the view for that week."
  (interactive)
  (when-let* ((date (get-text-property (point) 'agenda-date-marker)))
    (agenda-view date)))

(defun agenda-view-goto-up ()
  (interactive)
  (if-let* ((date (get-text-property (point) 'agenda-date-marker)))
      (cond ((memq agenda-view-unit '(day year))
             (agenda-view (agenda-date-forward date -7 'day)))
            ((memq agenda-view-unit '(month month-3))
             (when-let* ((date-prev (agenda-date-forward date -1 'day))
                         (prop (save-excursion
                                 (goto-char (point-min))
                                 (text-property-search-forward 'agenda-date-marker
                                                               date-prev t))))
               (goto-char (prop-match-beginning prop))))
            ((memq agenda-view-unit '(week))
             (when-let* ((prop (save-excursion
                                 (text-property-search-backward 'agenda-date-marker))))
               (goto-char (prop-match-beginning prop))))             
            (t (call-interactively #'previous-line)))
    (call-interactively #'previous-line)))

(defun agenda-view-goto-down ()
  (interactive)
  (if-let* ((date (get-text-property (point) 'agenda-date-marker)))
      (cond ((memq agenda-view-unit '(day year))
             (agenda-view (agenda-date-forward date +7 'day)))
            ((memq agenda-view-unit '(month month-3))
             (when-let* ((date-prev (agenda-date-forward date +1 'day))
                         (prop (save-excursion
                                 (goto-char (point-min))
                                 (text-property-search-forward 'agenda-date-marker
                                                               date-prev t))))
               (goto-char (prop-match-beginning prop))))
            ((memq agenda-view-unit '(week))
             (when-let* ((prop (save-excursion
                                 (text-property-search-forward 'agenda-date-marker nil nil t))))
               (goto-char (prop-match-beginning prop))))
            (t (call-interactively #'next-line)))
    (call-interactively #'next-line)))

(defun agenda-view-goto-left ()
  (interactive)
  (if-let* ((date (get-text-property (point) 'agenda-date-marker)))
      (cond ((memq agenda-view-unit '(day year))
             (agenda-view (agenda-date-forward date -1 'day)))
            ((memq agenda-view-unit '(month month-3))
             (if-let* ((date-prev (agenda-date-forward date -1 'month))
                       (prop (save-excursion
                               (goto-char (point-min))
                               (text-property-search-forward 'agenda-date-marker
                                                             date-prev t))))
                 (goto-char (prop-match-beginning prop))
               (call-interactively #'left-char)))
            (t   (call-interactively #'left-char)))
    (call-interactively #'left-char)))

(defun agenda-view-goto-right ()
  (interactive)
  (if-let* ((date (get-text-property (point) 'agenda-date-marker)))
      (cond ((memq agenda-view-unit '(day year))
             (agenda-view (agenda-date-forward date +1 'day)))
            ((memq agenda-view-unit '(month month-3))
             (if-let* ((date-prev (agenda-date-forward date +1 'month))
                         (prop (save-excursion
                                 (goto-char (point-min))
                                 (text-property-search-forward 'agenda-date-marker
                                                               date-prev t))))
                 (goto-char (prop-match-beginning prop))
               (call-interactively #'right-char)))
            (t (call-interactively #'right-char)))
    (call-interactively #'right-char)))

(defun agenda-view-goto-shift-left ()
  (interactive)
  (if-let* ((date (and (memq agenda-view-unit '(day year month))
                       agenda-view-date)))
      (agenda-view (agenda-date-forward date -1 'month))
    (call-interactively #'left-char)))

(defun agenda-view-goto-shift-right ()
  (interactive)
  (if-let* ((date (and (memq agenda-view-unit '(day year month))
                       agenda-view-date)))
      (agenda-view (agenda-date-forward date +1 'month))
    (call-interactively #'right-char)))

(defun agenda-view-goto-shift-up ()
  (interactive)
  (if-let* ((date (and (memq agenda-view-unit '(year))
                       agenda-view-date)))
      (agenda-view (agenda-date-forward date -4 'month))
    (call-interactively #'previous-line)))

(defun agenda-view-goto-shift-down ()
  (interactive)
  (if-let* ((date (and (memq agenda-view-unit '(year))
                       agenda-view-date)))
      (agenda-view (agenda-date-forward date +4 'month))
    (call-interactively #'next-line)))

(defun agenda-view-goto-entry ()
  "Jump to the source line of the entry at point or to the closest date if
no entry at point. When in year mode, switch to week mode instead."
  (interactive)  
  (let* ((marker (get-text-property (point) 'agenda-entry-marker))
         (date (get-text-property (point) 'agenda-date-marker)))
    (cond ((eq agenda-view-unit 'year)
           (agenda-view-week))

          (marker
           (pop-to-buffer (marker-buffer marker))
           (agenda-filter "")
           (goto-char (marker-position marker))
           (agenda-highlight (line-beginning-position)
                             (line-end-position))
           (recenter))
          
          (date
           (pop-to-buffer (agenda-source-buffer))
           (agenda-filter "")
           (remove-text-properties (point-min) (point-max) '(invisible nil))
           (agenda--goto-date date)
           (agenda-highlight (line-beginning-position)
                             (line-end-position))
           (recenter)))))

(defun agenda-view-day (&optional date force)
  "Display the agenda for DATE in day mode.
When FORCE is t, force update from source."
  (interactive)
  (agenda-view date 'day force))

(defun agenda-view-week (&optional date force)
  "Display the agenda for DATE in week mode.
When FORCE is t, force update from source."
  (interactive)
  (agenda-view date 'week force))

(defun agenda-view-month-1 (&optional date force)
  "Display the agenda for DATE in month mode.
When FORCE is t, force update from source."
  (interactive)
  (agenda-view date 'month force))

(defun agenda-view-month-3 (&optional date force)
  "Display the agenda for DATE in 3 months mode.
When FORCE is t, force update from source."
  (interactive)
  (agenda-view date 'month-3 force))

(defun agenda-view-year (&optional date force)
  "Display the agenda for DATE in year mode.
When FORCE is t, force update from source."
  (interactive)
  (agenda-view date 'year force))

(defun agenda-view-capture (&optional with-data)
  "Capture a new entry using date at point (if any)."
  (interactive "P")
  (when-let ((date (or (get-text-property (point) 'agenda-date-marker)
                       (agenda-view-date)
                       (calendar-current-date))))
    (agenda-capture date with-data)))

(defun agenda-view (&optional date unit force)
  "Display the agenda for DATE in UNIT mode.
When FORCE is t, force update from source."
  (interactive)
  (let* ((buffer (get-buffer-create agenda-view-buffer))
         (date (or date
                   agenda-view-date
                   (calendar-current-date)))
         (unit (or unit agenda-view-unit 'day)))
    (setq agenda-view-date date)
    (setq agenda-view-unit unit)
    (if (memq agenda-view-unit '(week month month-3))
        (hl-line-mode 1)
      (hl-line-mode -1))      
    (setq-local hl-line-range-function #'agenda-view--hl-line-range)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cond ((eq unit 'day)     (agenda-view-day-display nil force))
              ((eq unit 'week)    (agenda-view-week-display nil force))
              ((eq unit 'month)   (agenda-view-month-1-display nil force))
              ((eq unit 'month-3) (agenda-view-month-3-display nil force))
              ((eq unit 'year)    (agenda-view-year-display nil force))
              (t (error (format "Unknown view mode (%s)" unit))))
        (set-buffer-modified-p nil)
        (unless (derived-mode-p 'agenda-view-mode)
          (agenda-view-mode))
        (setq mode-name (concat "Agenda View: "
                                (cond ((eq agenda-view-unit 'day)     "Day")
                                      ((eq agenda-view-unit 'week)    "Week")
                                      ((eq agenda-view-unit 'month)   "Month")
                                      ((eq agenda-view-unit 'month-3) "3 Months")
                                      ((eq agenda-view-unit 'year)    "Year"))))
        (pop-to-buffer buffer)))))

(defvar agenda-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")       #'agenda-view-goto-entry)
    (define-key map (kbd "TAB")       #'agenda-view-goto-entry)
    (define-key map (kbd "<mouse-1>") #'agenda-view-goto-date-at-point)
    (define-key map (kbd "c")         #'agenda-view-capture)
    (define-key map (kbd "r")         #'agenda-view-refresh)
    (define-key map (kbd "C-l")       #'agenda-view-refresh)
    (define-key map (kbd "e")         #'agenda-export)
    (define-key map (kbd "d")         #'agenda-view-day)
    (define-key map (kbd "w")         #'agenda-view-week)
    (define-key map (kbd "m")         #'agenda-view-month-1)
    (define-key map (kbd "t")         #'agenda-view-month-3)
    (define-key map (kbd "y")         #'agenda-view-year)    
    (define-key map (kbd "g")         #'agenda-view-goto-date)
    (define-key map (kbd ".")         #'agenda-view-goto-today)

    (define-key map (kbd "<left>")    #'agenda-view-goto-left)
    (define-key map (kbd "<right>")   #'agenda-view-goto-right)
    (define-key map (kbd "<up>")      #'agenda-view-goto-up)
    (define-key map (kbd "<down>")    #'agenda-view-goto-down)
    (define-key map (kbd "S-<left>")  #'agenda-view-goto-shift-left)
    (define-key map (kbd "S-<right>") #'agenda-view-goto-shift-right)
    (define-key map (kbd "S-<up>")    #'agenda-view-goto-shift-up)
    (define-key map (kbd "S-<down>")  #'agenda-view-goto-shift-down)

    (define-key map (kbd "f")         #'agenda-view-goto-next)
    (define-key map (kbd "n")         #'agenda-view-goto-next)
    (define-key map (kbd "b")         #'agenda-view-goto-prev)
    (define-key map (kbd "p")         #'agenda-view-goto-prev)
    map)
  "Keymap for `agenda-view-mode'.")

(defun agenda-view--hl-line-range ()
  "Restrict line highlight to some parts of the line.
 Each view mode defines relevant range as 'agenda-hl-range'."
  (save-excursion
    (save-restriction
      (save-match-data
        (let* ((prop 'agenda-hl-range)
               (val t)
               (lbp (line-beginning-position))
               (lep (line-end-position))
               beg end)
          ;; Restrict all buffer operations to just this line
          (narrow-to-region lbp lep)
          (cond
           ;; 1. Check if point is already ON the property
           ((eq (get-text-property (point) prop) val)
            (setq beg (previous-single-property-change
                       (min (1+ (point)) (point-max)) prop nil lbp)
                end (next-single-property-change (point) prop nil lep)))         
           ;; 2. BACKWARD search: Only if NOT at the very start of the line
           ((and (> (point) lbp)
                 (let ((match (text-property-search-backward prop val t)))
                   (when (and match (>= (prop-match-beginning match) lbp))
                     (setq beg (prop-match-beginning match)
                           end (prop-match-end match))
                     t)))) ; Return t to break the cond
           ;; 3. FORWARD search: Look ahead within the same line
           (t
            (let ((match-fwd (text-property-search-forward prop val t)))
              (when (and match-fwd (< (prop-match-beginning match-fwd) lep))
                (setq beg (prop-match-beginning match-fwd)
                      end (prop-match-end match-fwd))))))
      (cons (or beg lbp) (or end lep)))))))

(define-derived-mode agenda-view-mode special-mode "Agenda view"
  "Major mode for displaying the agenda dashboard."
  (setq truncate-lines t)
  (hl-line-mode 1)
  (setq-local hl-line-range-function #'agenda-view--hl-line-range)
  (face-remap-add-relative 'hl-line 'nano-salient-h)
  (add-hook 'post-command-hook (lambda ()
                                 (let ((message-log-max nil))
                                   (display-local-help t)) nil t)))

(provide 'agenda-view)
;;; agenda-view.el ends here
