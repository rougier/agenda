;;; agenda.el --- A fast & simple text-based agenda -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/agenda
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: calendar, agenda, convenience

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

;;; Commentary:

;; This package provides a fast, lightweight agenda system based on a
;; simple text file format. It provides an edit mode, to ease entering
;; information and a view mode to render the agenda for day, week,
;; month, 3 months or year.
;;
;; The project is structured into several sub-modules:
;;   - agenda-faces.el          : Faces definition
;;   - agenda-parse.el          : Parsing and test functions
;;   - agenda-utils.el          : Various utility functions
;;   - agenda-edit.el           : Minor edit mode
;;   - agenda-view.el           : Major view mode
;;     - agenda-view-day.el     : Day view
;;     - agenda-view-week.el    : Week view
;;     - agenda-view-month.el   : Single month and 3 months view
;;     - agenda-view-year.el    : Year view
;;   - agenda-export.el         ; Export functions (ical)
;;   - agenda-capture.el        : Capture functions

;; Note on date formats:
;;  - Calendar format is (month day year)
;;  - Agenda entry format is (year month day)
;;  - Timestamp format is (second minute hour day month year)
;;  - ISO commercial format is (week weekday year)

;; It is important to distinguish between two modes:
;; - agenda-edit-mode: For the editing the source file 
;; - agenda-view-mode: For the viewing the agenda

;;; Code:
(require 'cal-iso)
(require 'calendar)
(require 'agenda-faces)
(require 'agenda-parse)
(require 'agenda-utils)
(require 'agenda-view)
(require 'agenda-edit)
(require 'agenda-export)
(require 'agenda-capture)

(defcustom agenda-file (expand-file-name "agenda.txt" user-emacs-directory)
  "Agenda file"
  :group 'agenda
  :type 'file)

(defcustom agenda-file-ical-public "~/.calendars/Public.ics"
  "Agenda export filename for public agenda (personal information stripped)."
  :group 'agenda
  :type 'file)

(defcustom agenda-file-ical-private "~/.calendars/Private.ics"
  "Agenda export filename for public agenda (personal information visible)."
  :group 'agenda
  :type 'file)

(defconst agenda-view-buffer "*agenda*"
  "Name of the agenda view buffer")

(defvar agenda--highlight-overlay nil
  "Overlay used for transient highlighting.")

(defcustom agenda-marks '((inweek   . "○")
                          (today    . "●")
                          (cancel   . "")
                          (capture  . " ") 
                          (birthday . "󰃫")
                          (conflict . "*")
                          (deadline . "!"))
  "List of margin marks."
  :group 'agenda
  :type '(alist :key-type (symbol :tag "State")
                :value-type (string :tag "Mark")))

(defcustom agenda-slot-faces
  '(("DEADLINE"       . agenda-slot-deadline)
    ("PERSONAL\\|HOME" . agenda-slot-personal))
  "Mapping of tag to faces."
  :type '(alist :key-type (string :tag "Tags filter")
                :value-type (face :tag "Face"))
  :group 'agenda)

(defcustom agenda-deadline-faces
  '(( 0 . agenda-deadline-0)
    ( 7 . agenda-deadline-1)    
    (15 . agenda-deadline-2)
    (30 . agenda-deadline-3))
  "Mapping of days to deadlines faces (view week mode)."
  :type '(alist :key-type (integer :tag "Number of days")
                :value-type (face :tag "Face"))
  :group 'agenda)

(defcustom agenda-calendar-palette
  (let* ((black (face-foreground 'default nil 'default))
         (white (face-background 'default nil 'default)))
    (list (cons black white)       ;; Base entry
          ;; Material deep orange shades
          (cons black "#FBE9E7")   ;; 50
          (cons black "#FFCCBC")   ;; 100
          (cons black "#FFAB91")   ;; 200
          (cons black "#FF8A65")   ;; 300
          (cons black "#FF7043")   ;; 400
          (cons white "#FF5722")   ;; 500
          (cons white "#F4511E")   ;; 600
          (cons white "#E64A19")   ;; 700
          (cons white "#D84315")   ;; 800
          (cons white "#BF360C"))) ;; 900  
  "List of (FG . BG) color pairs for calendar highlighting.
The first entry is a base (foreground . background) pair using the default Emacs
colors. Subsequent entries correspond to the standard Deep Orange shades (50–900),
with foreground chosen for readability (black on light backgrounds, white on dark)."
  :type '(repeat (cons color color))
  :group 'nano-agenda)

;; Installing agenda-mode for the agenda-file
(add-to-list 'auto-mode-alist (cons agenda-file 'agenda-edit-mode))

;;;###autoload
(defun agenda (&optional empty-cache)
  "Invalidate all caches if EMPTY-CACHE is t and popup the agenda."  
  (interactive)
  (unless (and agenda-file (file-exists-p agenda-file))
    (error "Agenda file not found: %s. " agenda-file))
  (setq agenda-source-buffer (find-file-noselect agenda-file))
  (with-current-buffer agenda-source-buffer
    (when empty-cache
      (setq agenda--entries-cache nil
            agenda--uids-cache nil
            agenda--slots-cache nil)
      (when agenda-view--calendar-cache
        (clrhash agenda-view--calendar-cache)))
    (agenda-edit-mode)
    (agenda-parse-buffer)
    (agenda-view)))

(bind-key (kbd "M-a") #'agenda)
(bind-key (kbd "M-c") #'agenda-capture-with-calendar)

(provide 'agenda)
;;; agenda.el ends here
