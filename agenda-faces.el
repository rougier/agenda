;;; agenda-faces.el --- Visual styles and customization -*- lexical-binding: t; -*-

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

(defgroup agenda nil
  "Agenda"
  :group 'applications)

(defgroup agenda-edit-faces nil
  "Agenda faces for edit mode."
  :group 'agenda)

(defgroup agenda-view-faces nil
  "Agenda faces for view mode."
  :group 'agenda)

(defface agenda-edit-highlight
  '((t :inherit (widget-field bold default)
       :inverse-video nil))
  "Face for transient highlighting (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-inweek
  '((t :inherit (default)))
  "Face for entries in current week (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-outweek
  '((t :inherit (shadow)))
  "Face for entries oustide current week (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-today
  '((t :inherit (bold)))
  "Face for today entries (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-tag
  '((t :inherit (warning)))
  "Face for tags (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-action
  '((t :inherit (link)))
  "Face for actions (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-reference
  '((t :inherit shadow))
  "Face for references (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-data
  '((t :inherit shadow))
  "Face for data line (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-data-ref
  '((t :inherit (shadow bold)))
  "Face for key on data line (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-cancel
  '((t :strike-through t :inherit shadow))
  "Face for cancelled entries (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-deadline
  '((t :inherit (error bold)))
  "Face for deadline entries (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-header
  '((t :inherit (bold)))
  "Face for headers (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-header-prefix
  '((t :inherit (shadow default)))
  "Face for headers prefix (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-header-summary
  '((t :inherit (warning)))
  "Face for headers entries summary (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-data
  '((t :inherit (shadow)))
  "Face for date line (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-ref
  '((t :inherit (bold)))
  "Face for date line reference (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-edit-comment
  '((t :inherit (shadow)))
  "Face for comments (edit mode)."
  :group 'agenda-edit-faces)

(defface agenda-view-title
  '((t :inherit (bold)))
  "Face for title (week, view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-subtitle
  '((t :inherit (shadow)))
    "Face for subtitle (week extent, view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-date
  '((t :inherit (bold)))
  "Face for date (view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-tag
  '((t :inherit (agenda-edit-tag)))
  "Face for tags (view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-action
  '((t :inherit (agenda-edit-action)))
  "Face for actions (view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-cancel
  '((t :inherit agenda-edit-cancel))
  "Face for cancelled entries (view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-button
  '((t :inherit (link bold)))
  "Face for buttons (view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-time
  '((t :inherit (bold)))
  "Face for time (view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-deadline-header
  '((t :inherit (bold error)))
  "Face for deadline header (view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-date-today
  '((t :inherit (bold error)))
  "Face for today text in view mode."
  :group 'agenda-faces)

(defface agenda-view-time-today
  '((t :inherit (bold)))
  "Face for today time (view mode)."
  :group 'agenda-view-faces)

(defface agenda-view-time-now
  '((t :inverse-video t :inherit bold))
  "Face for now time (view mode)."
  :group 'agenda-view-faces)

(defface agenda-deadline-0
  '((t :inverse-video t :inherit (bold)))
  "Face for deadline with priority 0."
  :group 'agenda-view-faces)

(defface agenda-deadline-1
  '((t :inverse-video t :inherit (bold error)))
  "Face for deadline with priority 1."
  :group 'agenda-view-faces)

(defface agenda-deadline-2
  '((t :inherit (bold error)))
  "Face for deadline with priority 2."
  :group 'agenda-view-faces)

(defface agenda-deadline-3
  '((t :inherit (bold)))
  "Face for deadline with priority 3."
  :group 'agenda-view-faces)

(defface agenda-slot-free
  '((t :inherit region))
  "Face for free slot."
  :group 'agenda-view-faces)

(defface agenda-slot-busy
  '((t :inverse-video t :inherit (bold shadow)))
  "Face for busy slot."
  :group 'agenda-view-faces)

(defface agenda-slot-busy-inweek
  '((t :inverse-video t :inherit (bold)))
  "Face for busy slot for the current week."
  :group 'agenda-view-faces)

(defface agenda-slot-deadline
  '((t :inverse-video t :inherit (error bold)))
  "Face for busy & deadline slot."
  :group 'agenda-view-faces)

(defface agenda-slot-personal
  '((t :inverse-video t :inherit (link bold)))
  "Face for busy & personal slot."
  :group 'agenda-view-faces)

(provide 'agenda-faces)
;;; agenda-faces.el ends here
