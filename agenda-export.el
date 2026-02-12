;;; agenda-export.el --- Export functions -*- lexical-binding: t; -*-

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

(defun agenda-export--ical-sanitize (string)
  "Sanitize STRING by escaping commas, semicolons, and newlines with a
backslash and removing unicode nerd icons."
  (when (stringp string)
    (setq string (replace-regexp-in-string (regexp-quote "\\") "\\\\" string t t))
    (setq string (replace-regexp-in-string (regexp-quote ",")  "\\," string t t))
    (setq string (replace-regexp-in-string (regexp-quote ";")  "\\;" string t t))
    (setq string (replace-regexp-in-string (regexp-quote "\n") "\\n" string t t))
    (setq string (replace-regexp-in-string "[\uE000-\uF8FF] " "" string))
    string))

(defun agenda-export--ical-uid (date time public)
  "Generate UID from DATE, TIME and PUBLIC.
Public UIDs are suffixed with '-PUBLIC', else with '-PRIVATE'."
  (format "%04d%02d%02dT%02d%02d%s"
          (nth 0 date) (nth 1 date) (nth 2 date)
          (if time (nth 0 time) 0)
          (if time (nth 1 time) 0)
          (if public "-PUBLIC" "-PRIVATE")))

(defun agenda-export--ical-timestamp (date time &optional public)
  (if time
      (format "%04d%02d%02dT%02d%02d00" 
              (nth 0 date) (nth 1 date) (nth 2 date)
              (nth 0 time) (nth 1 time))
    (format "%04d%02d%02d"
            (nth 0 date) (nth 1 date) (nth 2 date))))

(defun agenda-export--ical-entry (entry &optional public)  
  "Return regular ENTRY as a VEVENT ical string.
 Generated UID and informations depends on whether PUBLIC is t."
  (let* ((date (cdr (assoc 'date entry)))
         (time-start (cdr (assoc 'time-start entry)))
         (time-end (cdr (assoc 'time-end entry)))
         (title (cdr (assoc 'title entry)))
         (actions (cdr (assoc 'actions entry))) 
         (tags (cdr (assoc 'tags entry)))
         (is-deadline (member "DEADLINE" tags))
         (is-birthday (member "BIRTHDAY" tags))
         (data (cdr (assoc 'data entry)))
         (url (cdr (assoc 'url data)))
         (note (cdr (assoc 'note data)))
         (map (cdr (assoc 'map data))))
    (concat
     "BEGIN:VEVENT\n"
     ;; UID
     (format "UID:%s\n" (agenda-export--ical-uid date time-start public))

     ;; DSTAMP
     (format "DTSTAMP:%s\n" (format-time-string "%Y%m%dT%H%M%SZ" nil t))

    ;; DTSTART or DUE
     (if is-deadline
         (concat (format "DUE:%s\n" (agenda-export--ical-timestamp date time-start))
                 "STATUS:NEEDS-ACTION\n")
       (format "DTSTART:%s\n" (agenda-export--ical-timestamp date time-start)))

     ;; DTEND
     (when (and time-end (not is-deadline) (not is-birthday))
       (format "DTEND:%s\n" (agenda-export--ical-timestamp date time-end)))

     ;; BIRTHDAY
     (when is-birthday
       (concat "RRULE:FREQ=YEARLY\n"
               "TRANSP:TRANSPARENT\n"
               "X-APPLE-SPECIAL-DAY:BIRTHDAY\n"))

     ;; PUBLIC part
     (if public
         ;; SUMMARY
         (if map
             (format "SUMMARY:(away)\n")
           (format "SUMMARY:(busy)\n"))
       
       ;; PRIVATE part
       (concat
        ;; SUMMARY
        (format "SUMMARY:%s (%s)\n"
                (agenda-export--ical-sanitize title)
                (mapconcat #'string-trim actions ","))
        ;; CATEGORIES
        (when tags (format "CATEGORIES:%s\n" (mapconcat #'string-trim tags ",")))
        ;; URL
        (when url (format "URL:%s\n" url))
        ;; LOCATION
        (when map (format "LOCATION:%s\n" map))
        ;; DESCRIPTION
        (when note (format "DESCRIPTION: %s\n" (agenda-export--ical-sanitize note)))))
     "END:VEVENT\n")))

(defun agenda-export-ical (filename &optional public)
  (let ((entries (agenda-collect-entries t)))
    (with-temp-buffer
      (insert "BEGIN:VCALENDAR\n"
              "VERSION:2.0\n"
              "CALSCALE:GREGORIAN\n"
              (format "X-WR-CALNAME:%s/%s\n"
                      (upcase user-login-name)
                      (upcase (if public "public" "private")))
              (format-time-string "X-WR-TIMEZONE:%Z\n")
              "\n")
      (dolist (entry entries)
        (insert (agenda-export--ical-entry entry public) "\n"))
      (insert "\n"
              "END:VCALENDAR\n")
      (write-region (point-min) (point-max) filename))))

(defun agenda-export-sync ()
  "Run 'vdirsyncer sync' and report status / time."
  (interactive)
  (let* ((buf-name "*Vdirsyncer Output*")
         (buf (get-buffer-create buf-name))
         (proc (get-buffer-process buf))
         (start-time (float-time (current-time))))    
    (if (process-live-p proc)
        (message "vdirsyncer is already running.")      
      (progn
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)))        
        (message "vdirsyncer: sync started...")        
        (let ((new-proc (start-process-shell-command "vdir-sync" buf "vdirsyncer sync")))
          (set-process-sentinel new-proc
              (lambda (p event)
                (let* ((elapsed (- (float-time (current-time)) start-time))
                       (p-buf (process-buffer p)))
                  (cond 
                   ((string-match-p "finished" event)
                    (message "vdirsyncer: sync complete (%.2fs)." elapsed))
                   ((string-match-p "exited abnormally" event)
                    (message "vdirsyncer: sync failed (%.2fs)." elapsed)
                    (pop-to-buffer p-buf)))))))))))

(defun agenda-export (&optional no-sync)
  "Export agenda in public and private ical files and synchronize them
using vdirsyncer unless NO-SYNC is t."
  (interactive)
  (if (and agenda-file-ical-public
           (file-exists-p agenda-file-ical-public))
      (agenda-export-ical agenda-file-ical-public t)
    (error "Public ical file (%s) does not exist." agenda-file-ical-public))
  (if (and agenda-file-ical-private
           (file-exists-p agenda-file-ical-private))
      (agenda-export-ical agenda-file-ical-private nil)
    (error "Private ical file (%s) does not exist." agenda-file-ical-private))
  (unless no-sync
    (agenda-export-sync)))

(provide 'agenda-export)
;;; agenda-export.el ends here
