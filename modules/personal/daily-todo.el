;;; daily-todo.el --- File shortcuts and daily navigation -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-id)
(require 'paths)
(require 'gtd-config)   ; my/get-project-files, my/area-hub-file

;;; --- File Open Commands ---

(defun my/open-journal-file ()
  "Open journal file and jump to today's heading, creating it if needed."
  (interactive)
  (find-file my/journal-file)
  (journal--ensure-daily-heading))

(defun my/open-project-file ()
  "Pick a project file under Areas/<Area>/ via completion and open it."
  (interactive)
  (let* ((files (my/get-project-files))
         (names (mapcar (lambda (f) (file-relative-name f my/areas-dir)) files))
         (choice (completing-read "Project: " names nil t)))
    (find-file (expand-file-name choice my/areas-dir))))

(defun my/open-job-applications-file ()
  "Open the job applications file."
  (interactive)
  (find-file my/job-applications-file))

(defun my/open-area-file ()
  "Pick one of the three area hubs and open it."
  (interactive)
  (find-file (my/area-hub-file (completing-read "Area: " my/area-names nil t))))

(defun my/open-notes-file ()
  "Open the rough notes file."
  (interactive)
  (find-file my/rough-notes-file))

;;; Keybindings
;; C-c o (open files) and C-c t i / o (clock in / out)
;; are in keymaps.el.  Narrow to subtree is Org's own C-x n s.

(provide 'daily-todo)
;;; daily-todo.el ends here
