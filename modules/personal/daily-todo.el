;;; daily-todo.el --- File shortcuts and daily navigation -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-id)
(require 'paths)

;;; --- File Open Commands ---

(defun my/open-journal-file ()
  "Open journal file and jump to today's heading, creating it if needed."
  (interactive)
  (find-file my/journal-file)
  (goto-char (point-min))
  (let* ((heading (format-time-string "%Y-%m-%d %A"))
         (regex (format "^\\* %s$" heading)))
    (unless (re-search-forward regex nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "\n* %s\n" heading)))
    (end-of-line)))

(defun my/open-tasks-file ()
  "Open the main tasks file."
  (interactive)
  (find-file my/tasks-file))

(defun my/open-logbook-file ()
  "Open the logbook file."
  (interactive)
  (find-file my/logbook-file))

(defun my/open-projects-file ()
  "Open the projects file."
  (interactive)
  (find-file my/gtd-projects-file))

(defun my/open-shortcuts-file ()
  "Open the shortcuts reference file."
  (interactive)
  (find-file my/shortcuts-file))

(defun my/open-job-applications-file ()
  "Open the job applications file."
  (interactive)
  (find-file my/job-applications-file))

(defun my/open-notes-file ()
  "Open the rough notes file."
  (interactive)
  (find-file my/rough-notes-file))

;;; --- Notes Index ---

(defun my/notes-rebuild-index ()
  "Rebuild the notes index file with a Year->Month->Week structure."
  (interactive)
  (let ((notes-by-date (make-hash-table :test 'equal)))
    (with-current-buffer (find-file-noselect my/rough-notes-file)
      (org-map-entries
       (lambda ()
         (let* ((id (org-entry-get (point) "ID"))
                (headline (org-get-heading t t))
                (time-str (org-entry-get (point) "TIME"))
                (time (and time-str (org-time-string-to-seconds time-str))))
           (when (and id time)
             (let* ((year  (format-time-string "%Y" time))
                    (month (format-time-string "%m" time))
                    (week  (format-time-string "%V" time))
                    (year-ht   (or (gethash year notes-by-date)
                                   (make-hash-table :test 'equal)))
                    (month-ht  (or (gethash month year-ht)
                                   (make-hash-table :test 'equal)))
                    (week-list (or (gethash week month-ht) '())))
               (setf (gethash week month-ht) (cons (list headline id) week-list))
               (setf (gethash month year-ht) month-ht)
               (setf (gethash year notes-by-date) year-ht)))))
       "NOTE|IDEA" 'file))
    (with-temp-file my/shortcuts-file
      (insert "#+TITLE: Notes and Ideas Index\n\n")
      (dolist (year (cl-sort (hash-table-keys notes-by-date) #'string<))
        (insert (format "* %s\n" year))
        (let ((year-ht (gethash year notes-by-date)))
          (dolist (month (cl-sort (hash-table-keys year-ht) #'string<))
            (let* ((month-name (format-time-string "%B"
                                (encode-time 0 0 0 1
                                  (string-to-number month)
                                  (string-to-number year))))
                   (month-ht (gethash month year-ht)))
              (insert (format "** %s-%s %s\n" year month month-name))
              (dolist (week (cl-sort (hash-table-keys month-ht) #'string<))
                (let ((note-list (gethash week month-ht)))
                  (insert (format "*** Week %s\n" week))
                  (dolist (item (reverse note-list))
                    (insert (format "- [[id:%s][%s]]\n"
                                    (cadr item) (car item))))))))))))
  (message "Notes index rebuilt."))

;;; --- Single Buffer View ---

(defun my/open-single-buffer ()
  "Open current org heading in indirect buffer, focused fullscreen."
  (interactive)
  (org-tree-to-indirect-buffer)
  (other-window 1)
  (delete-other-windows)
  (end-of-buffer 1)
  (recenter-top-bottom t))

;;; Keybindings
(global-set-key (kbd "C-c o j") #'my/open-journal-file)
(global-set-key (kbd "C-c o d") #'my/open-tasks-file)
(global-set-key (kbd "C-c o h") #'my/open-logbook-file)
(global-set-key (kbd "C-c o p") #'my/open-projects-file)
(global-set-key (kbd "C-c o s") #'my/open-shortcuts-file)
(global-set-key (kbd "C-c o y") #'my/open-job-applications-file)
(global-set-key (kbd "C-c o n") #'my/open-notes-file)
(global-set-key (kbd "C-c o b") #'my/open-single-buffer)
(global-set-key (kbd "C-c b n") #'my/notes-rebuild-index)
(global-set-key (kbd "C-x i")   #'org-clock-in)
(global-set-key (kbd "C-x j")   #'org-clock-out)

(provide 'daily-todo)
;;; daily-todo.el ends here
