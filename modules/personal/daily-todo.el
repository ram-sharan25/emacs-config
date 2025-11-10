;;; flat-diary-config.el --- Flat Org diary with tags and properties -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'org-id)
(require 'org-element)

(setq personal-dir "~/Stillness/Personal/Writings/")
 ;;"Base directory for personal writings."
(setq diary-file (concat personal-dir "Tasks.org"))
(setq projects-file (concat personal-dir "Projects.org"))
(setq log-file (concat personal-dir "LogBook.org"))
(setq journal-file (concat personal-dir "Journal.org"))
(setq notes-file (concat personal-dir "RoughNotes.org"))
(setq index-file (concat personal-dir "NotesIndex.org"))
(setq todo-index-file (concat personal-dir "TodoIndex.org"))
(setq shortcut-file "~/Stillness/Personal/Notes/shortcuts_in_emacs.org")


(defun ensure-date-tree ()
  "Create the year/month/day heading structure for today if it doesn't exist."
  (save-excursion
    (let* ((year (format-time-string "%Y"))
	   (month (format-time-string "%Y-%m %B"))
	   (day (format-time-string "%Y-%m-%d %A"))
	   (year-regex (format "^\\* %s$" year))
	   (month-regex (format "^\\** %s$" month))
	   (day-regex (format "^\\*** %s$" day)))

      (goto-char (point-min))

      ;; 1. Find or create Year headline (* Year)
      (unless (re-search-forward year-regex nil t)
	(goto-char (point-max))
	(insert (format "\n* %s" year))
	;; Land the cursor on the new heading for the next search limit calculation
	(re-search-backward year-regex (point-min) t))

      ;; 2. Find or create Month headline (** Month) inside the Year
      (let ((year-end (org-end-of-subtree nil t)))
	(unless (re-search-forward month-regex year-end t)
	  ;; If not found within the year, insert it at the end of the year's content
	  (goto-char year-end)
	  (insert (format "\n** %s" month))
	  ;; Land on the new month headline
	  (re-search-backward month-regex (point-min) t)))

      ;; 3. Find or create Day headline (*** Day) inside the Month
      (let ((month-end (org-end-of-subtree nil t)))
	(unless (re-search-forward day-regex month-end t)
	  ;; If not found within the month, insert it at the end of the month's content
	  (goto-char month-end)
	  (insert (format "\n*** %s" day)))))))

(defun diary--now ()
  "Return the current timestamp string."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" "STARTED(s)" "REVIEW(r)" "MAYBE(m)" "DEFERRED(f)" "TODAY(n)")))


;;; === Capture Templates ===

(setq org-capture-templates
      `(("i" "Idea Entry" entry
   (file ,notes-file)
   "* %^{Title} :IDEA:\n:PROPERTIES: \n:ID:    %(org-id-new)\n:NAME: %\\1\n:TAGS:  \n:KEYWORDS: \n:TIME: %(diary--now)\n:END:\n- Description: %?"
   :empty-lines 1)

  ("u" "Scratch Note " entry
   (file ,notes-file)
   "* %^{Title} :NOTE:\n:PROPERTIES:\n:ID:    %(org-id-new)\n:TOPIC: %\\1\n:TAGS: \n:KEYWORDS: \n:TIME: %(diary--now)\n:END:\n- Description: %?"
   :empty-lines 1)

  ("h" "Log Time" entry (file+datetree,log-file )
	 "* %? \n" :clock-in t :clock-keep t :clock-resume t)

  ("j" "Journal " plain ; Use 'plain' type to insert text directly
   (file+function ,journal-file
      (lambda ()
	;; Navigate to the end of today's entry
	(let ((day-heading (format-time-string "^\\*\\*\\* %Y-%m-%d")))
	  (goto-char (point-min))
	  (re-search-forward day-heading nil t)
	  (org-end-of-subtree))))
   ;; This template creates a list item instead of a new headline
   "**** %<%I:%M %p>:\n - %? \n - [[%F][source]]")))

(defun diary--entry-date (time-string)
  "Extract date (YYYY-MM-DD) from a TIME property string like [2025-05-05 Mon 18:00]."
  (when (string-match "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" time-string)
    (match-string 1 time-string)))


(advice-add 'org-capture :before
      (lambda (&rest _)
	(with-current-buffer (find-file-noselect journal-file)
    (ensure-date-tree))))

(defun notes-rebuild-index ()
  "Rebuild the notes index file with a Year->Month->Week structure."
  (interactive)
  (let ((notes-by-date (make-hash-table :test 'equal)))
    ;; 1. Collect all notes and group them by Y/M/W in a hash table
    (with-current-buffer (find-file-noselect notes-file)
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
	(year-ht   (or (gethash year notes-by-date) (make-hash-table :test 'equal)))
	(month-ht  (or (gethash month year-ht) (make-hash-table :test 'equal)))
	(week-list (or (gethash week month-ht) '())))
	 (setf (gethash week month-ht) (cons (list headline id) week-list))
	 (setf (gethash month year-ht) month-ht)
	 (setf (gethash year notes-by-date) year-ht)))))
       "NOTE|IDEA" 'file))

    (with-temp-file index-file
      (insert "#+TITLE: Notes and Ideas Index\n\n")
      (dolist (year (cl-sort (hash-table-keys notes-by-date) #'string<))
  (insert (format "* %s\n" year))
  (let ((year-ht (gethash year notes-by-date)))
    (dolist (month (cl-sort (hash-table-keys year-ht) #'string<))
      ;; This is the line that was fixed
      (let* ((month-name (format-time-string "%B" (encode-time 0 0 0 1 (string-to-number month) (string-to-number year))))
       (month-ht (gethash month year-ht)))
	(insert (format "** %s-%s %s\n" year month month-name))
	(dolist (week (cl-sort (hash-table-keys month-ht) #'string<))
    (let ((note-list (gethash week month-ht)))
      (insert (format "*** Week %s\n" week))
      (dolist (item (reverse note-list))
	(insert (format "- [[id:%s][%s]]\n" (cadr item) (car item))))))))))))
  (message "Hierarchical notes index rebuilt successfully."))

;; <<< NEW function to open the index file
(defun notes-open-index-file ()
  "Open the notes index file."
  (interactive)
  (find-file index-file))

(defun logbook-open-file ()
  "Open the logbook  file."
  (interactive)
  (find-file log-file))


(defun open-diary-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file diary-file))

(defun open-project-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file projects-file))


(defun open-shortcut-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file shortcut-file))


(defun open-journal-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file journal-file)
  (ensure-date-tree)
  (let ((day-heading (format-time-string "^\\*\\*\\* %Y-%m-%d")))
    (goto-char (point-min))
    (re-search-forward day-heading nil t)))

(defun open-single-buffer ()
  "Open current org heading in indirect buffer and close all other windows."
  (interactive)
  ;; Create indirect buffer with current subtree
  (org-tree-to-indirect-buffer)
  (other-window 1)
  (delete-other-windows)
  (end-of-buffer 1)
  (recenter-top-bottom t)
  ;; Close all other windows (buffers remain available in background)
  )

;;; === Keybindings ===
(global-set-key (kbd "C-c i") (lambda () (interactive) (org-capture nil "i")))  ;; New Idea
(global-set-key (kbd "C-c j") (lambda () (interactive) (org-capture nil "j")))  ;; New Journal
(global-set-key (kbd "C-c h") (lambda () (interactive) (org-capture nil "h")))  ;; New Log Entry
(global-set-key (kbd "C-c n") (lambda () (interactive) (org-capture nil "u")))  ;; New Note

(global-set-key (kbd "C-c o j") 'open-journal-file)
(global-set-key (kbd "C-c o d") 'open-diary-file)
(global-set-key (kbd "C-c o h") 'logbook-open-file)
(global-set-key (kbd "C-c o p") 'open-project-file)
(global-set-key (kbd "C-c o s") 'open-shortcut-file)


(global-set-key (kbd "C-c b n") #'notes-rebuild-index) ; "Notes - Rebuild"
(global-set-key (kbd "C-c o n") #'notes-open-index-file) ; "Notes - Open"


(global-set-key (kbd "C-x i") #'org-clock-in) ; "clock in "
(global-set-key (kbd "C-x j") #'org-clock-out) ; "clock out"

(global-set-key (kbd "C-c o b") 'open-single-buffer);;open a heading in a new buffer


(setq org-agenda-files
      '("~/Stillness/Personal/Writings/Tasks.org"
	"~/Stillness/Personal/Writings/RoughNotes.org"))


(provide 'flat-diary-config)
