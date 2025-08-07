;;; flat-diary-config.el --- Flat Org diary with tags and properties -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'org-id)

(setq personal-dir "~/Stillness/Personal/Writings/")
 ;;"Base directory for personal writings."
(setq diary-file (concat personal-dir "Diary.org"))
(setq journal-file (concat personal-dir "Journal.org"))
(setq notes-file (concat personal-dir "RoughNotes.org"))
(setq index-file (concat personal-dir "NotesIndex.org"))
(setq todo-index-file (concat personal-dir "TodoIndex.org"))

(defun ensure-date-tree ()
  "Create the year/month/day heading structure for today if it doesn't exist."
  (let ((year (format-time-string "%Y"))
	(month (format-time-string "%Y-%m %B"))
	(day (format-time-string "%Y-%m-%d %A")))
    (save-excursion
      (goto-char (point-min))
      ;; Find or create Year headline
      (unless (re-search-forward (format "^\\* %s$" year) nil t)
	(goto-char (point-max))
	(insert (format "\n* %s" year)))
      ;; Find or create Month headline
      (goto-char (point-min))
      (re-search-forward (format "^\\* %s$" year))
      (unless (re-search-forward (format "^\\** %s$" month) nil t)
	(org-end-of-subtree)
	(insert (format "\n** %s" month)))
      ;; Find or create Day headline
      (goto-char (point-min))
      (re-search-forward (format "^\\* %s$" year))
      (re-search-forward (format "^** %s$" month))
      (unless (re-search-forward (format "^\\*** %s$" day) nil t)
	(org-end-of-subtree)
	(insert (format "\n*** %s" day))))))


(defun diary--now ()
  "Return the current timestamp string."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))


;;; === Capture Templates ===

(setq org-capture-templates
      `(("t" "TODO Entry" entry
	 (file ,diary-file)
	 "* TODO %^{Title} :TODO:\n :PROPERTIES:\n :ID:    %(org-id-new)\n :NAME:  %\\1\n :KEYWORDS: \n :TIME: %(diary--now)\n :END:\n\n- Description: \n  - %?\n\n- TODO:[/]\n  - [ ] \n \n- DONE: \n  - "
	 :empty-lines 1)

	("m" "Meeting Entry" entry
	 (file ,diary-file)
	 "*  %^{Person} :MEETING:\n:PROPERTIES:\n:ID:    %(org-id-new)\n:NAME: %\\1\n:KEYWORDS: \n:TIME: %(diary--now)\n:END:\n- Agenda: %^{Agenda}\n- Discussions:\n  - %?"
	 :empty-lines 1)

	("i" "Idea Entry" entry
	 (file ,notes-file)
	 "* %^{Title} :IDEA:\n:PROPERTIES: \n:ID:    %(org-id-new)\n:NAME: %\\1\n:TAGS:  \n:KEYWORDS: \n:TIME: %(diary--now)\n:END:\n- Description: %?"
	 :empty-lines 1)

	("u" "Scratch Note " entry
	 (file ,notes-file)
	 "* %^{Title} :NOTE:\n:PROPERTIES:\n:ID:    %(org-id-new)\n:TOPIC: %\\1\n:TAGS: \n:KEYWORDS: \n:TIME: %(diary--now)\n:END:\n- Description: %?"
	 :empty-lines 1)

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

;; <<< MODIFIED function to build a hierarchical index with status tags
(defun todos-rebuild-index ()
  "Rebuild the TODO index file, using tags to denote status."
  (interactive)
  (let ((todos-by-date (make-hash-table :test 'equal)))
    ;; 1. Collect all tasks and their statuses, grouping by Y/M/W
    (with-current-buffer (find-file-noselect diary-file)
      (org-map-entries
       (lambda ()
	 (let* ((id (org-entry-get (point) "ID"))
		(headline (org-get-heading t t))
		(status (org-get-todo-state)) ; Get the task's status
		(time-str (org-entry-get (point) "TIME"))
		(time (and time-str (org-time-string-to-seconds time-str))))
	   (when (and id time status)
	     (let* ((year  (format-time-string "%Y" time))
		    (month (format-time-string "%m" time))
		    (week  (format-time-string "%V" time))
		    (year-ht   (or (gethash year todos-by-date) (make-hash-table :test 'equal)))
		    (month-ht  (or (gethash month year-ht) (make-hash-table :test 'equal)))
		    ;; Store the task with its status in a single list
		    (week-list (or (gethash week month-ht) '())))
	       (setf (gethash week month-ht) (cons (list headline id status) week-list))
	       (setf (gethash month year-ht) month-ht)
	       (setf (gethash year todos-by-date) year-ht)))))
       "TODO|IN-PROGRESS|WAITING|DONE|CANCELED" 'file))

    ;; 2. Write the data into the index file, appending a tag for each status
    (with-temp-file todo-index-file
      (insert "#+TITLE: TODO Index by Status\n\n")
      (dolist (year (cl-sort (hash-table-keys todos-by-date) #'string<))
	(insert (format "* %s\n" year))
	(let ((year-ht (gethash year todos-by-date)))
	  (dolist (month (cl-sort (hash-table-keys year-ht) #'string<))
	    (let* ((month-name (format-time-string "%B" (encode-time 0 0 0 1 (string-to-number month) (string-to-number year))))
		   (month-ht (gethash month year-ht)))
	      (insert (format "** %s-%s %s\n" year month month-name))
	      (dolist (week (cl-sort (hash-table-keys month-ht) #'string<))
		(insert (format "*** Week %s\n" week))
		(let ((task-list (gethash week month-ht)))
		  (dolist (item (reverse task-list))
		    (let* ((headline (car item))
			   (id (cadr item))
			   (status (caddr item))
			   ;; Determine the tag based on the status keyword
			   (tag (cond ((string= status "DONE")     "DONE")
				      ((string= status "CANCELED") "CANCELED")
				      ((string= status "WAITING") "WAITING")
				       ((string= status "TODO") "TODO")
				      (t                          "IN-PROGRESS"))))
		      ;; Insert the list item with a neatly aligned tag
		      (insert (format "- [[id:%s][%s]]\t:%s:\n" id headline tag)))))))))))
  (message "Hierarchical TODO index with tags rebuilt successfully.")))

(defun todos-open-index-file ()
  "Open the TODO index file."
  (interactive)
  (find-file todo-index-file))


(defun open-diary-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file diary-file))


(defun open-journal-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file journal-file)
  (ensure-date-tree)
  (let ((day-heading (format-time-string "^\\*\\*\\* %Y-%m-%d")))
    (goto-char (point-min))
    (re-search-forward day-heading nil t)))

;;; === Keybindings ===
(global-set-key (kbd "C-c t") (lambda () (interactive) (org-capture nil "t")))  ;; New TODO
(global-set-key (kbd "C-c m") (lambda () (interactive) (org-capture nil "m")))  ;; New Meeting
(global-set-key (kbd "C-c i") (lambda () (interactive) (org-capture nil "i")))  ;; New Idea
(global-set-key (kbd "C-c j") (lambda () (interactive) (org-capture nil "j")))
(global-set-key (kbd "C-c n") (lambda () (interactive) (org-capture nil "u")))
(global-set-key (kbd "C-c o j") 'open-journal-file)
(global-set-key (kbd "C-c o d") 'open-diary-file)


(global-set-key (kbd "C-c b n") #'notes-rebuild-index) ; "Notes - Rebuild"
(global-set-key (kbd "C-c o n") #'notes-open-index-file) ; "Notes - Open"

(global-set-key (kbd "C-c b t") #'todos-rebuild-index) ; "TODOS - Rebuild"
(global-set-key (kbd "C-c o t") #'todos-open-index-file) ; "TODOS - Open"




(provide 'flat-diary-config)
;;; flat-diary-config.el ends here
