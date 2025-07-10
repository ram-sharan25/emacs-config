;;; flat-diary-config.el --- Flat Org diary with tags and properties -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'org-id)

(setq personal-dir "~/Stillness/Personal/Writings/")
 ;;"Base directory for personal writings."
(setq diary-file (concat personal-dir "Diary.org"))
(setq daily-file (concat personal-dir "Journal.org"))





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

;;; === Capture Templates ===

(setq org-capture-templates
      `(("t" "TODO Entry" entry
	 (file ,diary-file)
	 "* TODO %^{Title} :TODO:\n :ID:    %(org-id-new)\n :PROPERTIES:\n:NAME: %\\1\n:TIME: %(diary--now)\n:END:\n- Description: %?\n- DONE:"
	 :empty-lines 1)

	("m" "Meeting Entry" entry
	 (file ,diary-file)
	 "* MEETING with %^{Person} :MEETING:\n:ID:    %(org-id-new)\n :PROPERTIES:\n:NAME: %\\1\n:TIME: %(diary--now)\n:END:\n- Agenda: %^{Agenda}\n- Discussions:\n  - %?"
	 :empty-lines 1)

	("i" "Idea Entry" entry
	 (file ,diary-file)
	 "* %^{Title} :IDEA:\n:ID:    %(org-id-new)\n:PROPERTIES:\n:NAME: %\\1\n:TIME: %(diary--now)\n:END:\n- Description: %?"
	 :empty-lines 1)

	("u" "Scratch Note " entry
	 (file ,diary-file)
	 "* Small Note  %^{Title} :NOTE:\n:ID:    %(org-id-new)\n:PROPERTIES:\n:TOPIC: %\\1\n:TIME: %(diary--now)\n:END:\n- Description: %?"
	 :empty-lines 1)

	("j" "Journal Entry" plain ; Use 'plain' type to insert text directly
	 (file+function ,daily-file
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
  (when (string-match "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" time-string)s
    (match-string 1 time-string)))

(defun diary--collect-entries (&optional only-today)
  "Return an alist of (tag . list-of-entries). If ONLY-TODAY is non-nil, filter by today's date."
  (let ((tags '("TODO" "MEETING"  "IDEA"))
	(entries (make-hash-table :test #'equal))
	(today (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect diary-file)
      (org-map-entries
       (lambda ()
	 (let* ((heading (org-get-heading t t t t))
		(props (org-entry-properties))
		(tag (car (seq-intersection tags (org-get-tags))))
		(time (cdr (assoc "TIME" props)))
		(content (buffer-substring-no-properties
			  (point)
			  (org-end-of-subtree t t))))
	   (when (and tag
		      (or (not only-today)
			  (string= (diary--entry-date time) today)))
	     (push content (gethash tag entries)))))
       nil 'file))
    entries))

(defun diary--display-grouped-entries (entries buffer-name)
  "Display grouped ENTRIES in a new buffer named BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(maphash
	 (lambda (tag items)
	   (insert (format "* %s\n\n" tag))
	   (dolist (entry (reverse items)) ;; show oldest first
	     (insert entry "\n\n")))
	 entries)
	(org-mode)
	(goto-char (point-min))
	(read-only-mode 1)))
    (pop-to-buffer buf)))

(defun diary--today-date ()
  (format-time-string "%Y-%m-%d"))

(defun diary--get-tagged-entries-today ()
  "Return an alist of (tag . parsed-entry) for today's entries."
  (let ((tags '("TODO" "MEETING"  "IDEA"))
	(today (diary--today-date))
	(result (make-hash-table :test #'equal)))
    (with-current-buffer (find-file-noselect diary-file)
      (org-map-entries
       (lambda ()
	 (let* ((entry-point (point))
		(tag (car (seq-intersection tags (org-get-tags))))
		(title (org-get-heading t t t t))
		(props (org-entry-properties))
		(time (cdr (assoc "TIME" props))))
	   (when (and tag time (string-prefix-p today time))
	     (let ((body (buffer-substring-no-properties
			  entry-point
			  (org-end-of-subtree t t))))
	       (push (list :title title
			   :time time
			   :props props
			   :body body)
		     (gethash tag result))))))
       nil 'file))
    result))


;;; === Commands ===

(defun diary-list-entries-for-today ()
  "Show a grouped view of today's entries by tag."
  (interactive)
  (let ((entries (diary--collect-entries t)))
    (diary--display-grouped-entries entries "*Diary: Today*")))

(defun diary-list-all-entries-by-tag ()
  "Show all diary entries grouped by tag."
  (interactive)
  (let ((entries (diary--collect-entries nil)))
    (diary--display-grouped-entries entries "*Diary: All Entries by Tag*")))

(advice-add 'org-capture :before
	    (lambda (&rest _)
	      (with-current-buffer (find-file-noselect daily-file)
		(ensure-date-tree))))


(defun open-daily-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file daily-file)
  (ensure-date-tree)
  (let ((day-heading (format-time-string "^\\*\\*\\* %Y-%m-%d")))
    (goto-char (point-min))
    (re-search-forward day-heading nil t)))



;;; === Keybindings ===

(global-set-key (kbd "C-c t") (lambda () (interactive) (org-capture nil "t")))  ;; New TODO
(global-set-key (kbd "C-c m") (lambda () (interactive) (org-capture nil "m")))  ;; New Meeting
(global-set-key (kbd "C-c i") (lambda () (interactive) (org-capture nil "i")))  ;; New Idea
(global-set-key (kbd "C-c j") (lambda () (interactive) (org-capture nil "j")))
(global-set-key (kbd "C-c u") (lambda () (interactive) (org-capture nil "u")))
(global-set-key (kbd "C-c o d") 'open-daily-file)
(global-set-key (kbd "C-c d") #'diary-list-entries-for-today)
(global-set-key (kbd "C-c l") #'diary-list-all-entries-by-tag)




(provide 'flat-diary-config)
;;; flat-diary-config.el ends here
