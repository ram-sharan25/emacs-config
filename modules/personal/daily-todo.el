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
	 "* TODO %^{Title} :TODO:\n :PROPERTIES:\n :ID:    %(org-id-new)\n:NAME: %\\1\n:TIME: %(diary--now)\n:END:\n- Description: %?\n- DONE:"
	 :empty-lines 1)

	("m" "Meeting Entry" entry
	 (file ,diary-file)
	 "* MEETING with %^{Person} :MEETING:\n:PROPERTIES:\n:ID:    %(org-id-new)\n:NAME: %\\1\n:TIME: %(diary--now)\n:END:\n- Agenda: %^{Agenda}\n- Discussions:\n  - %?"
	 :empty-lines 1)

	("i" "Idea Entry" entry
	 (file ,notes-file)
	 "* %^{Title} :IDEA:\n:PROPERTIES: \n:ID:    %(org-id-new)\n:NAME: %\\1\n:TIME: %(diary--now)\n:END:\n- Description: %?"
	 :empty-lines 1)

	("u" "Scratch Note " entry
	 (file ,notes-file)
	 "* Rough Note  %^{Title} :NOTE:\n:PROPERTIES:\n:ID:    %(org-id-new)\n:TOPIC: %\\1\n:TIME: %(diary--now)\n:END:\n- Description: %?"
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

;; ;; <<< NEW function to build the index
;; (defun notes-rebuild-index ()
;;   "Rebuild the notes index file by scanning all notes and ideas."
;;   (interactive)
;;   (let ((index-entries '()))
;;     ;; Go through the notes file and collect headlines and IDs
;;     (with-current-buffer (find-file-noselect notes-file)
;;       (org-map-entries
;;        (lambda ()
;;	 (let ((id (org-entry-get (point) "ID"))
;;	       (headline (org-get-heading t t)))
;;	   (when id
;;	     (push (list headline id) index-entries))))
;;        "NOTE|IDEA" 'file)) ; Match entries with :NOTE: or :IDEA: tags

;;     ;; Write the collected data to the index file, overwriting it
;;     (with-temp-file index-file
;;       (insert "#+TITLE: Notes and Ideas Index\n\n")
;;       (dolist (item (reverse index-entries)) ; reverse to keep original order
;;	(let ((headline (car item))
;;	      (id (cadr item)))
;;	  (insert (format "- [[id:%s][%s]]\n" id headline)))))
;;     (message "Notes index rebuilt successfully with %d entries." (length index-entries))))

;; <<< MODIFIED function to build the HIERARCHICAL index
;; <<< CORRECTED function to build the HIERARCHICAL index
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

    ;; 2. Write the collected data into the structured index file
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





(defun open-daily-file ()
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
(global-set-key (kbd "C-c u") (lambda () (interactive) (org-capture nil "u")))
(global-set-key (kbd "C-c o d") 'open-daily-file)


(global-set-key (kbd "C-c o r") #'notes-rebuild-index) ; "Notes - Rebuild"
(global-set-key (kbd "C-c o i") #'notes-open-index-file) ; "Notes - Open"




(provide 'flat-diary-config)
;;; flat-diary-config.el ends here
