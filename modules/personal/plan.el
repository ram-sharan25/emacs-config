;;; project-tasks.el --- A system for capturing tasks linked to projects -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'org-id)

;;; --- 1. File Paths ---

(defvar my/projects-file "~/Stillness/Personal/Writings/Projects.org"
  "The file containing high-level project definitions.")

(defvar my/tasks-file "~/Stillness/Personal/Writings/Tasks.org"
  "The file where all daily, actionable tasks will be captured.")
(defvar my/diary-file "~/Stillness/Personal/Writings/Diary.org"
  "The file where all daily, actionable tasks will be captured.")

;;; --- 2. Packages ---

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode))

;;; --- 3. Helper Functions to Read Project Names ---

(defun my/org-get-project-headings ()
  "Return a list of all level-1 headings from `my/projects-file`."
  (with-current-buffer (find-file-noselect my/projects-file)
    (let ((headings '()))
      (org-map-entries
       (lambda ()
	 (push (org-get-heading t t) headings))
       "LEVEL=1" 'file)
      (nreverse headings))))

(defun my/org-select-project-allow-empty ()
  "Prompt user to select a project, allowing empty input."
  (let ((project-list (my/org-get-project-headings)))
    (completing-read "Select Project (or leave empty for Dump): "
		     project-list nil t)))

;;; --- 4. TODO Keywords ---

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)"
		  "CANCELED(c)"  "REVIEW(r)" "MAYBE(m)"
		  "DEFERRED(f)" )))

;;; --- 5. Shared Status Groups ---

(defvar my/org-super-agenda-status-groups
  '((:name "TODO"        :todo "TODO"        :order 1)
    (:name "IN-PROGRESS" :todo "IN-PROGRESS" :order 2)
    (:name "WAITING"     :todo "WAITING"     :order 3)
    (:name "REVIEW"      :todo "REVIEW"      :order 5)
    (:name "MAYBE"       :todo "MAYBE"       :order 6)
    (:name "DEFERRED"    :todo "DEFERRED"    :order 7)
    (:name "DONE"        :todo "DONE"        :order 9)
    (:name "CANCELED"    :todo "CANCELED"    :order 10)
    (:discard (:anything t)))
  "Reusable org-super-agenda groups by TODO keyword.")

;;; --- 6. Capture Template ---

(add-to-list 'org-capture-templates
	     '("p" "Project Task" entry
	       (file my/tasks-file)
	       (function
		(lambda ()
		  (let (project-name goal-link-string)
		    ;; Check if we are capturing from a project heading
		    (if (and (buffer-file-name)
			     (equal (expand-file-name (buffer-file-name))
				    (expand-file-name my/projects-file))
			     (org-at-heading-p))
			;; Context: from a Project heading
			(progn
			  (setq project-name (org-get-heading t t))
			  (setq goal-link-string
				(concat "** Goal: " (org-store-link nil) "\n")))
		      ;; Context: anywhere else
		      (let ((selected (my/org-select-project-allow-empty)))
			(setq project-name (if (string-empty-p selected)
					       "Dump" selected))
			(setq goal-link-string "")))
		    ;; Final template string
		    (format
"* TODO %%^{Task Title}\n:PROPERTIES:\n:PROJECT: %s\n:ID: %%(org-id-new)\n:CREATED:  %%U\n:END:\n  \n** Description\n- %%? \n\n** Walkthrough\n- [ ] "
		     project-name
		     goal-link-string))))

	       :empty-lines 1))

;;; --- 7. Keybinding ---

(global-set-key (kbd "C-c t") (lambda () (interactive) (org-capture nil "p")))
;;; --- 8. Agenda Views ---

;; Set agenda files
(setq org-agenda-files (list my/projects-file my/tasks-file my/diary-file))

;; Use org-super-agenda's built-in auto-grouping features
(setq org-super-agenda-groups
      '(;; Group by project property automatically
	(:auto-property "PROJECT")
	;; Then group remaining by TODO keyword
	(:auto-todo t)))

(defun my/org-agenda-project-status-hierarchy ()
  "Create a custom agenda view showing tasks grouped by project, then by status."
  (interactive)
  (let ((buffer-name "*Org Project-Status Hierarchy*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Tasks by Project and Status\n")
      ;; Get all TODO entries from agenda files
      (let ((entries '())
	    (project-status-map (make-hash-table :test 'equal)))

	;; Collect all entries
	(org-map-entries
	 (lambda ()
	   (let* ((heading (org-get-heading t t t t))
		  (todo-state (org-get-todo-state))
		  (project (org-entry-get (point) "PROJECT"))
		  (file (buffer-file-name))
		  (pos (point))
		  (entry (list heading todo-state project file pos)))
	     (when todo-state
	       (push entry entries))))
	 nil org-agenda-files)

	;; Group entries by project and status
	(dolist (entry entries)
	  (let* ((heading (nth 0 entry))
		 (todo-state (nth 1 entry))
		 (project (or (nth 2 entry) "No Project"))
		 (key (cons project todo-state)))
	    (if (gethash key project-status-map)
		(push entry (gethash key project-status-map))
	      (puthash key (list entry) project-status-map))))

	;; Sort and display
	(let ((project-list '()))
	  ;; Collect all unique projects
	  (maphash (lambda (key _value)
		     (let ((project (car key)))
		       (unless (member project project-list)
			 (push project project-list))))
		   project-status-map)
	  (setq project-list (sort project-list 'string<))

	  ;; Display each project with its statuses
	  (dolist (project project-list)
	    (insert (format "* PROJECT: %s\n" project))

	    ;; Sort statuses within project
	    (let ((status-list '()))
	      (maphash (lambda (key _value)
			 (when (string= (car key) project)
			   (push (cdr key) status-list)))
		       project-status-map)
	      (setq status-list (sort status-list 'string<))

	      ;; Display each status with its tasks
	      (dolist (status status-list)
		(let ((key (cons project status))
		      (tasks (gethash (cons project status) project-status-map)))
		  (when tasks
		    (insert (format "** %s:\n" status))
		    (dolist (task (sort tasks (lambda (a b) (string< (car a) (car b)))))
		      (let* ((heading (nth 0 task))
			     (file (nth 3 task))
			     (pos (nth 4 task))
			     ;; Create a clickable link
			     (link (format "[[file:%s::%d][%s]]" file pos heading)))
			(insert (format "   - %s\n" link)))))))
	    (insert "\n"))))

      ;; Enable link following and make buffer read-only for safety
      (goto-char (point-min))
      (setq buffer-read-only t)
      (local-set-key (kbd "RET") 'org-open-at-point)
      (local-set-key (kbd "<mouse-1>") 'org-open-at-point)
      (local-set-key (kbd "r") 'my/refresh-project-hierarchy)
      (pop-to-buffer buffer-name)))))

(defun my/refresh-project-hierarchy ()
  "Refresh the project hierarchy view."
  (interactive)
  (when (string= (buffer-name) "*Org Project-Status Hierarchy*")
    (setq buffer-read-only nil)
    (my/org-agenda-project-status-hierarchy)))
;; Functions to quickly switch grouping modes
(defun my/agenda-group-by-project ()
  "Group agenda by project, showing ALL tasks."
  (interactive)
  (setq org-super-agenda-groups my/agenda-group-by-project)
  (setq org-agenda-skip-function nil)  ; Show all tasks
  (org-agenda-redo-all))

(defun my/agenda-group-by-status ()
  "Group agenda by status, showing ALL tasks."
  (interactive)
  (setq org-super-agenda-groups my/agenda-group-by-status)
  (setq org-agenda-skip-function nil)  ; Show all tasks
  (org-agenda-redo-all))

(defun my/agenda-group-by-both ()
  "Group agenda by project then status, showing ALL tasks."
  (interactive)
  (setq org-super-agenda-groups my/agenda-group-by-both)
  (setq org-agenda-skip-function nil)  ; Show all tasks
  (org-agenda-redo-all))
;; Custom agenda commands using built-in features
(setq org-agenda-custom-commands
      '(("p" "By Project" alltodo ""
	 ((org-super-agenda-groups '((:auto-property "PROJECT")))))

	("s" "By Status" alltodo ""
	 ((org-super-agenda-groups '((:auto-todo t)))))

	("b" "By Project & Status" alltodo ""
	 ((org-super-agenda-groups '((:auto-property "PROJECT")
				     (:auto-todo t)))))

	("d" "Daily" agenda ""
	 ((org-agenda-span 'day)
	  (org-super-agenda-groups '((:name "Today"
				      :time-grid t
				      :scheduled today)
				     (:auto-property "PROJECT")))))))


(global-set-key (kbd "C-c a") 'org-agenda)


;;; --- End of Configuration ---
(provide 'project-tasks-config)
