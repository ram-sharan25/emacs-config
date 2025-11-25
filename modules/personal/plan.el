;;; project-tasks.el --- A system for capturing tasks linked to projects -*- lexical-binding: t; -*-

(require 'paths)
(add-to-list 'load-path "~/.emacs.d/modules/git-modules/org-timeblock/")
;;; --- 1. File Paths ---

(defvar my/projects-file my/projects-file)
(defvar my/tasks-file my/tasks-file)
(defvar my/diary-file my/diary-file)
(defvar my/google-calendar-file my/gcal-file)
;;; --- 2. Packages ---

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode))

;;; --- 3. Helper Functions to Read Project Names ---

(defun my/get-area-names ()
  "Return a list of Area names (filenames without extension) from `my/areas-dir`."
  (let ((files (directory-files my/areas-dir nil "\\.org$")))
    (mapcar #'file-name-sans-extension files)))

(defun my/get-area-id-by-name (area-name)
  "Return the ID of the Area file corresponding to AREA-NAME.
Creates the ID if it doesn't exist."
  (let ((file (expand-file-name (concat area-name ".org") my/areas-dir)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
	(org-id-get-create)))))

(defun my/org-get-project-headings (&optional area-filter)
  "Return an alist of (Project Name . ID) from `my/projects-file` with ACTIVE=TRUE.
If AREA-FILTER is provided, only include projects with a matching :AREA: property."
  (with-current-buffer (find-file-noselect my/projects-file)
    (let ((projects '()))
      (org-map-entries
       (lambda ()
	 (let* ((heading (org-get-heading t t))
		(id (org-id-get-create))
		(area (org-entry-get nil "AREA")))
	   ;; Filter by Area if provided
	   (when (or (null area-filter)
		     (string= area area-filter))
	     (push (cons heading id) projects))))
       "LEVEL=1+ACTIVE=\"TRUE\"" 'file)
      (nreverse projects))))

(defun my/select-area-default-misc ()
  "Prompt for Area, defaulting to 'Misc' and showing it first."
  (let* ((areas (my/get-area-names))
	 ;; Ensure Misc is first and unique
	 (options (cons "Misc" (remove "Misc" areas)))
	 (selected (completing-read "Area: " options nil t nil nil "Misc")))
    (if (string-empty-p selected) "Misc" selected)))

(defun my/org-select-project-allow-empty (&optional area-filter)
  "Prompt user to select a project, optionally filtered by AREA-FILTER.
Returns a cons cell (Name . ID). Includes 'Dump' as the first option."
  (let* ((project-alist (my/org-get-project-headings area-filter))
	 ;; Prepend "Dump" option explicitly so it appears first
	 (options (cons '("Dump" . nil) project-alist))
	 (project-names (mapcar #'car options))
	 (selected-name (completing-read "Select Project: "
					 project-names nil t nil nil "Dump")))
    (if (string-empty-p selected-name)
	'("Dump" . nil)
      (assoc selected-name options))))

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
	    (let* ((area-name (my/select-area-default-misc))
		   ;; Select Project (filtered by Area) - returns (Name . ID)
		   (project-cons (my/org-select-project-allow-empty area-name))
		   (project-name (car project-cons))
		   (project-id (cdr project-cons))
		   template-string)

	      ;; Select Template based on Project
	      (if (equal project-name "Dump")
		  ;; Simple Template for Dump
		  (setq template-string "* TODO %%^{Task Title}\n:PROPERTIES:\n:AREA: %s\n:PROJECT: %s\n:ID: %%(org-id-new)\n:CREATED: %%U\n:END:\n")
		;; Complex Template for Projects
		;; We construct the Project Link: [[id:ID][Name]]
		(let ((project-link (format "[[id:%s][%s]]" project-id project-name)))
		  (setq template-string
			(concat "* TODO %%^{Task Title}\n"
				":PROPERTIES:\n"
				":AREA: %s\n"
				":PROJECT: %s\n"
				":ID: %%(org-id-new)\n"
				":CREATED: %%U\n"
				":END:\n"
				":THOUGHTS:\n"
				"- %%? \n"
				":END:\n\n"
				"** Sub-tasks\n- [ ] \n\n"
				"** Resources\n"
				"- Project: " project-link "\n"
				"- Area: [[id:%s][%s]]\n"))))

	      ;; Return formatted string
	      (format template-string area-name project-name (my/get-area-id-by-name area-name) area-name))))
	 :empty-lines 1))

(add-to-list 'org-capture-templates
       '("q" "New Project" entry
	 (file my/projects-file)
	 (function
	  (lambda ()
	    (let ((area-name (completing-read "Area (default Misc): " (my/get-area-names) nil t)))
	      (when (string-empty-p area-name)
		(setq area-name "Misc"))
	      (format
	       "* %%^{Project Name}\n:PROPERTIES:\n:AREA: %s\n:ACTIVE: TRUE\n:ID: %%(org-id-new)\n:CREATED: %%U\n:END:\n\n** Description\n- %%?\n"
	       area-name))))
	 :empty-lines 1))

;;; --- 7. Keybinding ---




(global-set-key (kbd "C-c t") (lambda () (interactive) (org-capture nil "p")))
(global-set-key (kbd "C-c q") (lambda () (interactive) (org-capture nil "q")))

(use-package org-timeblock
  :load-path  "~/.emacs.d/modules/git-modules/org-timeblock/"
  :config
  (setq org-timeblock-span 1)              ;; Show 1 Day
  (setq org-timeblock-day-start-hour 6)    ;; Start at 7 AM (Hide 0-6 AM)
  (setq org-timeblock-day-end-hour 23)
  (setq org-timeblock-scale 0.8)          ;; Zoom out (Fit day on screen)
  (setq org-timeblock-inbox-file my/tasks-file)
  ;; 2. GRID SETTINGS
  (setq org-timeblock-show-future-repeats t)
  (setq org-timeblock-time-grid-step 60))

(defun rsr/org-timeblock-split-view ()
  "Open Org Timeblock on the left and the daily list on the right."
  (interactive)
  ;; Open the standard grid
  (org-timeblock)
  ;; Remove other windows to clean up
  (delete-other-windows)
  ;; Split the screen horizontally
  (split-window-right)
  ;; Move to the right window
  (other-window 1)
  ;; Switch to the "List View" of the current timeblock
  (org-timeblock-list))

(defun my-org-clock-on-state-change ()
  "Clock in/out when TODO state changes to/from 'IN PROGRESS'.
  This function checks `org-state' and `org-last-state'."

  ;; 1. Clock IN when moving TO "IN PROGRESS"
  (when (string= org-state "IN-PROGRESS")
    ;; We removed (unless (org-clock-is-active)) so it ALWAYS clocks in
    (org-clock-in))

  ;; 2. Clock OUT when moving FROM "IN PROGRESS" to anything else
  (when (and (string= org-last-state "IN-PROGRESS")
	     (not (string= org-state "IN-PROGRESS")))
    (when (org-clock-is-active)
      (org-clock-out))))

(add-hook 'org-after-todo-state-change-hook 'my-org-clock-on-state-change)


(defun my/org-agenda-project-suffix ()
  "Format as [Project]:Category, pad to fixed width, and hide 'nil' or '???'."
  (let* ((cat (org-get-category))
	 (width 40))

    ;; 1. FIRST, check if it's a Time Grid line ("???") or empty
    (if (or (null cat)
	    (string= (format "%s" cat) "nil")
	    (string= (format "%s" cat) "???")) ;; <--- This catches the 8:00 lines

	;; If it is Time Grid, just print whitespace and STOP.
	(make-string width ?\s)

      ;; 2. ONLY NOW is it safe to look for the project property
      ;; We wrap it in ignore-errors just in case
      (let* ((project (ignore-errors (org-entry-get nil "PROJECT")))
	     (output-str
	      (if project
		  (concat (propertize (format "[%s]" project)
				      'face '(:foreground "orange" :weight bold))
			  ":" cat)
		(format "%s:" cat))))

	;; Pad result to fixed width
	(format (format "%%-%ds" width) output-str)))))

;; Re-apply the setting
(setq org-agenda-prefix-format
      '((agenda . " %i %(my/org-agenda-project-suffix) %?-12t% s")
	(todo   . " %i %(my/org-agenda-project-suffix) ")
	(tags   . " %i %(my/org-agenda-project-suffix) ")
	(search . " %i %(my/org-agenda-project-suffix) ")))

(setq org-agenda-custom-commands
      '(("p" "Projects Dashboard" alltodo ""
	 (
	  ;; 1. Configure Super Agenda to group by the "PROJECT" property
	  (org-super-agenda-groups
	   '((:auto-property "PROJECT")
	     (:auto-todo t)))
	  (org-agenda-prefix-format
	   '((todo . "  %-12:c %?-12t% s")))
	  ))
      ("q" "Area Dashboard" alltodo ""
	 (
	  ;; 1. Configure Super Agenda to group by the "PROJECT" property
	  (org-super-agenda-groups
	   '((:auto-property "AREA")
	     (:auto-todo t)))
	  (org-agenda-prefix-format
	   '((todo . "  %-12:c %?-12t% s")))
	  ))))

(setq org-agenda-span 'day)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c d") 'rsr/org-timeblock-split-view)

;;; --- 8. Workflow Configuration ---

;; Archiving
(setq org-archive-location (concat my/archive-dir "%s_archive.org::"))

;; Agenda Files
(setq org-agenda-files (list my/tasks-file my/projects-file
			     my/google-calendar-file my/gtasks-dir))

;; Refiling
(setq org-refile-targets
      '((my/projects-file :maxlevel . 1)
	(my/tasks-file :maxlevel . 1)
	(my/areas-dir :maxlevel . 1)))

(provide 'project-tasks-config)
