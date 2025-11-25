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

(defun my/org-get-project-headings ()
  "Return a list of all level-1 headings from `my/projects-file` that have the ACTIVE property set to TRUE."
  (with-current-buffer (find-file-noselect my/projects-file)
    (let ((headings '()))
      (org-map-entries
       (lambda ()
   (push (org-get-heading t t) headings))
       "LEVEL=1+ACTIVE=\"TRUE\"" 'file)
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
	 (width 25))

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
	  ))))

(setq org-agenda-span 'day)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c d") 'rsr/org-timeblock-split-view)

(provide 'project-tasks-config)
