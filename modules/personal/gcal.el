(require 'epa-file)
(epa-file-enable)
(add-to-list 'load-path "~/.emacs.d/modules/git-modules/org-gtasks/")
(load "~/.emacs.d/secrets.el.gpg")
(setq epg-pinentry-mode 'loopback)
(require 'paths) ; Load standardized path variables
(require 'plan)
(setq google-task-file my/gtasks-file) ; Use standardized variable

(use-package org-gcal
  :ensure t
  :init
  (setq plstore-encrypt-to nil)
  :custom
  (org-gcal-client-id my/google-client-id)
  (org-gcal-client-secret my/google-client-secret)
  (org-gcal-down-days 365)
  (org-gcal-up-days 365)
  (org-gcal-fetch-file-alist
   `(("primary" . ,my/gcal-file))))

(use-package org-gtasks
  :ensure nil
  :load-path "~/.emacs.d/modules/git-modules/org-gtasks/"
  :after my-secrets
  :config
  (org-gtasks-register-account :name "Perso"
	     :directory my/gtasks-dir
	     :login "rimal.ram25@gmail.com"
	     :client-id my/google-client-id
	     :client-secret my/google-client-secret))

(defvar my/is-syncing-now nil
  "Internal flag to prevent recursive syncing loops.")

(defun sync-google-services ()
  "Fetch data from Google Calendar and Google Tasks safely."
  (interactive)

  ;; SAFETY CHECK: Only run if we aren't ALREADY syncing
  (unless my/is-syncing-now
    (let ((my/is-syncing-now t)) ;; Lock the function

      (message "🔄 Google Sync: Starting...")

      ;; --- 1. Sync Calendar ---
      (when (featurep 'org-gcal)
	(message "🔄 Google Calendar: Syncing...")
	(condition-case err
	    (org-gcal-sync)
	  (error (message "❌ Google Calendar Sync Failed: %s" err))))

      ;; --- 2. Sync Tasks ---
      (when (featurep 'org-gtasks)
	(let ((account (org-gtasks-find-account-by-name "Perso")))
	  (when account
	    ;; A. PUSH
	    (message "🔄 Google Tasks: Pushing...")
	    (condition-case err
		(org-gtasks-push account "Tasks") ;; Use "buffer" to be safer/faster than "ALL"
	      (error (message "❌ Google Tasks Push Failed: %s" err)))

	    ;; B. SAVE (Safe because we have the 'my/is-syncing-now' lock)
	    (save-excursion
	      (org-save-all-org-buffers))
	    (sit-for 1)

	    ;; C. PULL
	    (message "🔄 Google Tasks: Pulling...")
	    (condition-case err
		(org-gtasks-pull account "Tasks")
	      (error (message "❌ Google Tasks Pull Failed: %s" err))))))

      (message "✅ Google Sync: Completed."))))

;; --- RECOMMENDED: Run on a Timer (e.g., every 20 mins) ---
;; This is much safer than a save hook.
(run-at-time "5 min" 1200 'sync-google-services)



(defun my-org-gtask-assign-metadata ()
  "Prompt for Area/Project, set properties, and add resource links to the current task."
  (interactive)
  (let* ((area-name (my/select-area-default-misc))
	 (project-cons (my/org-select-project-allow-empty area-name))
	 (project-name (car project-cons))
	 (project-id (cdr project-cons)))

    ;; 1. Set Properties
    (org-entry-put nil "AREA" area-name)
    (org-entry-put nil "PROJECT" project-name)

    ;; 2. Add Resources (if not already present)
    (save-excursion
      (let ((end-pos (org-entry-end-position)))
	(goto-char end-pos)
	;; Check if "Resources" heading already exists in the subtree
	(unless (save-excursion
		  (org-back-to-heading t)
		  (re-search-forward "^\\*+ Resources" end-pos t))
	  (insert "\n** Resources\n")
	  (when project-id
	    (insert (format "- Project: [[id:%s][%s]]\n" project-id project-name)))
	  (insert (format "- Area: [[id:%s][%s]]\n" (my/get-area-id-by-name area-name) area-name)))))

    (message "Assigned Area: %s, Project: %s" area-name project-name)))

(defun my-org-gtask-process ()
  "Move current task to tasks.org.
   If Area/Project are missing, prompt for them first.
   Generates new ID, saves old ID, and marks original DONE."
  (interactive)

  ;; 1. Ensure Metadata Exists
  (unless (org-entry-get nil "AREA")
    (my-org-gtask-assign-metadata))

  (let ((original-google-id (org-entry-get nil "ID"))
	(task-content nil))

    ;; 2. Capture Content
    (save-excursion
      (org-back-to-heading t)
      (let ((beg (point))
	    (end (progn (org-end-of-subtree t t) (point))))
	(setq task-content (buffer-substring beg end))))

    ;; 3. Paste to Destination
    (with-current-buffer (find-file-noselect my/tasks-file)
      (save-excursion
	(goto-char (point-max))
	(insert "\n")
	(let ((paste-start-pos (point)))
	  (insert task-content)
	  (insert "\n")

	  ;; 4. Update New Entry
	  (goto-char paste-start-pos)
	  (org-back-to-heading t)

	  ;; Swap IDs
	  (when original-google-id
	    (org-entry-put nil "gtaskId" original-google-id))
	  (org-entry-put nil "ID" (org-id-new))

	  (save-buffer))))

    ;; 5. Mark Original DONE
    (org-todo "DONE")
    (message "Task processed and moved to %s." (file-name-nondirectory my/tasks-file))))

(defun open-google-tasks-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file my/gtasks-file)) ; Use standardized variable)


(global-set-key (kbd "C-c f") 'my-org-gtask-process)
(global-set-key (kbd "C-c g") 'my-org-gtask-assign-metadata)
(global-set-key (kbd "C-c o f") 'open-google-tasks-file)



;; 2. Set the Timer
;; Format: (run-at-time START-TIME REPEAT-INTERVAL FUNCTION)
;; nil = Start immediately
;; 1800 = Repeat every 1800 seconds (30 minutes)
(run-at-time nil 3600 #'sync-google-services)
(setq plstore-cache-passphrase-for-symmetric-encryption t)
