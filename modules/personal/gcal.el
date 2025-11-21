(require 'epa-file)
(epa-file-enable)
(add-to-list 'load-path "~/.emacs.d/modules/git-modules/org-gtasks/")
(load "~/.emacs.d/secrets.el.gpg")
(setq epg-pinentry-mode 'loopback)
(setq google-task-file "~/Stillness/Personal/Writings/gtasks/GoogleTasks.org")

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
   '(("primary" . "~/Stillness/Personal/Writings/gcal.org"))))

(use-package org-gtasks
  :ensure nil
  :load-path "~/.emacs.d/modules/git-modules/org-gtasks/"
  :after my-secrets
  :config
  (org-gtasks-register-account :name "Perso"
             :directory "~/Stillness/Personal/Writings/gtasks/"
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

(defun my/org-get-project-headings ()
  "Return a list of all level-1 headings from `my/projects-file`."
  (with-current-buffer (find-file-noselect my/projects-file)
    (let ((headings '()))
      (org-map-entries
       (lambda ()
         ;; (org-get-heading t t t t) -> No tags, no todo, no priority, no comment
         (push (org-get-heading t t t t) headings))
       "LEVEL=1" 'file)
      (nreverse headings))))

(defun my-org-process-gtask-and-file ()
  "Copy task, swap old ID to 'gtaskId', generate FRESH Org ID,
   assign Project, and mark original DONE."
  (interactive)

  (let ((selected-project
         (completing-read "Select Project: " (my/org-get-project-headings) nil t))
        ;; 1. Grab the CURRENT ID (from Google Task) before we leave this buffer
        (original-google-id (org-entry-get nil "ID"))
        (task-content nil))

    (when selected-project

      ;; 2. Capture the content (Still TODO, keeping original properties for now)
      (save-excursion
        (org-back-to-heading t)
        (let ((beg (point))
              (end (progn (org-end-of-subtree t t) (point))))
          (setq task-content (buffer-substring beg end))))

      ;; 3. Switch to DESTINATION and Paste
      (with-current-buffer (find-file-noselect my/tasks-file)
        (save-excursion
          (goto-char (point-max))
          (insert "\n")

          ;; Record where we started pasting so we can edit the new entry
          (let ((paste-start-pos (point)))
            (insert task-content)
            (insert "\n")

            ;; Move point to the newly pasted headline to edit its properties
            (goto-char paste-start-pos)
            (org-back-to-heading t)

            ;; --- PROPERTY TRANSFORMATION HAPPENS HERE ---
            ;; A. Set the Project
            (org-entry-put nil "PROJECT" selected-project)

            ;; B. Move the OLD ID to "gtaskId"
            (when original-google-id
              (org-entry-put nil "gtaskId" original-google-id))

            ;; C. Generate a FRESH Org ID for the standard ID property
            ;; This ensures the new task is a unique entity in your Org system
            (org-entry-put nil "ID" (org-id-new)))

          (save-buffer)))

      ;; 4. Mark Original (Google Task) as DONE
      ;; We do this LAST so the copy acts on the "active" version
      (org-todo "DONE")

      (message "Task moved to %s. Old ID saved as gtaskId."
               (file-name-nondirectory my/tasks-file)))))


(defun open-google-tasks-file ()
  "Open daily file and jump to today's entry."
  (interactive)
  (find-file google-task-file))


(global-set-key (kbd "C-c f") 'my-org-process-gtask-and-file)
(global-set-key (kbd "C-c o f") 'open-google-tasks-file)



;; 2. Set the Timer
;; Format: (run-at-time START-TIME REPEAT-INTERVAL FUNCTION)
;; nil = Start immediately
;; 1800 = Repeat every 1800 seconds (30 minutes)
(run-at-time nil 3600 #'sync-google-services)
(setq plstore-cache-passphrase-for-symmetric-encryption t)
