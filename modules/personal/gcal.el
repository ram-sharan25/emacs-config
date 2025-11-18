(require 'epa-file)
(epa-file-enable)
(add-to-list 'load-path "~/.emacs.d/modules/git-modules/org-gtasks/")
(load "~/.emacs.d/secrets.el.gpg")
(setq epg-pinentry-mode 'loopback)

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

(defun sync-google-services ()
  "Fetch data from Google Calendar and Google Tasks."
  (interactive)
  (message "🔄 Google Sync: Starting...")

  ;; --- Sync Calendar ---
  (when (featurep 'org-gcal)
    (message "🔄 Google Calendar: Syncing...")
    (condition-case err
        (org-gcal-sync)
      (error (message "❌ Google Calendar Sync Failed: %s" err))))

  ;; --- Sync Tasks ---
  (when (featurep 'org-gtasks)
    (let ((account (org-gtasks-find-account-by-name "Perso")))
      (when account
        (message "🔄 Google Tasks: Syncing 'Perso'...")
        (condition-case err
            (progn
              (org-gtasks-push account "ALL")
              (org-gtasks-pull account "ALL"))
          (error (message "❌ Google Tasks Sync Failed: %s" err))))))

  (message "✅ Google Sync: Completed."));; 2. Set the Timer
;; Format: (run-at-time START-TIME REPEAT-INTERVAL FUNCTION)
;; nil = Start immediately
;; 1800 = Repeat every 1800 seconds (30 minutes)
(run-at-time nil 1800 #'sync-google-services)
(setq plstore-cache-passphrase-for-symmetric-encryption t)
