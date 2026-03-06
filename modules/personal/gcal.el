;;; gcal.el --- Google Calendar integration -*- lexical-binding: t; -*-

(require 'epa-file)
(epa-file-enable)
(setq epg-pinentry-mode 'loopback)
(load "~/.emacs.d/secrets.el.gpg")
(require 'paths)

(use-package org-gcal
  :ensure t
  :init
  (setq plstore-encrypt-to nil
        plstore-cache-passphrase-for-symmetric-encryption t)
  :custom
  (org-gcal-client-id my/google-client-id)
  (org-gcal-client-secret my/google-client-secret)
  (org-gcal-down-days 90)
  (org-gcal-up-days 30)
  (org-gcal-fetch-file-alist `(("primary" . ,my/gcal-file))))

(defun my/sync-google-calendar ()
  "Sync Google Calendar."
  (interactive)
  (when (featurep 'org-gcal)
    (condition-case err
        (progn
          (message "Google Calendar: syncing...")
          (org-gcal-sync))
      (error (message "Google Calendar sync failed: %s" err)))))

(defun my/gcal--push (title timestamp)
  "Add an event with TITLE and TIMESTAMP to gcal.org and push it."
  (with-current-buffer (find-file-noselect my/gcal-file)
    (goto-char (point-max))
    (insert (format "\n* %s\n:PROPERTIES:\n:org-gcal-calendar-id: primary\n:END:\n%s\n"
                    title timestamp))
    (save-buffer)
    (org-back-to-heading t)
    (org-gcal-post-at-point))
  (message "Pushed to Google Calendar: %s" title))

(defun my/gcal-push-event ()
  "Prompt for event details and push to Google Calendar."
  (interactive)
  (let* ((title      (read-string "Title: "))
         (date       (org-read-date nil t))
         (start-time (read-string "Start (HH:MM): "))
         (end-time   (read-string "End   (HH:MM): "))
         (timestamp  (format-time-string
                      (concat "<%Y-%m-%d %a " start-time "-" end-time ">")
                      date)))
    (my/gcal--push title timestamp)))

(defun my/gcal-push-task-at-point ()
  "Push the current org task to Google Calendar using its SCHEDULED time.
The task must have a SCHEDULED timestamp with a time range (HH:MM-HH:MM)."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((title     (org-get-heading t t t t))
         (scheduled (org-entry-get nil "SCHEDULED")))
    (unless scheduled
      (user-error "No SCHEDULED timestamp on this heading"))
    (unless (string-match "[0-9]\\{2\\}:[0-9]\\{2\\}-[0-9]\\{2\\}:[0-9]\\{2\\}" scheduled)
      (user-error "SCHEDULED timestamp has no time range (need HH:MM-HH:MM)"))
    (let ((timestamp (replace-regexp-in-string "<\\|>" "" scheduled)))
      (my/gcal--push title (concat "<" timestamp ">")))))

(provide 'gcal)
;;; gcal.el ends here
