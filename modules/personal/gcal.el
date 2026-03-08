;;; gcal.el --- Google Calendar integration -*- lexical-binding: t; -*-

(require 'epa-file)
(epa-file-enable)
(setq epg-pinentry-mode 'loopback)
(condition-case err
    (load "~/.emacs.d/secrets.el.gpg")
  (error (message "gcal: failed to load secrets (wrong passphrase?): %s" err)))
(require 'paths)

(use-package org-gcal
  :ensure t
  :init
  (setq plstore-encrypt-to nil
        plstore-cache-passphrase-for-symmetric-encryption t)
  ;; Clear wrong cached passphrase and re-prompt
  (defun rsr/clear-plstore-cache ()
    "Clear cached plstore passphrase so you can re-enter it."
    (interactive)
    (setq plstore--cache (make-hash-table))
    (message "plstore passphrase cache cleared — try your operation again"))
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

(defun my/gcal--push (title timestamp &optional description)
  "Add an event with TITLE, TIMESTAMP, and optional DESCRIPTION to gcal.org and push it."
  (with-current-buffer (find-file-noselect my/gcal-file)
    (goto-char (point-max))
    (insert (format "\n* %s\n:PROPERTIES:\n:org-gcal-calendar-id: primary\n:END:\n%s\n"
                    title timestamp))
    (when (and description (not (string-empty-p description)))
      (insert description "\n"))
    (save-buffer)
    (org-back-to-heading t)
    (org-gcal-post-at-point))
  (message "Pushed to Google Calendar: %s" title))

(defun my/gcal-push-event ()
  "Prompt for event details and push to Google Calendar."
  (interactive)
  (let* ((title       (read-string "Title: "))
         (date        (org-read-date nil t))
         (start-time  (read-string "Start (HH:MM): "))
         (end-time    (read-string "End   (HH:MM): "))
         (description (read-string "Description (optional): "))
         (timestamp   (format-time-string
                       (concat "<%Y-%m-%d %a " start-time "-" end-time ">")
                       date)))
    (my/gcal--push title timestamp description)))

(defun my/gcal-push-task-at-point ()
  "Push the current org task to Google Calendar using its SCHEDULED time.
Uses the task heading as title and body text as description.
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
    (let* ((timestamp   (concat "<" (replace-regexp-in-string "<\\|>" "" scheduled) ">"))
           (description (save-excursion
                          (org-back-to-heading t)
                          (let ((beg (progn (forward-line 1)
                                            (when (looking-at org-property-drawer-re)
                                              (goto-char (match-end 0))
                                              (forward-line 1))
                                            (point)))
                                (end (org-entry-end-position)))
                            (string-trim (buffer-substring-no-properties beg end))))))
      (my/gcal--push title timestamp description))))

;; Auto-sync calendar every 30 minutes when Emacs is idle
(run-with-idle-timer 1800 t #'my/sync-google-calendar)

(provide 'gcal)
;;; gcal.el ends here
