;;; toggl.el --- Toggl configuration -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/modules/git-modules/org-toggle/")

(use-package org-toggl
  :ensure nil
  :after org
  :init
  (setq toggl-auth-token my/toggl-auth-token)
  (setq toggl-workspace-id 8843824)
  (setq org-use-property-inheritance '("PROJECT"))

  :config
  (require 'org-toggl)
  (require 'json)
  (require 'url)

  (defvar toggl-projects nil
    "A list of (ProjectName . ProjectID) used for completion.")

  (defun rsr/update-toggl-projects ()
    "Fetch active projects from Toggl and update `toggl-projects`."
    (interactive)
    (let* ((auth (base64-encode-string (concat toggl-auth-token ":api_token") t))
           (url "https://api.track.toggl.com/api/v9/me?with_related_data=true")
           (url-request-extra-headers `(("Authorization" . ,(concat "Basic " auth))
                                        ("Content-Type" . "application/json"))))
      (message "Fetching active Toggl projects...")
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (let* ((full-data (json-read))
               (projects-vector (alist-get 'projects full-data)))
          (kill-buffer (current-buffer))
          (setq toggl-projects
                (delq nil
                      (mapcar (lambda (item)
                                (if (not (eq (alist-get 'active item) :json-false))
                                    (cons (alist-get 'name item) (alist-get 'id item))
                                  nil))
                              (append projects-vector nil))))
          (message "Synced %d active projects." (length toggl-projects))))))

  ;; Fetch projects after 5s of idle — avoids blocking startup
  (run-with-idle-timer 5 nil #'rsr/update-toggl-projects)

  (defun toggl-start-time-entry (description &optional pid tags show-message)
    "Start Toggl time entry with optional PID and TAGS."
    (interactive "MDescription: \ni\np")
    (setq pid (or pid toggl-default-project))
    (toggl-request-post
     (format "workspaces/%s/time_entries" toggl-workspace-id)
     (json-encode `(("description" . ,description)
                    ("duration" . -1)
                    ("project_id" . ,pid)
                    ("tags" . ,(or tags []))
                    ("created_with" . "mbork's Emacs toggl client")
                    ("start" . ,(format-time-string "%FT%TZ" nil t))
                    ("workspace_id" . ,toggl-workspace-id)))
     nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (setq toggl-current-time-entry data)
        (when show-message (message "Toggl time entry started."))))
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (when show-message (message "Starting time entry failed because %s" error-thrown))))))

  (defun rsr/toggl-clock-in-hook ()
    "Start Toggl timer on org-clock-in.
Maps AREA property to Toggl project, PROJECT + org tags to Toggl tags."
    (when (derived-mode-p 'org-mode)
      (let* ((heading      (org-get-heading t t t t))
             (prop-area    (org-entry-get (point) "AREA" t))
             (prop-project (org-entry-get (point) "PROJECT" t))
             (org-tags     (org-get-tags))
             (project-tag  (when prop-project
                             (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_"
                                                       (replace-regexp-in-string " " "_" prop-project))))
             (final-tags   (append org-tags (when project-tag (list project-tag))))
             (is-activity  (org-entry-get (point) "ACTIVITY_TYPE")))
        (let* ((raw-input       (if is-activity
                                    ""
                                  (read-string (format "Task (default: %s): " heading))))
               (project-choice  (if prop-area
                                    prop-area
                                  (completing-read "Select Toggl Project (Area): " toggl-projects))))
          (setq final-desc        (if (string-equal raw-input "") heading raw-input))
          (setq final-project-id  (cdr (assoc project-choice toggl-projects)))
          (setq my/last-toggl-project-choice project-choice))
        (if final-project-id
            (toggl-start-time-entry final-desc final-project-id final-tags t)
          (message "Warning: Starting Toggl without a project.")
          (toggl-start-time-entry final-desc nil final-tags t)))))

  :hook
  (org-clock-in  . rsr/toggl-clock-in-hook)
  (org-clock-out . org-toggl-clock-out))

(provide 'toggl)
;;; toggl.el ends here
