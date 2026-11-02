;;; toggl.el --- Toggl configuration -*- lexical-binding: t; -*-

(require 'paths)

(defun rsr/toggl-tags-at-point ()
  "Toggl tags for the Org entry at point.
The entry's tags with inherited ones (the kind tags from #+FILETAGS, D4),
minus the hubs' note-kind tag \"hub\", then the category (= the project
file name, D5) made Toggl-safe, unless the category is an Area.  A
sitting's `deep' tag rides along here, so Toggl can split deep from
shallow time (Plans/Brain/working-sessions)."
  (let* ((org-tags (remove "hub" (org-get-tags)))
         (category (org-get-category))
         (project-tag (when (and category (not (member category my/area-names)))
                        (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_"
                                                  (replace-regexp-in-string " " "_" category)))))
    (append org-tags (when project-tag (list project-tag)))))

(use-package org-toggl
  :if (bound-and-true-p my/toggl-auth-token)
  :ensure nil
  :after org
  :init
  (setq toggl-auth-token my/toggl-auth-token)
  (setq toggl-workspace-id 8843824)

  :config
  (require 'org-toggl)
  (require 'json)

  (defvar toggl-projects nil
    "A list of (ProjectName . ProjectID) used for completion.")

  (defun rsr/update-toggl-projects ()
    "Fetch active projects from Toggl and update `toggl-projects'.
Asynchronous, through org-toggl's `request' helper, so a slow or dead
network never blocks Emacs; failures are reported in the echo area."
    (interactive)
    (message "Fetching active Toggl projects...")
    (toggl-request-get
     "me?with_related_data=true"
     nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (setq toggl-projects
              (delq nil
                    (mapcar (lambda (item)
                              (unless (eq (alist-get 'active item) :json-false)
                                (cons (alist-get 'name item) (alist-get 'id item))))
                            (append (alist-get 'projects data) nil))))
        (message "Synced %d active projects." (length toggl-projects))))
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (message "Fetching Toggl projects failed: %s" error-thrown)))))

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
Maps the inherited AREA property (a #+PROPERTY: line in every project file
and hub, D3) to the Toggl project, and the inherited kind tags (#+FILETAGS,
D4) plus the category (= project file name, D5) to Toggl tags.  Tags come
from `rsr/toggl-tags-at-point'."
    (when (derived-mode-p 'org-mode)
      (let* ((heading      (org-get-heading t t t t))
             (prop-area    (org-entry-get (point) "AREA" t))
             (final-tags   (rsr/toggl-tags-at-point))
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
  (org-clock-out . org-toggl-clock-out)
  (org-clock-cancel . org-toggl-clock-cancel))

(provide 'toggl)
;;; toggl.el ends here
