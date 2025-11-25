;;; toggl.el --- Toggl configuration -*- lexical-binding: t; -*-

;; 1. Define Local Path
(add-to-list 'load-path "~/.emacs.d/modules/git-modules/org-toggle/")

(use-package org-toggl
  :ensure nil
  :after org

  :init
  ;; --- CREDENTIALS ---
  (setq toggl-auth-token my/toggl-auth-token)
  (setq toggl-workspace-id 8843824)

  ;; --- INHERITANCE ---
  ;; Ensures :PROJECT: property is read from parent headings
  (setq org-use-property-inheritance '("PROJECT"))

  :config
  (require 'org-toggl)
  (require 'json)
  (require 'url)

  ;; --- 1. PROJECT DATA MANAGEMENT ---

  (defvar toggl-projects nil
    "A list of (ProjectName . ProjectID) used for completion.")

  (defun rsr/update-toggl-projects ()
    "Fetch ACTIVE projects from Toggl and update `toggl-projects`."
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

          ;; Filter out archived projects (delq nil removes empty slots)
          (setq toggl-projects
                (delq nil
                      (mapcar (lambda (item)
                                (if (not (eq (alist-get 'active item) :json-false))
                                    (cons (alist-get 'name item) (alist-get 'id item))
                                  nil))
                              (append projects-vector nil))))
          (message "Synced %d active projects." (length toggl-projects))))))

  (with-eval-after-load 'org-toggl
  (message "Toggl: Running project cache update...")
  ;; Call the function we defined to fetch and populate `toggl-projects`.
  (rsr/update-toggl-projects))

  ;; --- 2. OVERRIDE: Support Tags in Time Entry ---
  (defun toggl-start-time-entry (description &optional pid tags show-message)
    "Start Toggl time entry with optional PID and TAGS."
    (interactive "MDescription: \ni\np")
    (setq pid (or pid toggl-default-project))
    (toggl-request-post
     (format "workspaces/%s/time_entries" toggl-workspace-id)
     (json-encode `(("description" . ,description)
                    ("duration" . -1)
                    ("project_id" . ,pid)
                    ("tags" . ,(or tags [])) ; Send tags array
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
    "Starts Toggl. Maps AREA -> Project. Sends PROJECT + Tags as Toggl Tags."
    (when (derived-mode-p 'org-mode)
      (let* ((heading (org-get-heading t t t t))
             ;; 1. Get Metadata
             (prop-area (org-entry-get (point) "AREA" t))     ; Maps to Toggl Project
             (prop-project (org-entry-get (point) "PROJECT" t)) ; Maps to Toggl Tag
             (org-tags (org-get-tags))                        ; Org Tags -> Toggl Tags

             ;; 2. Construct Tag List (Sanitized)
             (project-tag (when prop-project
                            (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" 
                                                      (replace-regexp-in-string " " "_" prop-project))))
             (final-tags (append org-tags 
                                 (when project-tag (list project-tag))))

             ;; 3. Check for Journal Capture
             (is-journal-capture-active (and org-capture-mode
                                             (string-equal prop-project "Habits")))

             (final-desc nil)
             (final-project-id nil))

        (if is-journal-capture-active
            ;; --- PATH 1: JOURNAL CAPTURE ---
            (let ((project-name "Habits"))
              (setq final-desc "Daily Journal Entry")
              (setq final-project-id (cdr (assoc project-name toggl-projects)))
              (unless final-project-id
                (message "ERROR: Project '%s' not found in Toggl cache." project-name)))

          ;; --- PATH 2: REGULAR CLOCK-IN ---
          (let* ((raw-input (read-string (format "Task (default: %s): " heading)))
                 ;; Use AREA for Toggl Project selection
                 (project-choice (if prop-area
                                     prop-area
                                   (completing-read "Select Toggl Project (Area): " toggl-projects))))

            (setq final-desc (if (string-equal raw-input "") heading raw-input))
            (setq final-project-id (cdr (assoc project-choice toggl-projects)))))

        ;; --- START TIMER ---
        (if final-project-id
            (toggl-start-time-entry final-desc final-project-id final-tags t)
          (message "Warning: Starting Toggl without a project.")
          (toggl-start-time-entry final-desc nil final-tags t)))))

  ;; --- 3. HOOK ASSIGNMENTS ---
  :hook
  (org-clock-in . rsr/toggl-clock-in-hook)
  (org-clock-out . org-toggl-clock-out)
  )

;; --- GLOBAL KEYBINDINGS ---
(global-set-key (kbd "C-c c i") #'org-clock-in)
(global-set-key (kbd "C-c c o") #'org-clock-out)
(global-set-key (kbd "C-c c g") #'org-clock-goto)
;; Optional: Refresh projects manually if you add a new one on the web
;; (global-set-key (kbd "C-c t r") #'rsr/update-toggl-projects)
