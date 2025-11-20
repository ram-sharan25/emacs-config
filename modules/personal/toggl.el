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

  (defun rsr/toggl-clock-in-hook ()
  "Starts Toggl. Uses a combination of capture mode and project name for fixed description."
  (when (derived-mode-p 'org-mode)
    (let* ((heading (org-get-heading t t t t))
           (prop-project (org-entry-get (point) "PROJECT" t))

           ;; 🔑 NEW CHECK: Are we currently capturing AND is the project 'Habits'?
           (is-journal-capture-active (and org-capture-mode
                                           (string-equal prop-project "Habits")))

           (final-desc nil)
           (final-project-id nil))

      (if is-journal-capture-active
          ;; --- PATH 1: JOURNAL CAPTURE (Fixed Name, Fixed Project) ---
          (let ((project-name "Habits"))
            (setq final-desc "Daily Journal Entry")
            (setq final-project-id (cdr (assoc project-name toggl-projects)))

            (unless final-project-id
              (message "ERROR: Project '%s' not found in Toggl cache. Refresh projects!" project-name)))

        ;; --- PATH 2: REGULAR CLOCK-IN (Standard Prompt Logic) ---
        (let* (;; Get raw input from user (e.g., what they type, or "" if they hit Enter)
               (raw-input (read-string (format "Task (default: %s): " heading)))

               ;; Determine Project Choice (use property or prompt)
               (project-choice (if prop-project
                                   prop-project
                                 (completing-read "Select Toggl Project: " toggl-projects))))

          ;; Set Final Description (This is where the time heading comes from if raw-input is empty)
          (setq final-desc
                (if (string-equal raw-input "")
                    heading
                  raw-input))

          ;; Set the Final Project ID
          (setq final-project-id (cdr (assoc project-choice toggl-projects)))))

      ;; --- START TIMER ---
      (if final-project-id
          (toggl-start-time-entry final-desc final-project-id)
        (message "Warning: Starting Toggl without a project.")
        (toggl-start-time-entry final-desc)))))

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
