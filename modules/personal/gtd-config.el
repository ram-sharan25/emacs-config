;;; gtd-config.el --- GTD Workflow Configuration -*- lexical-binding: t; -*-


(require 'paths)
(require 'org-capture)
(defun my/get-area-files ()
  "Get all .org files in the Areas directory."
  (directory-files my/areas-dir t "\\.org$"))

;;; Inbox Capture
;;; Capture tasks directly to the Inbox (tasks.org) without immediate processing.
(add-to-list 'org-capture-templates
             '("i" "Inbox" entry
               (file my/inbox-file)
               "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
               :empty-lines 1))

(add-to-list 'org-capture-templates
             '("q" "New Project" entry
               (file my/gtd-projects-file)
               "* %^{Project Name} [/]\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:COOKIE_DATA: todo recursive\n:CATEGORY: %\\1\n:END:\n- Tags: %?\n\n** Description\n\n** Dashboard\n*** Tasks\n\n*** Notes\n"
               :empty-lines 1))


(setq org-agenda-files (append (list my/inbox-file
                                     my/next-file
                                     my/gtd-projects-file
                                     my/waiting-file
                                     my/someday-file
                                     my/rituals-file
                                     my/gcal-file
                                     my/gtasks-dir)
                               (my/get-area-files)))

(setq org-tag-alist
      '(;; Locations (Where) - No grouping to allow multiple
        ("@home" . ?h)
        ("@office" . ?o)
        ("@library" . ?l)
        ("@errand" . ?e)

        (:startgroup) ;; Work Mode (How)
        ("@deep" . ?d)     ; High focus, demanding tasks
        ("@shallow" . ?s)  ; Low focus, admin, batched tasks
        (:endgroup)

        ;; Activities (What)
        ("dev" . ?c)       ; Coding/Programming
        ("research" . ?r)
        ("study" . ?t)     ; Learning/Studying
        ("writing" . ?w)
        ("admin" . ?a)

        ;; Status
        ("WAITING" . ?W)
        ("SOMEDAY" . ?S)))

;;; GTD Agenda Dashboard
(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode))

(defun my/org-agenda-project-suffix ()
  "Format as [Project]:Category, pad to fixed width, and hide 'nil' or '???'."
  (let* ((cat (org-get-category))
         (width 40))

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

;;; Custom Commands (Merged)

(setq my/gtd-locations
  '(("@home" . "Home")
    ("@office" . "Office")
    ("@library" . "Library")))

(setq my/gtd-modes
  '(("d" "Deep Work" "@deep" "🎯")
    ("s" "Shallow Work" "@shallow" "⚡")))

(defun my/gtd-standard-header ()
  "Return the standard agenda blocks: Day View + In Progress."
  `((agenda ""
            ((org-agenda-span 'day)
             (org-deadline-warning-days 7)
             (org-super-agenda-groups
              '((:name "Today's Schedule"
                       :time-grid t)
                (:name "Scheduled"
                       :and (:scheduled today :todo "TODO"))
                (:name "Deadlines"
                  :deadline t)
                (:name "Overdue"
                       :deadline past
                       :scheduled past)
                (:discard (:anything t))))))
    (todo "STARTED|IN-PROGRESS|WAITING"
          ((org-agenda-overriding-header "In Progress")
           (org-agenda-files (append (list my/gtd-projects-file
                                           my/next-file)
                                   (my/get-area-files)))))))

(defun my/generate-gtd-agenda-commands ()
  "Generate agenda commands for each location and mode."
  (let ((commands '()))
    (dolist (mode my/gtd-modes)
      (let ((mode-key (nth 0 mode))
            (mode-name (nth 1 mode))
            (mode-tag (nth 2 mode))
            (mode-icon (nth 3 mode)))
        ;; Add the main menu item for the mode (e.g., "d" -> "Deep Work Contexts")
        (push (cons mode-key (concat mode-name " Contexts")) commands)

        (dolist (loc my/gtd-locations)
          (let* ((loc-tag (car loc))
                 (loc-name (cdr loc))
                 (key (concat mode-key (substring loc-tag 1 2))) ;; e.g., "dh"
                 (desc (format "%s @ %s" mode-name loc-name))
                 (header (format "%s %s @ %s" mode-icon mode-name loc-name))
                 ;; Strict Tagging: MUST have Location AND Mode tag
                 (tags-query (format "+%s+%s" loc-tag mode-tag)))

            (push (list key desc
                        (append (my/gtd-standard-header)
                                `((tags-todo ,tags-query
                                             ((org-agenda-overriding-header ,header))))))
                  commands)))))
    (nreverse commands)))

(setq org-agenda-custom-commands
      `(("a" "View All (GTD Dashboard)"
         (,@(my/gtd-standard-header)
          (todo "TODO"
                ((org-agenda-overriding-header "To Refile")
                 (org-agenda-files (list my/inbox-file))))
           (todo "WAITING|STARTED"
                ((org-agenda-overriding-header "Waiting")
                 (org-agenda-files (list my/waiting-file))))
          (todo "TODO"
                ((org-agenda-overriding-header "Projects & Areas (Backlog)")
                 (org-agenda-files (append (list my/gtd-projects-file)
                                           (my/get-area-files)))
                 (org-super-agenda-groups '((:auto-category t)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
          (todo "TODO"
                ((org-agenda-overriding-header "One-off Tasks")
                 (org-agenda-files (list my/next-file))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))
         nil)

        ("o" "Only Agenda"
         (,@(my/gtd-standard-header))
         nil)

        ,@(my/generate-gtd-agenda-commands)

        ("m" . "Work Modes (Global)")
        ("md" "Deep Work Mode (All Contexts)"
         ,(append (my/gtd-standard-header)
                  '((tags-todo "+@deep"
                               ((org-agenda-overriding-header " All Deep Work Tasks"))))))
        ("ms" "Shallow Work Mode (All Contexts)"
         ,(append (my/gtd-standard-header)
                  '((tags-todo "+@shallow"
                               ((org-agenda-overriding-header " All Shallow Work Tasks"))))))

        ("p" "Projects Dashboard" alltodo ""
         ((org-agenda-overriding-header "Active Project Tasks")
          (org-super-agenda-groups
           '((:auto-category t)))
          (org-agenda-prefix-format
           '((todo . "  %-12:c %?-12t% s")))))))

(setq org-agenda-span 'day)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-hide-tags-regexp ".")

;; Agenda Files (Updated to include GTD files and Areas)


(setq org-refile-targets
      `((,my/gtd-projects-file :regexp . "Tasks\\|Notes")
        (my/get-area-files :regexp . "Tasks\\|Notes")
        (,my/next-file :maxlevel . 1)
        ;; (,my/someday-file :maxlevel . 0)
        ;; (,my/waiting-file :maxlevel . 0)
        ;; (,my/inbox-file :maxlevel . 0)
        ;; (,my/discarded-file :maxlevel . 0)
        ))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
                         (when (member (buffer-file-name) org-agenda-files)
                           t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (gtd-save-org-buffers)))

(provide 'gtd-config)

;;; Inbox Processing Workflow
(defun my/org-agenda-set-effort ()
  "Set the effort property for the current agenda item."
  (interactive)
  (let* ((completion-ignore-case t)
         (effort (completing-read "Effort: "
                                  '("0:10" "0:30" "1:00" "2:00" "3:00" "4:00"))))
    (org-agenda-check-no-diary)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (inhibit-read-only t)
           newhead)
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (org-entry-put nil "Effort" effort)
          (setq newhead (org-get-heading)))
        (org-agenda-change-all-lines newhead hdmarker)))))

(defun my/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (interactive)
  (call-interactively 'org-agenda-set-tags)
  (call-interactively 'my/org-agenda-set-effort)
  (org-agenda-refile nil nil t))


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
(setq org-archive-location (concat my/archive-dir "%s_archive.org::"))
;;;this to remove the dialog bod of the timer in elisp
(setq org-confirm-elisp-link-function nil)
(global-set-key (kbd "C-c d") 'rsr/org-timeblock-split-view)

(define-key org-agenda-mode-map "j" 'my/org-agenda-process-inbox-item)



(provide 'gtd-config)
;;; gtd-config.el ends here
