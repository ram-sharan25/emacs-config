;;; gtd-config.el --- GTD Workflow Configuration -*- lexical-binding: t; -*-


(require 'paths)
(require 'org-capture)
(require 'org-id)

(defun my/get-area-files ()
  "Get all .org files in the Areas directory."
  (directory-files my/areas-dir t "\\.org$"))

(defvar my/resource-capture-title nil "Temporary storage for resource title during capture.")
(defvar my/resource-capture-author nil "Temporary storage for resource author during capture.")
(defvar my/resource-capture-type nil "Temporary storage for resource type during capture.")

(defun my/sanitize-filename (string)
  "Sanitize STRING for use in filenames by replacing invalid chars with hyphens."
  (replace-regexp-in-string "[^A-Za-z0-9]+" "-" (downcase string)))

(defun my/capture-resource-file ()
  "Prompt for resource details, set global vars, and return file path.
   Structure: Brain/Resources/Type_Name_Author.org"
  (let* ((type (completing-read "Resource Type: "
                                '("Article" "Video" "Podcast" "Paper" "Book" "Blog" "News" "Course")))
         (name (read-string "Resource Name: "))
         (author (read-string "Author: "))
         (filename (format "%s_%s_%s.org"
                           (my/sanitize-filename type)
                           (my/sanitize-filename name)
                           (my/sanitize-filename author)))
         (path (expand-file-name filename my/resources-dir)))

    ;; Set global variables for the template to use
    (setq my/resource-capture-type type)
    (setq my/resource-capture-title name)
    (setq my/resource-capture-author author)

    ;; Ensure directory exists
    (unless (file-exists-p my/resources-dir)
      (make-directory my/resources-dir t))

    path))

;; =============================================================================
;; Helper Functions for Capture
;; =============================================================================

(defvar my/selected-area-file nil
  "Temporary storage for the selected area file during capture.")

(defun my/select-area-file ()
  "Prompt user to select an Area and return its file path."
  (let* ((area-files (directory-files my/areas-dir nil "\\.org$"))
         (area-names (mapcar (lambda (f) (file-name-sans-extension f)) area-files))
         (selected-area (completing-read "Select Area: " area-names nil t))
         (area-file (expand-file-name (concat selected-area ".org") my/areas-dir)))
    (setq my/selected-area-file area-file)
    area-file))

(defun my/goto-area-tasks ()
  "Navigate to the Tasks heading under Dashboard in the current Area file."
  (interactive)
  (goto-char (point-min))
  ;; Search for Tasks heading at level 2 or 3 (** Tasks or *** Tasks)
  (if (re-search-forward "^\\*\\*\\*? Tasks" nil t)
      (progn
        (org-end-of-subtree t)
        (unless (bolp) (insert "\n")))
    ;; Fallback: if no Tasks heading, go to Dashboard or create structure
    (if (re-search-forward "^\\* Dashboard" nil t)
        (progn
          (org-end-of-subtree t)
          (unless (bolp) (insert "\n"))
          (insert "** Tasks\n"))
      ;; Last fallback: end of file
      (goto-char (point-max))
      (unless (bolp) (insert "\n")))))

;; Variable to store the selected project for activity refile
(defvar my/last-toggl-project-choice nil
  "Stores the last Toggl project selected, used for activity auto-refile.")

;; Advice to capture the project choice from Toggl hook (non-invasive)
(defun my/capture-toggl-project-choice (orig-fun &rest args)
  "Advice to capture the Toggl project choice before calling the original function."
  (let ((result (apply orig-fun args)))
    result))

(defun my/activity-auto-refile ()
  "Auto-refile ACTIVITY_TYPE entries to the Area's Tasks section.
Uses the project selected in Toggl (stored in my/last-toggl-project-choice).
After refile, saves and opens today's agenda.
Skips execution if `my/mobile-sync-in-progress' is non-nil."
  (when (and (not (bound-and-true-p my/mobile-sync-in-progress))
             (derived-mode-p 'org-mode)
             (org-entry-get (point) "ACTIVITY_TYPE")
             my/last-toggl-project-choice)
    (let ((area-file (expand-file-name (concat my/last-toggl-project-choice ".org") my/areas-dir))
          (source-buffer (current-buffer)))
      (when (file-exists-p area-file)
        ;; Cut the current entry
        (org-cut-subtree)
        ;; Refile to Area file without switching visible buffer
        (with-current-buffer (find-file-noselect area-file)
          (goto-char (point-min))
          ;; Find Tasks heading and determine its level
          (if (re-search-forward "^\\(\\*\\*\\*?\\) Tasks" nil t)
              (let ((tasks-level (length (match-string 1))))
                (org-end-of-subtree t)
                (unless (bolp) (insert "\n"))
                ;; Paste one level deeper than Tasks heading
                (org-paste-subtree (1+ tasks-level)))
            ;; Fallback: end of file, level 3
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (org-paste-subtree 3))
          (save-buffer))
        ;; Save and close the source (logbook) buffer
        (with-current-buffer source-buffer
          (save-buffer)
          (kill-buffer))
        (message "Activity refiled to %s" my/last-toggl-project-choice)
        ;; Open today's agenda
        (org-agenda nil "a")))))

;; Add after Toggl hook (use high depth to ensure it runs AFTER toggl hook)
(add-hook 'org-clock-in-hook #'my/activity-auto-refile 90)

(defun my/get-capture-link-compact ()
  "Return the captured link formatted as [[link][#]].
Falls back to empty string if no link is captured."
  (let ((link (or (alist-get 'annotation org-store-link-plist)
                  (org-capture-get :annotation))))
    (if (and link (string-match "\\[\\[\\(.*?\\)\\]\\[.*?\\]\\]" link))
        (format "[[%s][#]]" (match-string 1 link))
      (if link
          (format "[[%s][#]]" link)
        ""))))

(defun journal--ensure-daily-heading ()
  "Create the top-level daily heading (* YYYY-MM-DD Day) for today if it doesn't exist, and position point after it."
  (goto-char (point-min))
  (let* ((day-heading-text (format-time-string "%Y-%m-%d %A"))
         (day-regex (format "^\\* %s$" day-heading-text)))
    ;; Search for a top-level heading matching today's date
    (unless (re-search-forward day-regex nil t)
      ;; If not found, insert it at the bottom of the file
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))  ; Ensure we're on a new line
      (insert (format "\n* %s\n" day-heading-text)))
    ;; Move to end of the heading line (whether found or created)
    (end-of-line)))

;; =============================================================================
;; Capture Templates (Consolidated)
;; =============================================================================

(setq org-capture-templates
      `(("i" "Inbox" entry
         (file my/inbox-file)
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :empty-lines 1)

        ("q" "New Project" entry
         (file my/gtd-projects-file)
         "* %^{Project Name} [/]\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:COOKIE_DATA: todo recursive\n:CATEGORY: %\\1\n:END:\n- Tags: %?\n\n** Description\n\n** Dashboard\n*** Tasks\n\n*** Notes\n"
         :empty-lines 1)

        ("u" "Fleeting Note" entry
         (file ,my/rough-notes-file)
         "* %^{Title}\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n:THOUGHTS:\n- %? \n:END:\n- Source: %(my/get-capture-link-compact)\n"
         :empty-lines 1)

        ("w" "Web Capture" entry
         (file my/inbox-file)
         "* [[%:link][%:description]]\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
         :empty-lines 1)

        ("h" "Log Time" entry (file+datetree ,my/logbook-file)
         "* %? \n" :clock-in t :clock-keep t :clock-resume t)

        ("j" "Journal" plain
         (file+function ,my/journal-file journal--ensure-daily-heading)
         "** %<%I:%M %p>:\n:PROPERTIES:\n:PROJECT: Habits\n:END:\n:LOGBOOK:\n:END:\n- %?"
         :empty-lines 1)

        ("t" "Resource" plain
         (file (lambda () (my/capture-resource-file)))
         ":PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n#+TITLE: %(symbol-value 'my/resource-capture-type):%(symbol-value 'my/resource-capture-title):%(symbol-value 'my/resource-capture-author)\n#+DATE: %U\n#+FILETAGS: \n#+AUTHOR: %(symbol-value 'my/resource-capture-author)\n#+SOURCE_TYPE: %(symbol-value 'my/resource-capture-type)\n#+URL: %^{URL}\n\n* Summary\n%?\n\n* Key Concepts\n\n* Quotes\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
         :unnarrowed t)

        ("a" "Activity" entry
         (file+datetree my/logbook-file)
         "* IN-PROGRESS %^{Activity}\n:PROPERTIES:\n:CREATED: %U\n:ACTIVITY_TYPE: t\n:END:\n- %?"
         :clock-in t :clock-keep t :empty-lines 1)

        ("y" "Job Application" entry
 (file+headline my/job-applications-file "Applications")
 "* TODO %^{Job Title} @ %^{Company}
:PROPERTIES:
:Job_Title: %\\1
:Company_Name: %\\2
:Application_Date: %u
:CV: (pending generation)
:Cover_Letter: %^{Cover Letter Status|In Progress|Not Submitted yet|Submitted|Generated}
:References:
:Follow_Up_Actions: %^{Follow Up Timeline|week|3days|10days|15days|month|none}
:Status: %^{Status|Pending|CV Generated|Applied|Interview Scheduled|Rejected|Offered|Withdrawn}
:Link_to_Job: %^{Job Posting URL}
:END:"
         :empty-lines 1)
         ))


(setq org-agenda-files (append (list my/inbox-file
                                     my/next-file
                                     my/gtd-projects-file
                                     my/waiting-file
                                     my/someday-file
                                     my/rituals-file
                                     my/gcal-file
                                     my/gtasks-dir
                                     my/job-applications-file
                                     my/phone-inbox)
                               (my/get-area-files)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "HOLD(h)" "WAITING(w)" "|" "DONE(d)"
      "CANCELED(c)"  "DELAYED(f)" )))

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
                       :todo "TODO")
                (:name "Deadlines"
                  :deadline t)
                (:name "Overdue"
                       :deadline past
                       :scheduled past)
                (:discard (:anything t))))))
    (todo "HOLD|IN-PROGRESS|WAITING"
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
                 (tags-query (format "+%s+%s/TODO" loc-tag mode-tag)))

            (push (list key desc
                        (append (my/gtd-standard-header)
                                `((tags-todo ,tags-query
                                             ((org-agenda-overriding-header ,header))))))
                  commands)))))
    (nreverse commands)))

(setq org-agenda-custom-commands
      `(("o" "View All (GTD Dashboard)"
         (,@(my/gtd-standard-header)

          (todo "TODO"
                ((org-agenda-overriding-header "To Refile")
                 (org-agenda-files (list my/inbox-file my/phone-inbox))))
           (todo "TODO|WAITING|HOLD"
                ((org-agenda-overriding-header "Waiting")
                 (org-agenda-files (list my/waiting-file))))
          (todo "TODO|HOLD|WAITING"
                ((org-agenda-overriding-header "Projects & Areas (Backlog)")
                 (org-agenda-files (append (list my/gtd-projects-file my/job-applications-file)
                                           (my/get-area-files) ))
                 (org-super-agenda-groups '((:auto-category t)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
          (todo "TODO"
                ((org-agenda-overriding-header "One-off Tasks")
                 (org-agenda-files (list my/next-file))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))
         nil)

        ("a" "Only Agenda"
         (,@(my/gtd-standard-header))
         nil)

        ,@(my/generate-gtd-agenda-commands)

        ("m" . "Work Modes (Global)")
        ("md" "Deep Work Mode (All Contexts)"
         ,(append (my/gtd-standard-header)
                  '((tags-todo "+@deep/TODO"
                               ((org-agenda-overriding-header " Deep Work Tasks"))))))
        ("ms" "Shallow Work Mode (All Contexts)"
         ,(append (my/gtd-standard-header)
                  '((tags-todo "+@shallow/TODO"
                               ((org-agenda-overriding-header " Shallow Work Tasks"))))))

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
        (,my/someday-file :maxlevel . 1)
        (,my/waiting-file :maxlevel . 1)
        (,my/inbox-file :maxlevel . 1)
        (,my/discarded-file :maxlevel . 1)
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
This function checks `org-state' and `org-last-state'.
Skips execution if `my/mobile-sync-in-progress' is non-nil to prevent
duplicate clock entries when syncing from mobile app."
  ;; CRITICAL: Skip if mobile sync is in progress
  (unless (bound-and-true-p my/mobile-sync-in-progress)
    ;; 1. Clock IN when moving TO "IN PROGRESS"
    (when (string= org-state "IN-PROGRESS")
      ;; We removed (unless (org-clock-is-active)) so it ALWAYS clocks in
      (org-clock-in))

    ;; 2. Clock OUT when moving FROM "IN PROGRESS" to anything else
    (when (and (string= org-last-state "IN-PROGRESS")
               (not (string= org-state "IN-PROGRESS")))
      (when (org-clock-is-active)
        (org-clock-out)))))

(add-hook 'org-after-todo-state-change-hook 'my-org-clock-on-state-change)
(setq org-archive-location (concat my/archive-dir "%s_archive.org::"))
;;;this to remove the dialog bod of the timer in elisp
(setq org-confirm-elisp-link-function nil)
(global-set-key (kbd "C-c d") 'rsr/org-timeblock-split-view)

(define-key org-agenda-mode-map "j" 'my/org-agenda-process-inbox-item)

(defun my/org-agenda-add-effort-suffix (original-fn &rest args)
  "Advice to append Effort property to the agenda line."
  (let* ((effort (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort"))
         (result (apply original-fn args)))
    (if effort
        (concat result (propertize (format " (%s)" effort)
                                   'face '(:foreground "cyan" :slant italic)))
      result)))

(advice-add 'org-agenda-format-item :around #'my/org-agenda-add-effort-suffix)

;; =============================================================================
;; Keybindings for Capture
;; =============================================================================
(global-set-key (kbd "C-c i") (lambda () (interactive) (org-capture nil "i")))  ;; Inbox
(global-set-key (kbd "C-c q") (lambda () (interactive) (org-capture nil "q")))  ;; New Project
(global-set-key (kbd "C-c n") (lambda () (interactive) (org-capture nil "u")))  ;; Note
(global-set-key (kbd "C-c j") (lambda () (interactive) (org-capture nil "j")))  ;; Journal
(global-set-key (kbd "C-c h") (lambda () (interactive) (org-capture nil "h")))  ;; Log Time
(global-set-key (kbd "C-c r") (lambda () (interactive) (org-capture nil "t")))
(global-set-key (kbd "C-c w") (lambda () (interactive) (org-protocol-capture nil "w"))) ;; Web Capture (manual trigger)
(global-set-key (kbd "C-c t") (lambda () (interactive) (org-capture nil "a")))  ;; Activity (direct to Area Tasks)
(global-set-key (kbd "C-c y") (lambda () (interactive) (org-capture nil "y")))
;; Capture Jobs

(provide 'gtd-config)
;;; gtd-config.el ends here
