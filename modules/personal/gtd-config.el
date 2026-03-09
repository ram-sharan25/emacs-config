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
(defvar my/resource-capture-sections nil "Temporary storage for type-specific sections during capture.")
(defvar my/resource-capture-file-field nil "Temporary storage for optional local file path during capture.")

(defun my/sanitize-filename (string)
  "Sanitize STRING for use in filenames by replacing invalid chars with hyphens."
  (replace-regexp-in-string "[^A-Za-z0-9]+" "-" (downcase string)))

(defun my/capture-resource-file ()
  "Prompt for resource details, set global vars, and return file path.
   Structure: Brain/Resources/Type_Name_Author.org"
  (let* ((type (completing-read "Resource Type: "
                                '("Article" "Video" "Podcast" "Blog" "News" "Course")))
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
    (setq my/resource-capture-sections
          (pcase type
            ("Video"   "* Summary\n\n* Key Concepts\n\n* Timestamps\n")
            ("Podcast" "* Summary\n\n* Key Concepts\n\n* Timestamps\n")
            ("Course"  "* Summary\n\n* Key Concepts\n\n* Exercises\n")
            ("News"    "* Summary\n\n* Key Points\n")
            (_         "* Summary\n\n* Key Concepts\n\n* Quotes\n#+BEGIN_QUOTE\n\n#+END_QUOTE\n")))
    (setq my/resource-capture-file-field
          (if (member type '("Video" "Podcast"))
              (let ((file (read-string "Local file path (leave empty if none): ")))
                (if (string-empty-p file) "" (format "#+FILE: %s\n" file)))
            ""))

    ;; Ensure directory exists
    (unless (file-exists-p my/resources-dir)
      (make-directory my/resources-dir t))

    path))

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

(defun my/get-area-names ()
  "Return a list of Area names (filenames without extension) from `my/areas-dir`."
  (let ((files (directory-files my/areas-dir nil "\\.org$")))
    (mapcar #'file-name-sans-extension files)))

(defun my/get-area-id-by-name (area-name)
  "Return the ID of the Area file corresponding to AREA-NAME.
Creates the ID if it doesn't exist."
  (let ((file (expand-file-name (concat area-name ".org") my/areas-dir)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-id-get-create)))))

(defun my/org-get-project-headings (&optional area-filter)
  "Return an alist of (Project Name . ID) from `my/projects-file` with ACTIVE=TRUE.
If AREA-FILTER is provided, only include projects with a matching :AREA: property."
  (with-current-buffer (find-file-noselect my/projects-file)
    (let ((projects '()))
      (org-map-entries
       (lambda ()
         (let* ((heading (org-get-heading t t))
                (id (org-id-get-create))
                (area (org-entry-get nil "AREA")))
           (when (or (null area-filter)
                     (string= area area-filter))
             (push (cons heading id) projects))))
       "LEVEL=1+ACTIVE=\"TRUE\"" 'file)
      (nreverse projects))))

(defun my/select-area-default-misc ()
  "Prompt for Area, defaulting to 'Misc' and showing it first."
  (let* ((areas (my/get-area-names))
         (options (cons "Misc" (remove "Misc" areas)))
         (selected (completing-read "Area: " options nil t nil nil "Misc")))
    (if (string-empty-p selected) "Misc" selected)))

(defun my/org-select-project-allow-empty (&optional area-filter)
  "Prompt user to select a project, optionally filtered by AREA-FILTER.
Returns a cons cell (Name . ID). Includes 'Dump' as the first option."
  (let* ((project-alist (my/org-get-project-headings area-filter))
         (options (cons '("Dump" . nil) project-alist))
         (project-names (mapcar #'car options))
         (selected-name (completing-read "Select Project: "
                                         project-names nil t nil nil "Dump")))
    (if (string-empty-p selected-name)
        '("Dump" . nil)
      (assoc selected-name options))))

;; Variable to store the selected project for activity refile
(defvar my/last-toggl-project-choice nil
  "Stores the last Toggl project selected, used for activity auto-refile.")

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

;;; Capture Templates

(setq org-capture-templates
      `(("i" "Inbox" entry
         (file my/inbox-file)
         "* TODO %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n- src: %a\n"
         :empty-lines 1)

        ("s" "Study Task" entry
         (file my/inbox-file)
         "* TODO %^{Read|Watch|Listen}: %^{Source}   :study:\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n- src: %a\n- res: \n- [ ] Resource note: paper/book → =M-m r n= | other → =C-c c r= (paste link in -res:)\n- [ ] Consume: video → =C-c v o= + =C-c v= | article → =C-c c n= | paper → =M-m r p= + =M-i=\n- [ ] Extract Zettels → =M-m r c → z= (same session)\n- [ ] Link Zettels to * Key Concepts in resource → =M-m r i=\n%?"
         :empty-lines 1)

        ("q" "New Project" entry
         (file my/gtd-projects-file)
         "* %^{Project Name} [/]\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:COOKIE_DATA: todo recursive\n:CATEGORY: %\\1\n:END:\n- Tags: %?\n- src: %a\n\n** Description\n\n** Dashboard\n*** Tasks\n\n*** Notes\n"
         :empty-lines 1)

        ("u" "Fleeting Note" entry
         (file ,my/rough-notes-file)
         "* %^{Title}\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n:THOUGHTS:\n- %? \n:END:\n- src: %a\n"
         :empty-lines 1)

        ("w" "Web Capture" entry
         (file my/inbox-file)
         "* [[%:link][%:description]]\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
         :empty-lines 1)

        ("h" "Log Time" entry (file+datetree ,my/logbook-file)
         "* %? \n" :clock-in t :clock-keep t :clock-resume t)

        ("j" "Journal" plain
         (file+function ,my/journal-file journal--ensure-daily-heading)
         "** %<%I:%M %p>:\n:PROPERTIES:\n:PROJECT: Habits\n:END:\n:LOGBOOK:\n:END:\n- src: %a\n- %?"
         :empty-lines 1)

        ("t" "Resource" plain
         (file (lambda () (my/capture-resource-file)))
         ":PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n#+TITLE: %(symbol-value 'my/resource-capture-type):%(symbol-value 'my/resource-capture-title):%(symbol-value 'my/resource-capture-author)\n#+DATE: %U\n#+FILETAGS: \n#+AUTHOR: %(symbol-value 'my/resource-capture-author)\n#+SOURCE_TYPE: %(symbol-value 'my/resource-capture-type)\n#+URL: %^{URL}\n%(symbol-value 'my/resource-capture-file-field)#+CREATED_FROM: %a\n\n%(symbol-value 'my/resource-capture-sections)%?"
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
                                     my/job-applications-file
                                     my/phone-inbox)
                               (my/get-area-files)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "HOLD(h)" "WAITING(w)" "|" "DONE(d)"
      "CANCELED(c)"  "DELAYED(f)" )))

(setq org-tag-alist nil)

(setq org-log-done 'time)        ;; record CLOSED: timestamp when marking DONE
(setq org-log-into-drawer t)     ;; store log entries in :LOGBOOK: drawer

;;; Agenda

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode))

(defun my/org-agenda-project-suffix ()
  "Format as [Project]:Category, pad to fixed width, and hide 'nil' or '???'."
  (let* ((cat (org-get-category))
         (width 40))
    (if (or (null cat)
            (string= (format "%s" cat) "nil")
            (string= (format "%s" cat) "???"))
        (make-string width ?\s)
      (let* ((project (ignore-errors (org-entry-get nil "PROJECT")))
             (output-str
              (if project
                  (concat (propertize (format "[%s]" project)
                                      'face '(:foreground "orange" :weight bold))
                          ":" cat)
                (format "%s:" cat))))
        (format (format "%%-%ds" width) output-str)))))

(setq org-agenda-prefix-format
      '((agenda . " %i %(my/org-agenda-project-suffix) %?-12t% s")
        (todo   . " %i %(my/org-agenda-project-suffix) ")
        (tags   . " %i %(my/org-agenda-project-suffix) ")
        (search . " %i %(my/org-agenda-project-suffix) ")))

;; hide "Scheduled:" for one-time items, keep "2x" repeat count for recurring
(setq org-agenda-scheduled-leaders '("" "%dx "))
;; keep deadline info: "Deadline" on due date, "In 3d" for upcoming, "2d ago" for overdue
(setq org-agenda-deadline-leaders '("Deadline: " "In %3d d. " "%2d d. ago "))

;;; Custom Agenda Commands

(defun my/gtd-standard-header ()
  "Return the standard agenda blocks: Day View + In Progress."
  `((agenda ""
            ((org-agenda-span 'day)
             (org-deadline-warning-days 7)
             (org-super-agenda-groups
              '((:name "⭐ Daily Highlight" :property "HIGHLIGHT")
                (:name "Today's Schedule"  :time-grid t)
                (:name "Scheduled"         :todo "TODO")
                (:name "Deadlines"         :deadline t)
                (:name "Overdue"           :deadline past :scheduled past)
                (:discard (:anything t))))))
    (todo "HOLD|IN-PROGRESS|WAITING"
          ((org-agenda-overriding-header "In Progress")
           (org-agenda-files (append (list my/gtd-projects-file my/next-file)
                                     (my/get-area-files)))))))

(setq org-agenda-custom-commands
      `(("o" "GTD Dashboard"
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
                                           (my/get-area-files)))
                 (org-super-agenda-groups '((:auto-category t)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
          (todo "TODO"
                ((org-agenda-overriding-header "One-off Tasks")
                 (org-agenda-files (list my/next-file))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))
         nil)

        ("a" "Day Agenda"
         (,@(my/gtd-standard-header))
         nil)

        ("w" "Weekly Review"
         (;; what got done — exclude rituals
          (agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-overriding-header "Completed This Week")
                   (org-agenda-files (seq-remove (lambda (f) (equal f my/rituals-file))
                                                 org-agenda-files))
                   (org-agenda-show-log 'closed)
                   (org-agenda-log-mode-items '(closed))))
          ;; scheduled but not done — exclude rituals
          (agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-overriding-header "Not Completed — reschedule or drop")
                   (org-agenda-files (seq-remove (lambda (f) (equal f my/rituals-file))
                                                 org-agenda-files))
                   (org-agenda-entry-types '(:scheduled :deadline))
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELED")))))
          ;; everything still open — exclude rituals
          (todo "TODO|HOLD|WAITING"
                ((org-agenda-overriding-header "Open Tasks — schedule for next week")
                 (org-agenda-files (seq-remove (lambda (f) (equal f my/rituals-file))
                                               org-agenda-files))
                 (org-super-agenda-groups '((:auto-category t)))))
          ;; someday/maybe — promote or drop?
          (todo "TODO"
                ((org-agenda-overriding-header "Someday/Maybe — promote or drop?")
                 (org-agenda-files (list my/someday-file)))))
         nil)

        ("h" "Habits & Rituals"
         (;; today's rituals
          (agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-overriding-header "Today's Rituals")
                   (org-agenda-files (list my/rituals-file))
                   (org-super-agenda-groups
                    '((:name "Due Today" :scheduled today :deadline today)
                      (:name "Overdue"   :scheduled past  :deadline past)
                      (:discard (:anything t))))))
          ;; week view — streak/consistency check
          (agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-overriding-header "This Week — Consistency")
                   (org-agenda-files (list my/rituals-file))
                   (org-agenda-show-log 'closed)
                   (org-agenda-log-mode-items '(closed state)))))
         nil)

        ("p" "Projects"
         ((todo "TODO|IN-PROGRESS|HOLD|WAITING"
                ((org-agenda-overriding-header "Projects")
                 (org-agenda-files (list my/gtd-projects-file))
                 (org-super-agenda-groups '((:auto-category t)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))
         nil)))

(setq org-agenda-span 'day)
(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-sticky t)                ;; keep agenda buffer alive after closing
(setq org-agenda-window-setup 'current-window) ;; open agenda in current window


;;; Refile Targets

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

(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
                         (when (member (buffer-file-name) org-agenda-files)
                           t)))
  (message "Saving org-agenda-files buffers... done"))

(advice-add 'org-refile :after
            (lambda (&rest _)
              (gtd-save-org-buffers)))

;; save all org buffers before agenda opens — prevents #file.org# lockfile issues
(advice-add 'org-agenda :before
            (lambda (&rest _)
              (org-save-all-org-buffers)))

;;; Inbox Processing

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
  "Process a single inbox item: set effort, schedule, then refile."
  (interactive)
  (call-interactively 'my/org-agenda-set-effort)
  (call-interactively 'org-agenda-schedule)
  (org-agenda-refile nil nil t))

;;; Deep Work

(defun my/schedule-deep-work-block ()
  "Schedule a timed deep work block for the task at point in the agenda.
Prompts for a time range and schedules today with that window."
  (interactive)
  (let ((time (read-string "Deep work block (HH:MM-HH:MM): " "09:00-11:00")))
    (org-agenda-schedule nil (concat (format-time-string "%Y-%m-%d") " " time))
    (message "Deep work block scheduled: %s" time)))


;;; Daily Highlight

(defun my/org-agenda-toggle-highlight ()
  "Toggle HIGHLIGHT property on the current agenda item.
Highlighted tasks appear at the top of the agenda as the daily focus."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker) (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos    (marker-position marker)))
    (with-current-buffer buffer
      (goto-char pos)
      (if (string= (org-entry-get nil "HIGHLIGHT") "t")
          (org-delete-property "HIGHLIGHT")
        (org-entry-put nil "HIGHLIGHT" "t")))
    (org-agenda-redo)))

;;; Effort Budget

(defun my/org-day-effort-budget ()
  "Show total effort of all tasks visible in the current agenda buffer."
  (interactive)
  (let ((total 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (effort (when marker (org-entry-get marker "Effort"))))
          (when effort
            (let* ((parts (split-string effort ":"))
                   (h (string-to-number (or (car parts) "0")))
                   (m (string-to-number (or (cadr parts) "0"))))
              (setq total (+ total (* h 60) m)))))
        (forward-line 1)))
    (if (> total 0)
        (message "Effort budget: %dh %dm scheduled"
                 (/ total 60) (% total 60))
      (message "No effort estimates found in current view"))))


(use-package org-timeblock
  :load-path "~/.emacs.d/modules/git-modules/org-timeblock/"
  :config
  (setq org-timeblock-span 1)
  (setq org-timeblock-day-start-hour 6)
  (setq org-timeblock-day-end-hour 23)
  (setq org-timeblock-scale 0.8)
  (setq org-timeblock-inbox-file my/tasks-file)
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

(defun my/org-clock-on-state-change ()
  "Clock in/out when TODO state changes to/from IN-PROGRESS.
Skips if `my/mobile-sync-in-progress' is non-nil (mobile sync guard)."
  (unless (bound-and-true-p my/mobile-sync-in-progress)
    (when (string= org-state "IN-PROGRESS")
      (org-clock-in))
    (when (and (string= org-last-state "IN-PROGRESS")
               (not (string= org-state "IN-PROGRESS")))
      (when (org-clock-is-active)
        (org-clock-out)))))

(add-hook 'org-after-todo-state-change-hook 'my/org-clock-on-state-change)
(setq org-archive-location (concat my/archive-dir "%s_archive.org::"))
(setq org-confirm-elisp-link-function nil) ;; don't confirm elisp links

(defun my/org-agenda-add-effort-suffix (original-fn &rest args)
  "Advice to append Effort property to the agenda line."
  (let* ((marker (org-get-at-bol 'org-hd-marker))
         (effort (when (and marker (marker-buffer marker))
                   (org-entry-get marker "Effort")))
         (result (apply original-fn args)))
    (if (and effort (stringp result))
        (concat result (propertize (format " (%s)" effort)
                                   'face '(:foreground "cyan" :slant italic)))
      result)))

(advice-add 'org-agenda-format-item :around #'my/org-agenda-add-effort-suffix)

;;; Keybindings

(defun my/capture-inbox ()        "Capture to inbox."          (interactive) (org-capture nil "i"))
(defun my/capture-study ()        "Capture study task."        (interactive) (org-capture nil "s"))
(defun my/capture-project ()      "Capture new project."       (interactive) (org-capture nil "q"))
(defun my/capture-note ()         "Capture fleeting note."     (interactive) (org-capture nil "u"))
(defun my/capture-journal ()      "Capture journal entry."     (interactive) (org-capture nil "j"))
(defun my/capture-log-time ()     "Capture log time entry."    (interactive) (org-capture nil "h"))
(defun my/capture-resource ()     "Capture resource."          (interactive) (org-capture nil "t"))
(defun my/capture-web ()          "Capture web link."          (interactive) (org-capture nil "w"))
(defun my/capture-activity ()     "Capture activity."          (interactive) (org-capture nil "a"))
(defun my/capture-job ()          "Capture job application."   (interactive) (org-capture nil "y"))

(global-set-key (kbd "C-c a")   'org-agenda)
(global-set-key (kbd "C-c d")   'rsr/org-timeblock-split-view)
(global-set-key (kbd "C-c c g") 'org-clock-goto)
(global-set-key (kbd "C-c c i") 'my/capture-inbox)
(global-set-key (kbd "C-c c s") 'my/capture-study)
(global-set-key (kbd "C-c c q") 'my/capture-project)
(global-set-key (kbd "C-c c n") 'my/capture-note)
(global-set-key (kbd "C-c c j") 'my/capture-journal)
(global-set-key (kbd "C-c c h") 'my/capture-log-time)
(global-set-key (kbd "C-c c r") 'my/capture-resource)
(global-set-key (kbd "C-c c w") 'my/capture-web)
(global-set-key (kbd "C-c c t") 'my/capture-activity)
(global-set-key (kbd "C-c c y") 'my/capture-job)
(define-key org-agenda-mode-map "j" 'my/org-agenda-process-inbox-item)
(define-key org-agenda-mode-map "H" 'my/org-agenda-toggle-highlight)
(define-key org-agenda-mode-map "E" 'my/org-day-effort-budget)
(define-key org-agenda-mode-map "D" 'my/schedule-deep-work-block)

;;; --- Reschedule / Deadline change logging ---

(setq org-log-reschedule 'time)   ;; log old date in LOGBOOK when rescheduled
(setq org-log-redeadline 'time)   ;; log old date in LOGBOOK when deadline changes
(setq org-log-into-drawer t)      ;; keep LOGBOOK entries in drawer (not inline)

(provide 'gtd-config)
;;; gtd-config.el ends here
