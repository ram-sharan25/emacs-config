;;; gtd-config.el --- GTD Workflow Configuration -*- lexical-binding: t; -*-


(require 'paths)
(require 'org-capture)
(require 'org-id)
(require 'devlog)

(defun my/get-area-files ()
  "Every project file and hub under Areas/, recursively (agenda and refile set).
Skips autosaves (#x#), lockfiles (.#x) and dotfiles."
  (when (file-directory-p my/areas-dir)
    (directory-files-recursively my/areas-dir "\\`[^.#].*\\.org\\'")))

(defun my/area-hub-file (area)
  "Path of AREA's hub file, Areas/<AREA>/<AREA>.org."
  (expand-file-name (concat area "/" area ".org") my/areas-dir))

(defun my/get-area-hub-files ()
  "The three hub files."
  (mapcar #'my/area-hub-file my/area-names))

(defun my/get-project-files ()
  "Area files that are projects, i.e. everything under Areas/ except the hubs."
  (let ((hubs (my/get-area-hub-files)))
    (seq-remove (lambda (f) (member f hubs)) (my/get-area-files))))

(defvar my/resource-capture-title nil "Temporary storage for resource title during capture.")
(defvar my/resource-capture-author nil "Temporary storage for resource author during capture.")
(defvar my/resource-capture-type nil "Temporary storage for resource type during capture.")
(defvar my/resource-capture-sections nil "Temporary storage for type-specific sections during capture.")
(defvar my/resource-capture-file-field nil "Temporary storage for optional local file path during capture.")
(defvar my/resource-capture-url nil "Temporary storage for URL during capture.")
(defvar my/resource-capture-noter-field nil "Temporary storage for NOTER_DOCUMENT property line during capture.")

(defun my/sanitize-filename (string)
  "Sanitize STRING for use in filenames by replacing invalid chars with hyphens."
  (replace-regexp-in-string "[^A-Za-z0-9]+" "-" (downcase string)))

(defun my/capture-paper-resource-file ()
  "Handle Paper type: BibTeX picker → set globals → register PDF-open hook.
Returns the note file path (Resources/<citekey>.org)."
  (require 'bibtex-completion)
  (let* ((candidates (bibtex-completion-candidates))
         (selected   (completing-read "Paper (Zotero): " candidates nil t))
         (entry      (cdr (assoc selected candidates)))
         (key        (bibtex-completion-get-value "=key=" entry))
         (title      (bibtex-completion-get-value "title" entry "Untitled"))
         (author     (bibtex-completion-get-value "author" entry "Unknown"))
         (url        (or (bibtex-completion-get-value "url" entry) ""))
         (pdf-path   (car (bibtex-completion-find-pdf key)))
         (note-file  (expand-file-name (concat key ".org") my/resources-dir)))
    (setq my/resource-capture-type        "Paper")
    (setq my/resource-capture-title       title)
    (setq my/resource-capture-author      author)
    (setq my/resource-capture-url         url)
    (setq my/resource-capture-file-field  "")
    (setq my/resource-capture-sections
          "* Raw Notes\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n- src: %a\n\n* Compiled Notes\n\n* Quotes\n#+BEGIN_QUOTE\n\n#+END_QUOTE\n\n* Summary\n")
    (setq my/resource-capture-noter-field
          (if pdf-path
              (format ":NOTER_DOCUMENT: %s\n" pdf-path)
            ""))
    ;; After capture finalises, open the PDF in a right-side window.
    (when pdf-path
      (letrec ((hook (lambda ()
                       (remove-hook 'org-capture-after-finalize-hook hook)
                       (display-buffer
                        (find-file-noselect pdf-path)
                        '((display-buffer-in-side-window)
                          (side . right)
                          (window-width . 0.45))))))
        (add-hook 'org-capture-after-finalize-hook hook)))
    note-file))

(defun my/capture-resource-file ()
  "Prompt for resource details, set global vars, and return file path.
For Paper: uses BibTeX picker and opens PDF after capture.
For others: Structure is Brain/Resources/Type_Name_Author.org"
  (let* ((type (completing-read "Resource Type: "
                                '("Article" "Video" "Podcast" "Blog" "News" "Course" "Paper"))))
    (if (string= type "Paper")
        ;; Paper: delegate entirely to the BibTeX-aware helper.
        (my/capture-paper-resource-file)
      ;; All other types: prompt manually.
      (let* ((name     (read-string "Resource Name: "))
             (author   (read-string "Author: "))
             (filename (format "%s_%s_%s.org"
                               (my/sanitize-filename type)
                               (my/sanitize-filename name)
                               (my/sanitize-filename author)))
             (path     (expand-file-name filename my/resources-dir)))
        (setq my/resource-capture-type        type)
        (setq my/resource-capture-title       name)
        (setq my/resource-capture-author      author)
        (setq my/resource-capture-url         (read-string "URL: "))
        (setq my/resource-capture-file-field
              (if (member type '("Video" "Podcast"))
                  (let ((file (read-string "Local file path (leave empty if none): ")))
                    (if (string-empty-p file) "" (format "#+FILE: %s\n" file)))
                ""))
        (setq my/resource-capture-noter-field "")
        (setq my/resource-capture-sections
              (pcase type
                ("Video"   "* Raw Notes\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n- src: %a\n\n* Compiled Notes\n\n* Summary\n")
                ("Podcast" "* Raw Notes\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n- src: %a\n\n* Compiled Notes\n\n* Summary\n")
                ("Course"  "* Raw Notes\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n- src: %a\n\n* Compiled Notes\n\n* Summary\n")
                ("News"    "* Raw Notes\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n- src: %a\n\n* Compiled Notes\n\n* Summary\n")
                (_         "* Raw Notes\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n- src: %a\n\n* Compiled Notes\n\n* Quotes\n#+BEGIN_QUOTE\n\n#+END_QUOTE\n\n* Summary\n")))
        (unless (file-exists-p my/resources-dir)
          (make-directory my/resources-dir t))
        path))))

;; Variable to store the selected project for activity refile
(defvar my/last-toggl-project-choice nil
  "Stores the last Toggl project selected, used for activity auto-refile.")

(defun my/activity-auto-refile ()
  "Auto-refile ACTIVITY_TYPE entries to the Area's Tasks section.
Uses the project selected in Toggl (stored in my/last-toggl-project-choice).
After refile, saves and opens today's agenda."
  (when (and (derived-mode-p 'org-mode)
             (org-entry-get (point) "ACTIVITY_TYPE")
             my/last-toggl-project-choice)
    (let ((area-file (my/area-hub-file my/last-toggl-project-choice))
          (source-buffer (current-buffer)))
      (when (file-exists-p area-file)
        ;; Cut the current entry
        (org-cut-subtree)
        ;; Refile to Area file without switching visible buffer
        (with-current-buffer (find-file-noselect area-file)
          (goto-char (point-min))
          ;; Find Tasks heading and determine its level
          (if (re-search-forward "^\\(\\*\\) Tasks" nil t)
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

(defun my/review-capture-position ()
  "Position point at the end of the current year's review subtree."
  (widen)
  (goto-char (point-min))
  (let* ((year (format-time-string "%Y"))
         (year-heading (format "^\\* %s$" (regexp-quote year))))
    (if (re-search-forward year-heading nil t)
        (progn
          (beginning-of-line)
          (org-end-of-subtree t t))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert "\n* " year "\n")))
  (unless (bolp)
    (insert "\n")))

(defun my/weekly-review-heading ()
  "Return a heading describing the current Monday-to-Sunday week."
  (let* ((now (current-time))
         (weekday (string-to-number (format-time-string "%u" now)))
         (start (time-subtract now (days-to-time (1- weekday))))
         (end (time-add start (days-to-time 6))))
    (format "Week %s — %s to %s Weekly Review"
            (format-time-string "%V" start)
            (format-time-string "%Y-%m-%d" start)
            (format-time-string "%Y-%m-%d" end))))

(defun my/required-review-template (review-type template-file)
  "Return the capture section from TEMPLATE-FILE for REVIEW-TYPE.
Signal an explicit error when the file or its template markers are missing.
Content outside the markers remains documentation and is not copied into each
captured review."
  (unless (file-readable-p template-file)
    (user-error
     "%s review requires this Org template, but it was not found or readable: %s"
     review-type template-file))
  (with-temp-buffer
    (insert-file-contents template-file)
    (goto-char (point-min))
    (unless (re-search-forward "^# TEMPLATE-BEGIN[ \t]*$" nil t)
      (user-error
       "%s review template is missing the required # TEMPLATE-BEGIN marker: %s"
       review-type template-file))
    (forward-line 1)
    (let ((begin (point)))
      (unless (re-search-forward "^# TEMPLATE-END[ \t]*$" nil t)
        (user-error
         "%s review template is missing the required # TEMPLATE-END marker: %s"
         review-type template-file))
      (buffer-substring-no-properties begin (line-beginning-position)))))

(defun my/monthly-review-template ()
  "Return the required monthly-review Org template."
  (my/required-review-template "Monthly" my/monthly-review-template-file))

(defun my/weekly-review-template ()
  "Return the required weekly-review Org template."
  (my/required-review-template "Weekly" my/weekly-review-template-file))

;;; New-project file target (used by the "q" capture template; D3/D5, brain-roam-id-links patch)

(defvar my/new-project-title nil)
(defvar my/new-project-area nil)
(defvar my/new-project-kinds nil)

(defun my/new-project-file ()
  "Ask for area, title and kinds; return the path Areas/<Area>/<slug>.org.
Sets `my/new-project-title', `my/new-project-area' and `my/new-project-kinds'
for the template."
  (setq my/new-project-area (completing-read "Area: " my/area-names nil t)
        my/new-project-title (read-string "Project title: ")
        my/new-project-kinds (completing-read-multiple "Kind(s), one or two: " my/project-kinds nil t))
  (let* ((slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase my/new-project-title)))
         (slug (string-trim slug "-+" "-+"))
         (dir (expand-file-name my/new-project-area my/areas-dir))
         (file (expand-file-name (concat slug ".org") dir)))
    (make-directory dir t)
    (when (file-exists-p file)
      (user-error "Project file exists: %s" file))
    file))

(defun my/new-project-filetags ()
  "FILETAGS line value for the project being captured."
  (concat ":" (mapconcat #'identity my/new-project-kinds ":") ":"))

;;; Capture Templates

(setq org-capture-templates
      `(("i" "Inbox" entry
         (file my/inbox-file)
         "* TODO %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n- src: %a\n"
         :empty-lines 1)

        ("s" "Study Task" entry
         (file my/inbox-file)
         "* TODO %^{Read|Watch|Listen}: %^{Source}   :study:\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n- src: %a\n- res: \n- [ ] Resource note: paper/book → =C-c r n= | other → =C-c c r= (paste link in -res:)\n- [ ] Consume: video → =C-c v o= + =C-c v= | article → =C-c c n= | paper → =C-c r p= + =M-i=\n- [ ] Extract Zettels → =C-c n c → z= (same session)\n- [ ] Link Zettels to * Key Concepts in resource → =C-c n i=\n%?"
         :empty-lines 1)

        ;; One file per project under Areas/<Area>/ (D5). The :ID: drawer comes
        ;; FIRST so the file is a roam node and links inside its tasks are
        ;; credited to it (D16 R3).
        ("q" "New Project" plain
         (file (lambda () (my/new-project-file)))
         ":PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n#+TITLE: %(symbol-value 'my/new-project-title)\n#+PROPERTY: AREA %(symbol-value 'my/new-project-area)\n#+FILETAGS: %(my/new-project-filetags)\n#+ARCHIVE: ::* Archive\n\n* Outcome\n%?\n\n* Tasks [/]\n:PROPERTIES:\n:COOKIE_DATA: todo recursive\n:END:\n\n* Log\n\n* Notes\n- src: %a\n\n* Retro\n\n* Archive                                                            :ARCHIVE:\n"
         :unnarrowed t)

        ("u" "Fleeting Note" entry
         (file ,my/rough-notes-file)
         "* %^{Title}\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n:THOUGHTS:\n- %? \n:END:\n- src: %a\n"
         :empty-lines 1)

        ("W" "Web Capture" entry
         (file my/inbox-file)
         "* [[%:link][%:description]]\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
         :empty-lines 1)

        ("d" "Devlog line" plain (function rsr/devlog-capture-target)
         ,rsr/devlog-capture-template :empty-lines 0
         :hook rsr/devlog-capture-show-devlog)

        ("j" "Journal" plain
         (file+function ,my/journal-file journal--ensure-daily-heading)
         "** %<%I:%M %p>:\n:PROPERTIES:\n:AREA: Personal\n:END:\n:LOGBOOK:\n:END:\n- src: %a\n- %?"
         :empty-lines 1)

        ("m" "Monthly Review" plain
         (file+function ,my/monthly-reviews-file my/review-capture-position)
         (function my/monthly-review-template)
         :empty-lines 1
         :jump-to-captured t)

        ("w" "Weekly Review" plain
         (file+function ,my/weekly-reviews-file my/review-capture-position)
         (function my/weekly-review-template)
         :empty-lines 1
         :jump-to-captured t)

        ("t" "Resource" plain
         (file (lambda () (my/capture-resource-file)))
         ":PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:COMPILE_STATE: raw\n%(symbol-value 'my/resource-capture-noter-field):END:\n#+TITLE: %(symbol-value 'my/resource-capture-type):%(symbol-value 'my/resource-capture-title):%(symbol-value 'my/resource-capture-author)\n#+DATE: %U\n#+FILETAGS: :source:\n#+AUTHOR: %(symbol-value 'my/resource-capture-author)\n#+SOURCE_TYPE: %(symbol-value 'my/resource-capture-type)\n#+URL: %(symbol-value 'my/resource-capture-url)\n%(symbol-value 'my/resource-capture-file-field)#+CREATED_FROM: %a\n\n%(symbol-value 'my/resource-capture-sections)%?"
         :unnarrowed t)

        ("S" "Capture Stream" plain
         (file my/stream--capture-target)
         "%(my/stream--scaffold my/stream--capture-title)- %?"
         :unnarrowed t)

        ;; Lands in the inbox and is auto-refiled to the area hub by
        ;; `my/activity-auto-refile' once Toggl has the project (D8: no clock log).
        ("a" "Activity" entry
         (file my/inbox-file)
         "* IN-PROGRESS %^{Activity}\n:PROPERTIES:\n:CREATED: %U\n:ACTIVITY_TYPE: t\n:END:\n- %?"
         :clock-in t :clock-keep t :empty-lines 1)

        ("y" "Job Application" entry
 (file+headline my/job-applications-file "Tasks")
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


;; The three area DIRECTORIES, not their files: `org-agenda-files' (the
;; function) expands a directory entry on every call, so a project captured
;; with "q" (or synced in from another device) shows up in the next agenda
;; without a restart. Areas/<Area>/ is flat (D5), so the non-recursive
;; expansion is enough. Always read the set through (org-agenda-files).
(setq org-agenda-files (append (list my/inbox-file
                                     my/waiting-file
                                     my/someday-file
                                     my/rituals-file)
                               (mapcar (lambda (c) (plist-get c :file)) my/gcal-calendars)
                               (mapcar (lambda (a) (expand-file-name a my/areas-dir))
                                       my/area-names)))

;; Habits (D15): rituals.org holds habits only, each with :STYLE: habit and a
;; repeater. org-habit is loaded for its scheduling (done/not-done per day)
;; but the consistency graph is off: the user wants clean agenda buffers.
;; `org-habit-show-habits' must stay t: nil removes habit entries from every
;; agenda view (org-agenda-get-scheduled skips them), it does not hide the
;; graph. The graph is drawn by `org-habit-insert-consistency-graphs' in
;; org-agenda-finalize, so that function is the one to silence.
(add-to-list 'org-modules 'org-habit)
(require 'org-habit)
(setq org-habit-show-habits t
      org-habit-show-habits-only-for-today nil)
(advice-add 'org-habit-insert-consistency-graphs :override #'ignore)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "HOLD(h)" "WAITING(w)" "DELAYED(f)"
                  "|" "DONE(d)" "CANCELED(c)")))

;; DELAYED is an open state (it counts in [/] cookies and is never "done"),
;; but it stays out of every agenda view until `my/agenda-toggle-delayed'
;; turns it on.  The skip is global, so it covers the day agenda as well as
;; the todo blocks that list DELAYED below.
(defvar my/agenda-show-delayed nil
  "Non-nil shows DELAYED entries in agenda views.")

(defun my/agenda-skip-delayed ()
  "Skip DELAYED entries unless `my/agenda-show-delayed' is non-nil."
  (unless my/agenda-show-delayed
    (org-agenda-skip-entry-if 'todo '("DELAYED"))))

(setq org-agenda-skip-function-global #'my/agenda-skip-delayed)

(defun my/agenda-toggle-delayed ()
  "Show or hide DELAYED entries in agenda views, then refresh the agenda."
  (interactive)
  (setq my/agenda-show-delayed (not my/agenda-show-delayed))
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-redo-all))
  (message "DELAYED tasks %s" (if my/agenda-show-delayed "shown" "hidden")))

(setq org-tag-alist nil)

(setq org-log-done 'time)        ;; record CLOSED: timestamp when marking DONE
(setq org-log-into-drawer t)     ;; store log entries in :LOGBOOK: drawer

;;; Agenda

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode))

(defun my/org-agenda-project-suffix ()
  "Category (= the project or hub file name, D5) padded to a fixed width."
  (let ((cat (org-get-category))
        (width 40))
    (if (or (null cat)
            (string= (format "%s" cat) "nil")
            (string= (format "%s" cat) "???"))
        (make-string width ?\s)
      (format (format "%%-%ds" width) (format "%s:" cat)))))

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
           (org-agenda-files (my/get-area-files))))))

(setq org-agenda-custom-commands
      `(("o" "GTD Dashboard"
         (,@(my/gtd-standard-header)
          (todo "TODO"
                ((org-agenda-overriding-header "To Refile")
                 (org-agenda-files (list my/inbox-file))))
          (todo "TODO|WAITING|HOLD"
                ((org-agenda-overriding-header "Waiting")
                 (org-agenda-files (list my/waiting-file))))
          (todo "TODO|HOLD|WAITING|DELAYED"
                ((org-agenda-overriding-header "Projects & Areas (Backlog)")
                 (org-agenda-files (my/get-area-files))
                 (org-super-agenda-groups '((:auto-category t)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
          (todo "TODO|HOLD|WAITING"
                ((org-agenda-overriding-header "Someday/Maybe")
                 (org-agenda-files (list my/someday-file)))))
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
                                                 (org-agenda-files)))
                   (org-agenda-show-log 'closed)
                   (org-agenda-log-mode-items '(closed))))
          ;; scheduled but not done — exclude rituals
          (agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-overriding-header "Not Completed — reschedule or drop")
                   (org-agenda-files (seq-remove (lambda (f) (equal f my/rituals-file))
                                                 (org-agenda-files)))
                   (org-agenda-entry-types '(:scheduled :deadline))
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELED")))))
          ;; everything still open — exclude rituals
          (todo "TODO|HOLD|WAITING|DELAYED"
                ((org-agenda-overriding-header "Open Tasks — schedule for next week")
                 (org-agenda-files (seq-remove (lambda (f) (equal f my/rituals-file))
                                               (org-agenda-files)))
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
         ((todo "TODO|IN-PROGRESS|HOLD|WAITING|DELAYED"
                ((org-agenda-overriding-header "Projects")
                 (org-agenda-files (my/get-project-files))
                 (org-super-agenda-groups '((:auto-category t)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))
         nil)))

(setq org-agenda-span 'day)
;; Always show the time grid: drop `require-timed' so the hour lines appear
;; even on days with no clock-timed entries.
(setq org-agenda-time-grid
      '((daily today)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-sticky t)                ;; keep agenda buffer alive after closing
(setq org-agenda-window-setup 'current-window) ;; open agenda in current window


;;; Refile Targets

(setq org-refile-targets
      `((my/get-area-files :regexp . "^\\* Tasks")   ; * Tasks of every project file and hub
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
  (let ((files (org-agenda-files)))
    (save-some-buffers t (lambda ()
                           (when (member (buffer-file-name) files)
                             t))))
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
  (setq org-timeblock-inbox-file my/inbox-file)
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
  "Clock in/out when TODO state changes to/from IN-PROGRESS."
  (when (string= org-state "IN-PROGRESS")
    (org-clock-in))
  (when (and (string= org-last-state "IN-PROGRESS")
             (not (string= org-state "IN-PROGRESS")))
    (when (org-clock-is-active)
      (org-clock-out))))

(add-hook 'org-after-todo-state-change-hook 'my/org-clock-on-state-change)
(setq org-archive-location (concat my/archive-dir "%s_archive.org::"))  ; gtd/ files; project and hub files override with #+ARCHIVE: ::* Archive (D5)
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
(defun my/capture-devlog ()       "Capture a devlog line."     (interactive) (org-capture nil "d"))
(defun my/capture-monthly-review () "Capture a fresh monthly review." (interactive) (org-capture nil "m"))
(defun my/capture-weekly-review () "Capture a fresh weekly review." (interactive) (org-capture nil "w"))
(defun my/capture-resource ()     "Capture resource."          (interactive) (org-capture nil "t"))
(defun my/capture-web ()          "Capture web link."          (interactive) (org-capture nil "W"))
(defun my/capture-activity ()     "Capture activity."          (interactive) (org-capture nil "a"))
(defun my/capture-job ()          "Capture job application."   (interactive) (org-capture nil "y"))

;; C-c a agenda, C-c c capture, C-c t g clock goto and C-c t b timeblock
;; view are in keymaps.el.
(define-key org-agenda-mode-map "j" 'my/org-agenda-process-inbox-item)
(define-key org-agenda-mode-map "H" 'my/org-agenda-toggle-highlight)
(define-key org-agenda-mode-map "E" 'my/org-day-effort-budget)
(define-key org-agenda-mode-map "D" 'my/schedule-deep-work-block)
(define-key org-agenda-mode-map "V" 'my/agenda-toggle-delayed)

;;; --- Reschedule / Deadline change logging ---

(setq org-log-reschedule 'time)   ;; log old date in LOGBOOK when rescheduled
(setq org-log-redeadline 'time)   ;; log old date in LOGBOOK when deadline changes
(setq org-log-into-drawer t)      ;; keep LOGBOOK entries in drawer (not inline)

(provide 'gtd-config)
;;; gtd-config.el ends here
