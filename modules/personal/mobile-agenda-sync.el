;;; mobile-agenda-sync.el --- Mobile Agenda Synchronization (Minimal) -*- lexical-binding: t; -*-

;;; Commentary:
;; This module implements a minimal bi-directional sync between Emacs org-agenda
;; and mobile devices.
;;
;; AUTOMATIC EXPORT:
;; - Exports automatically whenever you open agenda (C-c a a)
;; - Can also manually export with M-m s e from anywhere
;; - Creates agenda.org with today's scheduled/deadline items
;;
;; MANUAL SYNC BACK:
;; - Press M-m s s to sync TODO state changes from mobile back to Emacs
;;
;; File format: Flat list with ORIGIN_ID property for linking back

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'cl-lib)
(require 'json)
(require 'paths nil t)  ; Optional - provides my/phone-inbox-dir

;;; ============================================================================
;;; Configuration Variables
;;; ============================================================================

(defvar my/phone-inbox-dir
  (expand-file-name "~/Stillness/Brain/phone_inbox/")
  "Phone inbox directory - fallback if not defined in paths.")

(defcustom my/mobile-agenda-file
  (expand-file-name "agenda.org" my/phone-inbox-dir)
  "Path to the mobile agenda file for phone sync."
  :type 'file
  :group 'org-mobile-agenda)

(defcustom my/mobile-sync-dir
  (expand-file-name "sync/" my/phone-inbox-dir)
  "Directory where mobile app drops JSON sync files."
  :type 'directory
  :group 'org-mobile-agenda)

(defcustom my/mobile-sync-failed-dir
  (expand-file-name "sync/failed/" my/phone-inbox-dir)
  "Directory for failed sync attempts with error logs."
  :type 'directory
  :group 'org-mobile-agenda)

;;; ============================================================================
;;; Export Function - Auto-export on Agenda Open
;;; ============================================================================

(defvar my/mobile-agenda-exporting nil
  "Guard variable to prevent recursive export calls.")

(defvar my/mobile-sync-in-progress nil
  "Non-nil during mobile JSON sync. Guards clock, Toggl, and refile hooks.")

(defvar my/mobile-agenda-exclude-patterns '("/cleanup_\\d{4}_\\d{2}_\\d{2}/")
  "List of directory patterns to exclude from mobile agenda generation.")


(defun my/export-mobile-agenda-minimal ()
  "Export today's agenda items to mobile file.

Collects items by scanning `org-agenda-files' directly via
`org-agenda-get-day-entries' — no *Org Agenda* buffer is created or
touched, so `org-agenda-mode-hook' never fires and f/b navigation in
an open agenda is completely unaffected."
  (interactive)
  (when my/mobile-agenda-exporting
    (message "mobile-export: already in progress, skipping...")
    (cl-return-from my/export-mobile-agenda-minimal nil))

  (setq my/mobile-agenda-exporting t)
  (unwind-protect
      (let ((items '())
            (total-count 0)
            (today (calendar-current-date)))

        ;; Scan each agenda file directly — no agenda buffer involved.
        (dolist (file (org-agenda-files))
          (when (file-exists-p file)
            (dolist (entry (org-agenda-get-day-entries file today))
              (let ((marker (get-text-property 0 'org-marker entry)))
                (when marker
                  (with-current-buffer (marker-buffer marker)
                    (save-excursion
                      (goto-char (marker-position marker))
                      (condition-case err
                          (let* ((id       (org-id-get-create))
                                 (todo     (org-get-todo-state))
                                 (headline (org-get-heading t t t t))
                                 (ts       (or (org-entry-get nil "SCHEDULED")
                                               (org-entry-get nil "DEADLINE"))))
                            (push (list :title headline :todo todo :id id :ts ts) items)
                            (cl-incf total-count))
                        (user-error
                         (message "mobile-export: skipping non-heading in %s (%s)"
                                  (buffer-name) (error-message-string err)))))))))))

        ;; Write to mobile agenda file
        (with-temp-file my/mobile-agenda-file
          (insert "#+FILETAGS: :mobile:agenda:\n#+STARTUP: content\n\n")
          (insert "* README\n")
          (insert "Last Update: " (format-time-string "%Y-%m-%d %H:%M") "\n")
          (insert "Total Items: " (number-to-string total-count) "\n\n")
          (dolist (item (reverse items))
            (insert (format "* %s %s\n"
                            (or (plist-get item :todo) "TODO")
                            (plist-get item :title)))
            (when (plist-get item :ts)
              (insert (format "SCHEDULED: <%s>\n" (plist-get item :ts))))
            (insert ":PROPERTIES:\n")
            (insert (format ":ORIGIN_ID: [[id:%s]]\n" (plist-get item :id)))
            (insert ":END:\n")))

        (message "Mobile export complete: %d items → %s" total-count my/mobile-agenda-file))

    (setq my/mobile-agenda-exporting nil)))

;;; ============================================================================
;;; Auto-export Hook
;;; ============================================================================

(defun my/mobile-agenda-auto-export ()
  "Schedule a mobile export when the *Org Agenda* buffer is closed.
Registered on `kill-buffer-hook' locally so it fires only for the agenda
buffer — after the user has finished rescheduling and closes the agenda."
  (unless my/mobile-agenda-exporting
    (run-with-idle-timer 1 nil #'my/export-mobile-agenda-minimal)))

;; Install the hook locally on the agenda buffer each time it is created.
;; Using add-hook with LOCAL=t means it only lives on *Org Agenda* and fires
;; once when that buffer is killed — not on f/b/r or any other redraw.
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook #'my/mobile-agenda-auto-export nil t)))

;;; ============================================================================
;;; JSON-Based Mobile Sync - Status Changes from Mobile App
;;; ============================================================================

(defun my/mobile-sync-parse-iso8601 (iso-string)
  "Parse ISO8601 timestamp string to Emacs time value.
Example: \"2026-03-02T14:20:00.767-06:00\" → (26021 12345)
Strips sub-second milliseconds (e.g. .NNN) before parsing, as
`parse-time-string' does not handle them.
Returns nil if parsing fails."
  (when (and iso-string (stringp iso-string))
    (condition-case nil
        (let ((cleaned (replace-regexp-in-string "\\.[0-9]+" "" iso-string)))
          (encode-time (parse-time-string cleaned)))
      (error
       (message "Warning: Failed to parse timestamp: %s" iso-string)
       nil))))

(defun my/mobile-sync--log-state-change (new-status old-status timestamp)
  "Insert a state-change logbook entry using native org machinery.
`cl-letf' overrides `current-time' so that `org-store-log-note' stamps the
entry with the mobile TIMESTAMP rather than now.
When `org-add-log-setup' is called with how = \\='time, `org-add-log-note'
calls `org-store-log-note' immediately (no interactive buffer)."
  (cl-letf (((symbol-function 'current-time) (lambda () timestamp)))
    (org-add-log-setup 'state new-status (or old-status "") 'time)
    (org-add-log-note)))

(defun my/mobile-sync--insert-clock (start-ts end-ts)
  "Insert a completed CLOCK entry via `org-clock-in'/`org-clock-out'.
Both Toggl hooks are stripped for the duration using `remq' — no global
mutation.  `org-clock-in-switch-to-state' is nil to prevent internal
`org-todo' calls."
  (let ((org-clock-in-hook             (remq 'rsr/toggl-clock-in-hook
                                        (remq 'org-toggl-clock-in org-clock-in-hook)))
        (org-clock-out-hook            (remq 'org-toggl-clock-out org-clock-out-hook))
        (org-clock-in-switch-to-state  nil)
        (org-clock-out-switch-to-state nil))
    (org-clock-in nil start-ts)
    (org-clock-out nil t end-ts)))

(defun my/mobile-sync-update-entry-native (marker events)
  "Apply sorted EVENTS to the org entry at MARKER.
Each event is an alist with keys: status, changed_at, file.

- Guard:      `my/mobile-sync-in-progress' is t for the entire body, so
              clock/Toggl/refile hooks that check it are silenced.
- TODO state: `org-todo' with `org-inhibit-logging' t (no auto log) then
              `my/mobile-sync--log-state-change' for the logbook entry.
- CLOCK:      `org-clock-in'/out with Toggl hooks stripped via `remq'.
- CLOSED:     `org-add-planning-info' for DONE/CANCELED."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let ((my/mobile-sync-in-progress t)
            (pending-clock-start nil))

        (dolist (event events)
          (let* ((new-status (alist-get 'status     event))
                 (ts-str     (alist-get 'changed_at event))
                 (timestamp  (my/mobile-sync-parse-iso8601 ts-str)))

            (unless timestamp
              (error "Invalid timestamp: %s" ts-str))

            ;; Close any open clock before state change
            (when pending-clock-start
              (my/mobile-sync--insert-clock pending-clock-start timestamp)
              (setq pending-clock-start nil))

            (when (string= new-status "IN-PROGRESS")
              (setq pending-clock-start timestamp))

            ;; 1. Update TODO keyword via org-todo (no auto log)
            (goto-char marker)
            (org-back-to-heading t)
            (let ((old-status (org-get-todo-state))
                  (org-inhibit-logging t))
              (org-todo new-status)
              ;; 2. State log at correct position using native org machinery
              (my/mobile-sync--log-state-change new-status old-status timestamp)
              ;; 3. CLOSED timestamp for terminal states
              (when (member new-status '("DONE" "CANCELED"))
                (org-add-planning-info 'closed timestamp)))))

        (when pending-clock-start
          (message "phone-sync: unclosed IN-PROGRESS for \"%s\" — waiting for end event"
                   (org-get-heading t t t t))))

      (org-update-parent-todo-statistics)
      (save-buffer))))

(defun my/mobile-sync--load-json-files (dir)
  "Return parsed alists for all .json files in DIR, each with an added 'file key."
  (when (file-directory-p dir)
    (delq nil
          (mapcar (lambda (f)
                    (condition-case err
                        (let ((data (json-read-file f)))
                          (push (cons 'file f) data)
                          data)
                      (error
                       (message "phone-sync: cannot parse %s: %s"
                                f (error-message-string err))
                       nil)))
                  (directory-files dir t "\\.json$")))))

(defun my/mobile-sync--group-by-id (records)
  "Group RECORDS (alists) by their 'id field. Returns a hash table id→list."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (r records)
      (let ((id (alist-get 'id r)))
        (puthash id (append (gethash id table) (list r)) table)))
    table))

(defun my/mobile-sync--sort-events (events)
  "Sort EVENTS oldest-first by their 'changed_at timestamp string."
  (sort (copy-sequence events)
        (lambda (a b)
          (let ((ta (my/mobile-sync-parse-iso8601 (alist-get 'changed_at a)))
                (tb (my/mobile-sync-parse-iso8601 (alist-get 'changed_at b))))
            (time-less-p ta tb)))))

(defun my/mobile-sync-from-json ()
  "Process JSON sync files from mobile app.

Groups files by org ID, sorts each group by timestamp, then applies
all state transitions in order — deriving CLOCK intervals from consecutive
IN-PROGRESS → other-state pairs.

Reads from `my/mobile-sync-dir' and `my/mobile-sync-failed-dir'.
Successfully processed files are deleted; failures move to failed/ with .error logs.

Usage: M-m s s"
  (interactive)
  (unless (file-directory-p my/mobile-sync-dir)
    (user-error "Sync directory does not exist: %s" my/mobile-sync-dir))
  (unless (file-directory-p my/mobile-sync-failed-dir)
    (make-directory my/mobile-sync-failed-dir t))

  (let* ((all-records (append
                       (my/mobile-sync--load-json-files my/mobile-sync-dir)
                       (my/mobile-sync--load-json-files my/mobile-sync-failed-dir)))
         (total (length all-records)))

    (if (zerop total)
        (message "phone-sync: nothing to process")

      (org-id-update-id-locations)
      (let ((groups  (my/mobile-sync--group-by-id all-records))
            (success 0)
            (failed  0))

        (maphash
         (lambda (org-id events)
           (condition-case err
               (let ((marker (org-id-find org-id t)))
                 (unless marker
                   (error "No org entry for ID: %s" org-id))
                 (my/mobile-sync-update-entry-native
                  marker
                  (my/mobile-sync--sort-events events))
                 ;; Delete all files for this ID
                 (dolist (ev events)
                   (let ((f (alist-get 'file ev)))
                     (when (file-exists-p f) (delete-file f))))
                 (cl-incf success))
             (error
              (dolist (ev events)
                (let* ((f        (alist-get 'file ev))
                       (basename (file-name-nondirectory f))
                       (dest     (expand-file-name basename my/mobile-sync-failed-dir))
                       (errfile  (concat dest ".error")))
                  (when (file-exists-p f)
                    (rename-file f dest t))
                  (with-temp-file errfile
                    (insert (format "Error: %s\nTime: %s\nFile: %s\n"
                                    (error-message-string err)
                                    (format-time-string "%Y-%m-%d %H:%M:%S")
                                    basename)))))
              (message "phone-sync: failed %s — %s" org-id (error-message-string err))
              (cl-incf failed))))
         groups)

        (message "phone-sync: %d task(s) updated, %d failed" success failed)
        (list success failed)))))

;;; ============================================================================
;;; Keybindings
;;; ============================================================================

;; Global keybindings - M-m s prefix for "Sync"

  (define-key rsr/global-prefix-map (kbd "s e") #'my/export-mobile-agenda-minimal)
  (define-key rsr/global-prefix-map (kbd "s s") #'my/mobile-sync-from-json)

(provide 'mobile-agenda-sync)

;;; mobile-agenda-sync.el ends here
