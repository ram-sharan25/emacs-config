;;; gcal.el --- Google Calendar integration -*- lexical-binding: t; -*-

(require 'epa-file)
(epa-file-enable)
;; Preserve Mac pinentry behavior; use the desktop agent on Linux.
(setq epg-pinentry-mode (unless (eq system-type 'gnu/linux) 'loopback))
(require 'paths)
;; Toggl and gptel read their keys from here too, at load time.
(unless (or noninteractive (featurep 'my-secrets))
  (my/load-secrets))

(make-directory my/gcal-calendars-dir t)

(defun my/gcal--calendar (key value)
  "Return the entry of `my/gcal-calendars' whose KEY equals VALUE."
  (seq-find (lambda (c) (equal (plist-get c key) value)) my/gcal-calendars))

(defun my/gcal--account-for (calendar-id)
  "Return the Google account whose token serves CALENDAR-ID.
\"primary\" (used by events synced before calendars had real IDs)
maps to the default calendar's account; unknown IDs map to themselves."
  (or (plist-get (my/gcal--calendar :id calendar-id) :account)
      (and (equal calendar-id "primary")
           (plist-get (my/gcal--calendar :label my/gcal-default-calendar) :account))
      calendar-id))

(defun my/gcal--label-for (calendar-id)
  "The label of CALENDAR-ID in `my/gcal-calendars', for messages."
  (or (plist-get (my/gcal--calendar :id calendar-id) :label) calendar-id))

(defun my/gcal--quiet-archive (fn &rest args)
  "Call FN with ARGS without one notification per archived event."
  (let ((org-gcal-notify-p nil))
    (apply fn args)))

(use-package org-gcal
  :if (and (bound-and-true-p my/google-client-id)
           (bound-and-true-p my/google-client-secret))
  :ensure t
  :init
  (setq plstore-encrypt-to nil
        plstore-cache-passphrase-for-symmetric-encryption t)
  ;; Clear a wrong cached passphrase and re-prompt.
  (defun rsr/clear-plstore-cache ()
    "Clear cached plstore and OAuth data so the passphrase is requested again."
    (interactive)
    (setq plstore-passphrase-alist nil)
    (when (boundp 'oauth2-auto--plstore-cache)
      (clrhash oauth2-auto--plstore-cache))
    (message "Plstore passphrase cache cleared; retry the operation"))
  :custom
  (org-gcal-client-id my/google-client-id)
  (org-gcal-client-secret my/google-client-secret)
  (org-gcal-down-days 90)
  (org-gcal-up-days 30)
  (org-gcal-fetch-file-alist
   (mapcar (lambda (c) (cons (plist-get c :id) (plist-get c :file)))
           my/gcal-calendars))
  (org-gcal-local-timezone "America/Chicago")
  (org-gcal-cancelled-todo-keyword "CANCELED")
  ;; Keep headings of events cancelled in Google; never delete Org text.
  (org-gcal-remove-api-cancelled-events nil)
  (org-gcal-strip-html-descriptions t)
  :config
  (advice-add 'org-gcal--archive-old-event :around #'my/gcal--quiet-archive)
  ;; Bounded requests, one run at a time, tokens refreshed before a run
  ;; without blocking, and no browser outside `my/gcal-reauth'.
  (require 'gcal-resilience)
  (setq my/gcal-account-function #'my/gcal--account-for
        my/gcal-calendar-label-function #'my/gcal--label-for)
  (my/gcal-resilience-install))

(defun my/sync-google-calendar ()
  "Sync Google Calendar; quiet unless called interactively.
A sync already running is reported, not started again."
  (interactive)
  (when (featurep 'org-gcal)
    (my/gcal-sync nil (not (called-interactively-p 'any)))))

(defun my/gcal-reauth (label)
  "Sign in again to calendar LABEL's Google account in the browser.
This is the only place a browser opens; syncs report when it is needed."
  (interactive
   (list (completing-read "Re-authenticate calendar: "
                          (mapcar (lambda (c) (plist-get c :label)) my/gcal-calendars)
                          nil t)))
  (require 'oauth2-auto)
  (rsr/clear-plstore-cache)
  (let ((account (plist-get (my/gcal--calendar :label label) :account)))
    ;; Asynchronous: Emacs stays usable while you sign in.
    (aio-listen (oauth2-auto-force-reauth account 'org-gcal)
                (lambda (result)
                  (condition-case err
                      (progn (funcall result)
                             (message "Google Calendar: signed in to %s; tokens saved" account))
                    (error (message "Google Calendar: sign-in to %s failed: %s"
                                    account (my/gcal--sanitize (error-message-string err)))))))
    (message "Google Calendar: finish signing in to %s in your browser" account)))

(defvar org-time-was-given)
(defvar org-end-time-was-given)

(defun my/gcal--read-calendar ()
  "Ask for one of `my/gcal-calendars' and return its plist."
  (let* ((choices (mapcar (lambda (c)
                            (cons (format "%s (%s)" (plist-get c :label) (plist-get c :id))
                                  c))
                          my/gcal-calendars))
         (default (car (rassoc (my/gcal--calendar :label my/gcal-default-calendar)
                               choices))))
    (cdr (assoc (completing-read "Calendar: " choices nil t nil nil default)
                choices))))

(defun my/gcal--body-notes ()
  "Return the notes under the heading at point, or nil if there are none.
Drawers, planning lines and child headings are left out."
  (save-excursion
    (org-back-to-heading t)
    (let* ((end (save-excursion (outline-next-heading) (point)))
           (beg (min end (line-beginning-position 2)))
           (text (concat (buffer-substring-no-properties beg end) "\n")))
      (setq text (replace-regexp-in-string
                  "^[ \t]*:[-_[:alnum:]]+:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n" "" text))
      (setq text (replace-regexp-in-string
                  "^[ \t]*\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):.*\n" "" text))
      (setq text (string-trim text))
      (unless (string-empty-p text) text))))

(defun my/gcal--set-description (desc)
  "Make DESC the event description of the heading at point.
org-gcal sends only what is inside the :org-gcal: drawer, so replace it."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (when (re-search-forward
             (format "^[ \t]*:%s:[^z-a]*?\n[ \t]*:END:[ \t]*\n?"
                     (regexp-quote org-gcal-drawer-name))
             end t)
        (replace-match "" t t)))
    (org-back-to-heading t)
    (re-search-forward org-property-drawer-re
                       (save-excursion (outline-next-heading) (point)))
    (insert (format "\n:%s:\n%s\n:END:" org-gcal-drawer-name
                    ;; org-gcal turns a leading ✱ back into * when reading.
                    (replace-regexp-in-string "^\\*" "✱" desc)))))

(defun my/gcal--ensure-end-time ()
  "Ask for a duration when SCHEDULED has a start time but no end time.
Otherwise org-gcal would create a zero-length event."
  (let ((ts (org-element-property :scheduled (org-element-at-point))))
    (when (and (org-element-property :hour-start ts)
               (equal (org-element-property :hour-start ts)
                      (org-element-property :hour-end ts))
               (equal (org-element-property :minute-start ts)
                      (org-element-property :minute-end ts)))
      (let* ((minutes (org-duration-to-minutes
                       (read-string "Duration (H:MM): " nil nil
                                    (or (org-entry-get nil "EFFORT") "1:00"))))
             (start (org-timestamp-to-time ts))
             (end (time-add start (round (* 60 minutes)))))
        (org-schedule nil (format "%s-%s"
                                  (format-time-string "%Y-%m-%d %H:%M" start)
                                  (format-time-string "%H:%M" end)))))))

(defun my/gcal--push-heading (&optional calendar)
  "Push the heading at point to Google Calendar.
Use the calendar it is already linked to, else CALENDAR, else ask.
The event takes its time from SCHEDULED and its description from the
notes under the heading; events pulled from Google keep theirs."
  ;; Refuse before the prompts, not after them.
  (when my/gcal--run
    (user-error "Google Calendar %s is still running; push when it finishes"
                (my/gcal-run-kind my/gcal--run)))
  ;; Pulled events keep their time in the :org-gcal: drawer, not SCHEDULED.
  (unless (plist-get (org-gcal--get-time-and-desc) :start)
    (call-interactively #'org-schedule)
    (unless (org-entry-get nil "SCHEDULED")
      (user-error "Not scheduled; nothing pushed")))
  (my/gcal--ensure-end-time)
  (let ((cal-id (or (org-entry-get nil org-gcal-calendar-id-property)
                    (plist-get (or calendar (my/gcal--read-calendar)) :id)))
        (notes (my/gcal--body-notes)))
    (org-entry-put nil org-gcal-calendar-id-property cal-id)
    (when (and notes
               (not (equal (org-entry-get nil org-gcal-managed-property) "gcal")))
      (my/gcal--set-description notes))
    (save-buffer)
    (my/gcal-user-command #'org-gcal-post-at-point)
    (message "Pushing to %s: %s" cal-id (org-get-heading t t t t))))

(declare-function org-timeblock-selected-block-marker "org-timeblock")
(declare-function org-timeblock-redraw-buffers "org-timeblock")

(defun my/gcal--entry-marker-at-point ()
  "Marker of the Org entry at point.
Works in Org buffers, the agenda, the timeblock grid and its list."
  (cond ((derived-mode-p 'org-agenda-mode)
         (or (org-get-at-bol 'org-hd-marker)
             (user-error "No Org entry on this line")))
        ((derived-mode-p 'org-mode) (point-marker))
        ((derived-mode-p 'org-timeblock-mode)
         (or (org-timeblock-selected-block-marker)
             (user-error "No block selected; move onto one with n or p")))
        ((derived-mode-p 'org-timeblock-list-mode)
         (or (get-text-property (line-beginning-position) 'marker)
             (user-error "No Org entry on this line")))
        (t (user-error "Not in an Org, agenda or timeblock buffer"))))

(defun my/gcal-push-at-point ()
  "Put the entry at point on Google Calendar, in place.
Works in Org buffers, the agenda and the timeblock views (also on P
there).  Asks which calendar the first time; later pushes update the
same event.  A timeblock view is redrawn, since the push may have added
an end time."
  (interactive)
  (let ((marker (my/gcal--entry-marker-at-point))
        (timeblock (derived-mode-p 'org-timeblock-mode 'org-timeblock-list-mode)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        (my/gcal--push-heading)))
    (when timeblock
      (org-timeblock-redraw-buffers))))

;; P in the timeblock grid and list: push the block at point.
(with-eval-after-load 'org-timeblock
  (keymap-set org-timeblock-mode-map "P" #'my/gcal-push-at-point)
  (keymap-set org-timeblock-list-mode-map "P" #'my/gcal-push-at-point))

(defun my/gcal-push-event ()
  "Create a new event on a chosen calendar and push it.
The event is added to that calendar's file."
  (interactive)
  (let* ((cal (my/gcal--read-calendar))
         (title (read-string "Title: "))
         (when-string (let (org-time-was-given org-end-time-was-given)
                        (let ((s (org-read-date nil nil nil "When (e.g. fri 10:00-11:00): ")))
                          (if org-end-time-was-given
                              (concat s "-" org-end-time-was-given)
                            s))))
         (desc (string-trim (read-string "Description (optional): "))))
    (when (string-empty-p (string-trim title))
      (user-error "Title is empty"))
    (with-current-buffer (find-file-noselect (plist-get cal :file))
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " title "\n")
        (unless (string-empty-p desc) (insert desc "\n"))
        (org-back-to-heading t)
        (org-schedule nil when-string)
        (my/gcal--push-heading cal)))))

(defun my/gcal-open-calendar (label)
  "Open the Org file of calendar LABEL."
  (interactive
   (list (completing-read "Open calendar: "
                          (mapcar (lambda (c) (plist-get c :label)) my/gcal-calendars)
                          nil t)))
  (find-file (plist-get (my/gcal--calendar :label label) :file)))

;; C-c g p / s / e / o / r: `my/gcal-map' in keymaps.el.

;; Sync every 30 minutes, waiting for 30 idle seconds so it never interrupts
;; typing; one idle wait at a time.  Cancel first so reloading this file does
;; not stack timers.
(defvar my/gcal--timer nil
  "Repeating timer that schedules the background sync.")
(when (timerp my/gcal--timer)
  (cancel-timer my/gcal--timer))
(setq my/gcal--timer
      (run-with-timer 1800 1800
                      (lambda () (my/gcal-schedule-sync #'my/sync-google-calendar))))

(provide 'gcal)
;;; gcal.el ends here
