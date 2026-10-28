;;; gcal.el --- Google Calendar integration -*- lexical-binding: t; -*-

(require 'epa-file)
(epa-file-enable)
(setq epg-pinentry-mode 'loopback)
(condition-case err
    (load "~/.emacs.d/secrets.el.gpg")
  (error (message "gcal: failed to load secrets (wrong passphrase?): %s" err)))
(require 'paths)

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

(defun my/gcal--token-args (args)
  "Swap the calendar ID in ARGS for its account, so tokens are per account."
  (cons (my/gcal--account-for (car args)) (cdr args)))

(defun my/gcal--quiet-archive (fn &rest args)
  "Call FN with ARGS without one notification per archived event."
  (let ((org-gcal-notify-p nil))
    (apply fn args)))

(use-package org-gcal
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
  (advice-add 'org-gcal--get-access-token :filter-args #'my/gcal--token-args)
  (advice-add 'org-gcal--refresh-token :filter-args #'my/gcal--token-args)
  (advice-add 'org-gcal--archive-old-event :around #'my/gcal--quiet-archive))

(defvar my/gcal--sync-started nil
  "Time the last `my/sync-google-calendar' started.")

(defun my/sync-google-calendar ()
  "Sync Google Calendar; quiet unless called interactively.
A sync lock older than 10 minutes is treated as left by a failed sync."
  (interactive)
  (when (featurep 'org-gcal)
    (when (and org-gcal--sync-lock my/gcal--sync-started
               (> (float-time (time-since my/gcal--sync-started)) 600))
      (org-gcal--sync-unlock))
    (setq my/gcal--sync-started (current-time))
    (condition-case err
        (org-gcal-sync nil (not (called-interactively-p 'any)))
      (error (message "Google Calendar sync failed: %s"
                      (error-message-string err))))))

(defun my/gcal-reauth (label)
  "Forget the OAuth token of calendar LABEL's account.
The next sync opens the Google login for that account."
  (interactive
   (list (completing-read "Re-authenticate calendar: "
                          (mapcar (lambda (c) (plist-get c :label)) my/gcal-calendars)
                          nil t)))
  (require 'oauth2-auto)
  (let* ((account (plist-get (my/gcal--calendar :label label) :account))
         (id (oauth2-auto--compute-id account 'org-gcal))
         (store (plstore-open oauth2-auto-plstore)))
    (unwind-protect
        (progn
          ;; Decrypt first, or saving would keep the old ciphertext.
          (plstore-get store id)
          (plstore-delete store id)
          (plstore-save store))
      (plstore-close store))
    (rsr/clear-plstore-cache)
    (message "Forgot the token for %s; the next sync opens the Google login" account)))

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
    (org-gcal-post-at-point)
    (message "Pushing to %s: %s" cal-id (org-get-heading t t t t))))

(defun my/gcal-push-at-point ()
  "Put the entry at point on Google Calendar, in place.
Works in Org buffers and in the agenda.  Asks which calendar the first
time; later pushes update the same event."
  (interactive)
  (let ((marker (cond ((derived-mode-p 'org-agenda-mode)
                       (or (org-get-at-bol 'org-hd-marker)
                           (user-error "No Org entry on this line")))
                      ((derived-mode-p 'org-mode) (point-marker))
                      (t (user-error "Not in an Org or agenda buffer")))))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        (my/gcal--push-heading)))))

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

(define-prefix-command 'my/gcal-map)
(global-set-key (kbd "C-c g") 'my/gcal-map)
(define-key my/gcal-map (kbd "p") #'my/gcal-push-at-point)
(define-key my/gcal-map (kbd "s") #'my/sync-google-calendar)
(define-key my/gcal-map (kbd "e") #'my/gcal-push-event)
(define-key my/gcal-map (kbd "o") #'my/gcal-open-calendar)
(define-key my/gcal-map (kbd "r") #'my/gcal-reauth)

;; Sync every 30 minutes, waiting for 30 idle seconds so it never interrupts
;; typing. Cancel first so reloading this file does not stack timers.
(defvar my/gcal--timer nil
  "Repeating timer that schedules the background sync.")
(when (timerp my/gcal--timer)
  (cancel-timer my/gcal--timer))
(setq my/gcal--timer
      (run-with-timer 1800 1800
                      (lambda () (run-with-idle-timer 30 nil #'my/sync-google-calendar))))

(provide 'gcal)
;;; gcal.el ends here
