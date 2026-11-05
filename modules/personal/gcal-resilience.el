;;; gcal-resilience.el --- Bounded, reported Google Calendar requests -*- lexical-binding: t; -*-

;; org-gcal sends requests with no timeout, waits for OAuth refreshes by
;; blocking Emacs, and releases its sync lock long before a sync is done.
;; This module owns every Google request and every sync run instead:
;; requests time out and report sanitized errors, one run happens at a time
;; under a 2-minute cap, tokens are refreshed before a run without blocking,
;; and writes whose outcome is unknown are checked at the next run.
;; `my/gcal-resilience-install' wires it into org-gcal; gcal.el calls it.

(require 'cl-lib)
(require 'subr-x)
(require 'deferred)
(require 'org-macs)

(defvar org-gcal--sync-lock)

(defvar my/gcal-request-timeout 15
  "Seconds before a Google request is cancelled.")

(defvar my/gcal-google-url-regexp
  "\\`https://\\(?:www\\|oauth2\\)\\.googleapis\\.com/"
  "URLs whose requests this module bounds and reports.")

(defvar my/gcal-run-timeout 120
  "Seconds before a sync run, or a single push or delete, is stopped.")

(defvar my/gcal-token-margin 600
  "Refresh a token before a sync when it has fewer seconds than this left.")

(defvar my/gcal-single-op-token-margin 120
  "Refresh a token before a single push or delete below this many seconds.")

(defvar my/gcal-account-function #'identity
  "Function mapping a calendar ID to the Google account whose token serves it.")

(defvar my/gcal-calendar-label-function #'identity
  "Function mapping a calendar ID to the name used in messages.")

(defvar my/gcal--run nil
  "The run in progress, a `my/gcal-run', or nil.")

(defvar my/gcal--write nil
  "Plist (:op :calendar :event :marker) of the write being sent, while it is sent.")

(defvar my/gcal--op-run nil
  "The run a push or delete began, while that push or delete runs.")

(defvar my/gcal--user-op nil
  "Non-nil while a command the user started runs; such a command never joins a run.")

(defvar my/gcal-unknown-property "org-gcal-outcome-unknown"
  "Property marking an entry whose last write may or may not have reached Google.
Its value is \"OP CALENDAR-ID EVENT-ID TIME\".")

(defvar my/gcal-sync-hash-property "GCAL_SYNC_HASH"
  "Org property holding the last Google-synchronized event payload hash.")

(cl-defstruct (my/gcal-run (:constructor my/gcal-run-create) (:copier nil))
  kind started timer requests pending failed-accounts pushed)

;;;; Diagnostics

(defun my/gcal--label (calendar-id)
  "CALENDAR-ID's name for messages; the ID itself if naming it fails."
  (or (ignore-errors (funcall my/gcal-calendar-label-function calendar-id)) calendar-id))

(defun my/gcal--sanitize (string)
  "Return STRING with OAuth tokens, secrets and auth headers redacted."
  (let ((case-fold-search t)
        (s string))
    (dolist (re '("\\(access_token\\|refresh_token\\|client_secret\\|code\\)=[^&[:space:]\"')]+"
                  "\\(\"?\\(?:access_token\\|refresh_token\\|client_secret\\)\"?\\s-*[:.]\\s-*\\)\"[^\"]*\""
                  "\\(:\\(?:access\\|refresh\\)-token\\s-+\\)\"[^\"]*\""
                  "\\(Bearer\\)\\s-+[^[:space:]\"')]+"))
      (setq s (replace-regexp-in-string
               re (lambda (m)
                    (save-match-data
                      (cond ((string-match "\\`\\(:[a-z-]+\\s-+\\)\"" m)
                             (concat (match-string 1 m) "\"REDACTED\""))
                            ((string-match "\\`\\(\"?[a-z_]+\"?\\s-*[:.]\\s-*\\)\"" m)
                             (concat (match-string 1 m) "\"REDACTED\""))
                            ((string-match "\\`\\([a-z_]+\\)=" m)
                             (concat (match-string 1 m) "=REDACTED"))
                            (t "Bearer REDACTED"))))
               s t t)))
    s))

(defun my/gcal--url-parts (url)
  "Return (CALENDAR-ID . EVENT-ID) from a Calendar API URL, or nil."
  (when (string-match "/calendars/\\([^/?]+\\)/events\\(?:/\\([^/?]+\\)\\)?" url)
    ;; Read both groups first: `url-unhex-string' clobbers the match data.
    (let ((calendar (match-string 1 url))
          (event (match-string 2 url)))
      (cons (url-unhex-string calendar) (and event (url-unhex-string event))))))

(defun my/gcal--describe-request (method url)
  "Short description of a request, e.g. \"PATCH event ev1 on Personal\"."
  (let ((parts (my/gcal--url-parts url)))
    (cond ((null parts) (format "%s %s" method
                                (if (string-match-p "/token" url) "token" "request")))
          ((cdr parts) (format "%s event %s on %s" method (cdr parts) (car parts)))
          (t (format "%s events on %s" method (car parts))))))

(defun my/gcal--google-error (data)
  "Google's \"reason: message\" from parsed error DATA, or nil."
  (let* ((err (and (listp data) (plist-get data :error)))
         (err (if (stringp err) (list :message err) err))
         (errors (plist-get err :errors))
         (first (if (vectorp errors) (and (> (length errors) 0) (aref errors 0)) (car errors)))
         (reason (or (plist-get first :reason) (plist-get err :status)))
         (msg (or (plist-get err :message) (plist-get data :error_description))))
    (when (or reason msg)
      (string-join (delq nil (list reason msg)) ": "))))

(defun my/gcal--report-failure (method url response)
  "Report a failed Google request RESPONSE once, without credentials."
  (let* ((status (request-response-status-code response))
         (symbol (request-response-symbol-status response))
         (detail (my/gcal--google-error (request-response-data response)))
         (thrown (request-response-error-thrown response)))
    (message "%s"
             (my/gcal--sanitize
              (format "Google Calendar: %s %s"
                      (my/gcal--describe-request method url)
                      (cond ((eq symbol 'timeout)
                             (format "timed out after %ss" my/gcal-request-timeout))
                            ((eq symbol 'abort) "was cancelled")
                            (status (format "failed: HTTP %s%s" status
                                            (if detail (concat " " detail) "")))
                            (t (format "failed: no response (%s)"
                                       (if (consp thrown)
                                           (string-trim (format "%s" (cdr thrown)))
                                         thrown)))))))))

;;;; Requests

(defun my/gcal--google-url-p (url)
  (and (stringp url) (string-match-p my/gcal-google-url-regexp url)))

(defun my/gcal--request-advice (orig url &rest settings)
  "Around `request': bound and report Google requests made with URL and SETTINGS."
  (if (not (my/gcal--google-url-p url))
      (apply orig url settings)
    (let* ((method (or (plist-get settings :type) "GET"))
           (quiet (plist-get settings :my/gcal-quiet))
           (run my/gcal--run)
           (current-p (lambda () (or (null run) (eq run my/gcal--run)))))
      (unless (plist-get settings :timeout)
        (setq settings (plist-put settings :timeout my/gcal-request-timeout)))
      ;; A run that has ended (finished or capped) gets no callbacks, so a
      ;; late response never edits Org buffers after the run released them.
      (dolist (key '(:success :complete))
        (when-let ((cb (plist-get settings key)))
          (setq settings (plist-put settings key
                                    (lambda (&rest args)
                                      (when (funcall current-p) (apply cb args)))))))
      ;; Always pass :error, so request.el never logs the URL (and its token).
      (let ((user-error-cb (plist-get settings :error)))
        (setq settings
              (plist-put settings :error
                         (cl-function
                          (lambda (&rest args &key response &allow-other-keys)
                            (when (funcall current-p)
                              (unless quiet (my/gcal--report-failure method url response))
                              (when user-error-cb (apply user-error-cb args))))))))
      ;; A write that may have reached Google is marked, never resent.  A
      ;; create carries its own event ID so that it can be looked up later.
      (when-let ((write (and (member method '("POST" "PATCH" "PUT" "DELETE")) my/gcal--write)))
        (when (and (equal (plist-get write :op) "create") (stringp (plist-get settings :data)))
          (let ((id (my/gcal--new-event-id)))
            (setq write (plist-put (copy-sequence write) :event id)
                  settings (plist-put settings :data
                                      (my/gcal--add-event-id (plist-get settings :data) id)))))
        (let ((cb (plist-get settings :complete)))
          (setq settings
                (plist-put settings :complete
                           (cl-function
                            (lambda (&rest args &key response &allow-other-keys)
                              (when-let ((why (my/gcal--unknown-outcome response)))
                                (my/gcal--mark-unknown write why))
                              (when cb (apply cb args))))))))
      (let ((response (apply orig url settings)))
        (when run (push response (my/gcal-run-requests run)))
        response))))

;;;; Runs
;; One run at a time: a full sync, or a single push or delete.  The run, not
;; org-gcal's lock, decides whether Google work is in progress; its end is
;; the end of its last request, or the cap.

(defun my/gcal--run-begin (kind)
  "Start a run of KIND (a string), or refuse with a message and return nil."
  (if-let ((run my/gcal--run))
      (progn
        (message "Google Calendar %s not started: a %s started %ds ago is still running"
                 kind (my/gcal-run-kind run)
                 (round (float-time (time-since (my/gcal-run-started run)))))
        nil)
    (when (bound-and-true-p org-gcal--sync-lock)
      (message "Google Calendar: cleared an org-gcal sync lock left by an earlier run"))
    (let ((run (my/gcal-run-create :kind kind :started (current-time))))
      (setf (my/gcal-run-timer run)
            (run-at-time my/gcal-run-timeout nil #'my/gcal--run-timeout run))
      (setq my/gcal--run run
            org-gcal--sync-lock t)
      run)))

(defun my/gcal--run-end (run)
  "End RUN: release the lock, cancel the cap, abort live requests.
Does nothing when RUN is not the current run, so it is safe to call twice."
  (when (and run (eq run my/gcal--run))
    (setq my/gcal--run nil
          org-gcal--sync-lock nil)
    (when (timerp (my/gcal-run-timer run))
      (cancel-timer (my/gcal-run-timer run)))
    (dolist (response (my/gcal-run-requests run))
      (unless (request-response-done-p response)
        (condition-case nil (request-abort response) (error nil))))
    t))

(defun my/gcal--run-timeout (run)
  "Stop RUN at the cap and say what it did not finish."
  (when (eq run my/gcal--run)
    (unwind-protect
        (message "Google Calendar %s stopped after %ss%s; the next scheduled sync will try again"
                 (my/gcal-run-kind run) my/gcal-run-timeout
                 (if-let ((pending (my/gcal-run-pending run)))
                     (format "; not finished: %s"
                             (mapconcat #'my/gcal--label pending ", "))
                   ""))
      (my/gcal--run-end run))))

(defvar my/gcal--busy 0
  "Depth of org-gcal write calls and token reads now on the stack.
Reading a token decrypts it with gpg, and timers run while gpg works; a
write in that state has not sent its request yet, so the run is not over.")

(defmacro my/gcal--while-busy (&rest body)
  "Run BODY counted in `my/gcal--busy'."
  (declare (indent 0))
  `(progn (cl-incf my/gcal--busy)
          (unwind-protect (progn ,@body) (cl-decf my/gcal--busy))))

(defun my/gcal--end-when-quiet (run)
  "End RUN once nothing of it is left: no request in flight, no org-gcal call
part-way to sending one, and no queued deferred step (org-gcal drops some of
its own chains, so their later steps show up only there)."
  (when (eq run my/gcal--run)
    (if (or (> my/gcal--busy 0)
            (bound-and-true-p deferred:queue)
            (seq-some (lambda (r) (not (request-response-done-p r))) (my/gcal-run-requests run)))
        (run-at-time 0.1 nil #'my/gcal--end-when-quiet run)
      (my/gcal--run-end run))))

(defun my/gcal--deferred (value)
  "VALUE as a deferred, wrapping it when it is not one."
  (if (deferred-p value) value (deferred:succeed value)))

(defun my/gcal--error-text (err)
  "Sanitized text of ERR, a condition or any other value."
  (my/gcal--sanitize (if (and (consp err) (symbolp (car err)) (get (car err) 'error-conditions))
                         (error-message-string err)
                       (format "%s" err))))

(defun my/gcal--guarded (thunk on-error)
  "Deferred running THUNK; an error from it goes to ON-ERROR and the chain goes on."
  (deferred:try
    (deferred:$ (deferred:succeed nil)
                (deferred:nextc it (lambda (_) (my/gcal--deferred (funcall thunk)))))
    :catch on-error))

(defun my/gcal--each (run items fn)
  "Call FN on each of ITEMS in order, waiting for each; stop when RUN ends."
  (let ((d (deferred:succeed nil)))
    (dolist (item items d)
      (setq d (deferred:nextc d (lambda (_) (when (eq run my/gcal--run) (funcall fn item))))))))

;;;; Sync

(defvar org-gcal-fetch-file-alist)
(defvar org-gcal-entry-id-property)
(defvar org-gcal-drawer-name)
(defvar org-gcal-managed-property)
(defvar org-gcal-auto-archive)
(defvar org-gcal-default-transparency)
(declare-function org-gcal--sync-calendar "org-gcal")
(declare-function org-gcal--notify "org-gcal")
(declare-function org-gcal--get-time-and-desc "org-gcal")
(declare-function org-gcal--headline "org-gcal")
(declare-function org-gcal--source-from-link-string "org-gcal")

(defun my/gcal--plain-string (value)
  "Return VALUE without text properties, leaving non-strings unchanged."
  (if (stringp value) (substring-no-properties value) value))

(defun my/gcal--entry-sync-hash ()
  "Hash the Google Calendar payload represented by the entry at point."
  (save-excursion
    (org-back-to-heading t)
    (let* ((time-desc (org-gcal--get-time-and-desc))
           (recurrence (org-entry-get nil "recurrence"))
           (source-link
            (or (org-entry-get nil "link")
                (car (org-entry-get-multivalued-property nil "ROAM_REFS"))))
           (source (and source-link
                        (ignore-errors
                          (org-gcal--source-from-link-string source-link))))
           (payload
            (list :summary (my/gcal--plain-string (org-gcal--headline))
                  :start (unless recurrence
                           (my/gcal--plain-string (plist-get time-desc :start)))
                  :end (unless recurrence
                         (my/gcal--plain-string (plist-get time-desc :end)))
                  :description (my/gcal--plain-string (plist-get time-desc :desc))
                  :location (my/gcal--plain-string (org-entry-get nil "LOCATION"))
                  :source (if source
                              (list (my/gcal--plain-string (plist-get source :url))
                                    (my/gcal--plain-string (plist-get source :title)))
                            (my/gcal--plain-string source-link))
                  :transparency
                  (my/gcal--plain-string
                   (or (org-entry-get nil "TRANSPARENCY")
                       org-gcal-default-transparency)))))
      (secure-hash 'sha256 (prin1-to-string payload)))))

(defun my/gcal--entry-sync-unchanged-p (&optional marker)
  "Non-nil when MARKER's event payload matches its last synchronized hash.
With nil MARKER, inspect the entry at point."
  (if marker
      (org-with-point-at marker
        (my/gcal--entry-sync-unchanged-p))
    (save-excursion
      (org-back-to-heading t)
      (let ((stored (org-entry-get nil my/gcal-sync-hash-property)))
        (and stored (equal stored (my/gcal--entry-sync-hash)))))))

(defun my/gcal--remember-sync-hash (&rest _)
  "Record the entry's current payload after org-gcal updates it."
  (org-back-to-heading t)
  (org-entry-put nil my/gcal-sync-hash-property (my/gcal--entry-sync-hash)))

(defun my/gcal--fetch-calendar (run calendar skip-export silent up down)
  "Fetch CALENDAR, a (ID . FILE) cons, reporting its failure without stopping RUN."
  (let ((id (car calendar)))
    (if (member (funcall my/gcal-account-function id) (my/gcal-run-failed-accounts run))
        (progn (setf (my/gcal-run-pending run) (delete id (my/gcal-run-pending run)))
               (deferred:succeed nil))
      (deferred:nextc
        (my/gcal--guarded
         (lambda ()
           (deferred:nextc (org-gcal--sync-calendar calendar skip-export silent up down)
             (lambda (_)
               (org-gcal--notify "Completed event fetching ."
                                 (concat "Events fetched into\n" (cdr calendar)) silent))))
         (lambda (err)
           (message "Google Calendar: %s failed: %s"
                    (my/gcal--label id) (my/gcal--error-text err))))
        (lambda (_)
          (setf (my/gcal-run-pending run) (delete id (my/gcal-run-pending run))))))))

(defun my/gcal--push-candidates ()
  "Markers of unfinished Org-managed entries in the sync window.
Completed and canceled tasks remain as historical calendar blocks but are not
updated by the automatic push phase."
  (let ((re (format "^[ \t]*:%s:[ \t]*$" (regexp-quote org-gcal-drawer-name)))
        (up (org-gcal--up-time))
        (down (org-gcal--down-time))
        markers)
    (dolist (file (delete-dups (mapcar #'expand-file-name (org-generic-id-files))))
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file 'nowarn)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             (let* ((time-desc (org-gcal--get-time-and-desc))
                    (start (plist-get time-desc :start))
                    (end (plist-get time-desc :end))
                    (start (and start (org-gcal--parse-calendar-time-string start)))
                    (end (and end (org-gcal--parse-calendar-time-string end))))
               (when (and start end
                          (not (org-entry-is-done-p))
                          (not (time-less-p start up))
                          (not (time-less-p down end))
                          (equal "org" (org-entry-get (point) org-gcal-managed-property)))
                 (push (point-marker) markers))))))))
    (nreverse markers)))

(defun my/gcal--push-phase (run skip-export)
  "Push RUN's Org-managed entries one at a time, as org-gcal's sync means to."
  (org-generic-id-update-id-locations org-gcal-entry-id-property)
  (my/gcal--each
   run (my/gcal--push-candidates)
   (lambda (marker)
     (my/gcal--guarded
      (lambda ()
        ;; The fetch phase already pushed entries Google returned; a second
        ;; write of the same event moments later draws rateLimitExceeded.
        ;; Entries whose synchronized payload hash still matches need no write.
        (unless (or (member (org-gcal--get-id marker) (my/gcal-run-pushed run))
                    (my/gcal--entry-sync-unchanged-p marker))
          (org-with-point-at marker
            (org-gcal-post-at-point nil skip-export (org-gcal--sync-get-update-existing)))))
      (lambda (err)
        (message "Google Calendar: push of %s failed: %s"
                 (org-with-point-at marker (org-get-heading t t t t))
                 (my/gcal--error-text err)))))))

(defun my/gcal-sync (&optional skip-export silent)
  "Sync every calendar as one bounded run; replaces `org-gcal-sync'.
Fetch each calendar in `org-gcal-fetch-file-alist' in turn, then push
Org-managed entries, unless SKIP-EXPORT.  SILENT suppresses org-gcal's
completion notices.  Return the run, or nil when another run is going."
  (interactive)
  (when-let ((run (my/gcal--run-begin "sync")))
    (condition-case err
        (let ((up (org-gcal--up-time))
              (down (org-gcal--down-time))
              (calendars org-gcal-fetch-file-alist))
          (org-generic-id-update-id-locations org-gcal-entry-id-property)
          (when org-gcal-auto-archive
            (dolist (c calendars)
              (with-current-buffer (find-file-noselect (cdr c))
                (org-gcal--archive-old-event))))
          (setf (my/gcal-run-pending run) (mapcar #'car calendars))
          (deferred:$
            (my/gcal--before-run run (delete-dups
                                      (mapcar (lambda (c) (funcall my/gcal-account-function (car c)))
                                              calendars)))
            (deferred:nextc it
              (lambda (_)
                (my/gcal--each run calendars
                               (lambda (c) (my/gcal--fetch-calendar run c skip-export silent up down)))))
            (deferred:nextc it
              (lambda (_) (when (and (eq run my/gcal--run) (not skip-export))
                            (my/gcal--push-phase run skip-export))))
            (deferred:error it
              (lambda (e) (message "Google Calendar sync failed: %s" (my/gcal--error-text e))))
            (deferred:nextc it (lambda (_) (my/gcal--end-when-quiet run))))
          run)
      ((error quit)
       (my/gcal--run-end run)
       (message "Google Calendar sync failed: %s" (my/gcal--error-text err))
       (when (eq (car err) 'quit) (signal 'quit nil))
       nil))))

(defun my/gcal--before-run (run accounts)
  "Deferred work before RUN touches calendars: tokens for ACCOUNTS, unknown writes."
  (deferred:nextc (my/gcal--ensure-tokens run accounts my/gcal-token-margin)
    (lambda (_) (my/gcal--verify-unknown run))))

;;;; Tokens
;; org-gcal waits for oauth2-auto with `aio-wait-for', and oauth2-auto
;; refreshes with `url-retrieve-synchronously' and opens a browser when that
;; fails.  Here org-gcal only ever reads the stored token; the refresh happens
;; before a run, as a bounded asynchronous request, and a browser opens only
;; from `my/gcal-reauth'.

(declare-function oauth2-auto--plstore-read "oauth2-auto")
(declare-function oauth2-auto--plstore-write "oauth2-auto")
(declare-function oauth2-auto--make-plist "oauth2-auto")
(declare-function oauth2-auto--provider-info "oauth2-auto")
(declare-function oauth2-auto--now "oauth2-auto")

(defun my/gcal--token-plist (account)
  "ACCOUNT's stored org-gcal token plist."
  (oauth2-auto--plstore-read account 'org-gcal))

(defun my/gcal--seconds-left (plist)
  "Seconds until PLIST's access token expires, or nil without one."
  (when (and (plist-get plist :access-token) (plist-get plist :expiration))
    (- (plist-get plist :expiration) (oauth2-auto--now))))

(defun my/gcal--access-token (calendar-id)
  "Stored access token for CALENDAR-ID's account; replaces `org-gcal--get-access-token'.
Never refreshes, waits, or opens a browser."
  (let* ((account (funcall my/gcal-account-function calendar-id))
         (plist (my/gcal--while-busy (my/gcal--token-plist account)))
         (left (my/gcal--seconds-left plist)))
    (if (and left (> left 0))
        (plist-get plist :access-token)
      (error "No valid access token for %s; it is refreshed at the start of each sync%s"
             account (if (plist-get plist :refresh-token) "" ", so run M-x my/gcal-reauth")))))

(defun my/gcal--refresh-token (calendar-id)
  "Refuse org-gcal's refresh-and-resend after a 401; replaces `org-gcal--refresh-token'.
The token was refreshed before the run, so a 401 means Google rejected it."
  (error "Google rejected the access token for %s (HTTP 401); not retrying.  If this repeats, run M-x my/gcal-reauth"
         (funcall my/gcal-account-function calendar-id)))

(defun my/gcal--account-calendars (account)
  "Labels of the calendars served by ACCOUNT."
  (mapcar #'my/gcal--label
          (seq-filter (lambda (id) (equal account (funcall my/gcal-account-function id)))
                      (mapcar #'car org-gcal-fetch-file-alist))))

(defun my/gcal--token-failed (run account text)
  "Record that ACCOUNT has no usable token in RUN, and say so once."
  (push account (my/gcal-run-failed-accounts run))
  (message "%s"
           (my/gcal--sanitize
            (if-let ((cals (and (equal (my/gcal-run-kind run) "sync")
                                (my/gcal--account-calendars account))))
                (format "Google Calendar: skipped %s: %s" (string-join cals ", ") text)
              (format "Google Calendar: %s" text)))))

(defun my/gcal--refresh-request (plist)
  "Deferred that refreshes the token in PLIST and resolves to the response."
  (let ((info (oauth2-auto--provider-info 'org-gcal))
        (d (deferred:new #'identity)))
    (request (cdr (assq 'token_url info))
      :type "POST"
      :data `(("client_id" . ,(cdr (assq 'client_id info)))
              ("client_secret" . ,(cdr (assq 'client_secret info)))
              ("refresh_token" . ,(plist-get plist :refresh-token))
              ("grant_type" . "refresh_token"))
      :parser (lambda () (let ((json-object-type 'alist)) (json-read)))
      :my/gcal-quiet t
      :complete (cl-function
                 (lambda (&key response &allow-other-keys)
                   (deferred:callback-post d response))))
    d))

(defun my/gcal--refresh-problem (response)
  "Classify a failed refresh RESPONSE: (KIND . DETAIL), KIND reauth, config or transient."
  (let* ((status (request-response-status-code response))
         (data (request-response-data response))
         (code (and (consp data) (cdr (assq 'error data))))
         (code (and (stringp code) code))
         (desc (and (consp data) (cdr (assq 'error_description data))))
         (detail (cond ((eq (request-response-symbol-status response) 'timeout)
                        (format "timed out after %ss" my/gcal-request-timeout))
                       ((null status) "no response")
                       (t (string-join (delq nil (list (format "HTTP %s" status) code)) " "))))
         (detail (if desc (format "%s: %s" detail desc) detail)))
    (cons (cond ((equal code "invalid_grant") 'reauth)
                ((member code '("invalid_client" "unauthorized_client" "invalid_scope"
                                "invalid_request" "unsupported_grant_type"))
                 'config)
                ((or (null status) (eq status 429) (>= status 500)) 'transient)
                (t 'config))
          detail)))

(defun my/gcal--refresh-done (run account plist response)
  "Store ACCOUNT's refreshed token from RESPONSE, or report why RUN must skip it."
  (let ((data (request-response-data response)))
    (if (and (eq (request-response-symbol-status response) 'success)
             (consp data) (cdr (assq 'access_token data)))
        (condition-case err
            (oauth2-auto--plstore-write account 'org-gcal (oauth2-auto--make-plist data plist))
          (error (my/gcal--token-failed
                  run account (format "could not save the refreshed token for %s: %s"
                                      account (my/gcal--error-text err)))))
      (pcase-let ((`(,kind . ,detail) (my/gcal--refresh-problem response)))
        (my/gcal--token-failed
         run account
         (pcase kind
           ('reauth (format "Google rejected the stored sign-in for %s (%s); run M-x my/gcal-reauth"
                            account detail))
           ('config (format "token refresh for %s was rejected as a configuration problem (%s); check the OAuth client ID, secret and scope"
                            account detail))
           (_ (format "token refresh for %s failed (%s); the next scheduled sync will try again"
                      account detail))))))))

(defun my/gcal--ensure-token (run account margin)
  "Deferred: make ACCOUNT's token last MARGIN more seconds, or mark it failed in RUN."
  (condition-case err
      (let* ((plist (my/gcal--token-plist account))
             (left (my/gcal--seconds-left plist)))
        (cond ((and left (> left margin)) (deferred:succeed nil))
              ((not (plist-get plist :refresh-token))
               (my/gcal--token-failed
                run account (format "%s has no stored sign-in; run M-x my/gcal-reauth" account))
               (deferred:succeed nil))
              (t (deferred:nextc (my/gcal--refresh-request plist)
                   (lambda (response) (my/gcal--refresh-done run account plist response))))))
    (error (my/gcal--token-failed
            run account (format "could not read the token for %s: %s"
                                account (my/gcal--error-text err)))
           (deferred:succeed nil))))

(defun my/gcal--ensure-tokens (run accounts margin)
  "Deferred: `my/gcal--ensure-token' for each of ACCOUNTS in turn."
  (my/gcal--each run accounts (lambda (a) (my/gcal--ensure-token run a margin))))

;;;; Single pushes and deletes
;; A push or delete is a run of its own, so it never overlaps a sync.  Called
;; from inside a run (org-gcal's sync pushes entries itself), it joins it.

(defvar org-gcal-calendar-id-property)
(defvar org-gcal-etag-property)
(defvar org-gcal-remove-api-cancelled-events)
(declare-function org-gcal--update-entry "org-gcal")
(declare-function org-gcal--handle-cancelled-entry "org-gcal")
(declare-function org-gcal-events-url "org-gcal")

(defun my/gcal-user-command (fn &rest args)
  "Call FN with ARGS as a command the user started: refused while a run is going."
  (let ((my/gcal--user-op t))
    (apply fn args)))

(defun my/gcal--entry-marker ()
  "Marker at the heading a post or delete at point acts on, or nil."
  (ignore-errors
    (if (derived-mode-p 'org-agenda-mode)
        (org-get-at-bol 'org-hd-marker)
      (save-excursion (end-of-line) (org-back-to-heading t) (point-marker)))))

(defun my/gcal--heading (marker)
  (org-with-point-at marker (org-get-heading t t t t)))

(defun my/gcal--single-op (kind orig args)
  "Run ORIG with ARGS, a push or delete of KIND, as its own run or within one."
  (let ((entry (my/gcal--entry-marker)))
    (cond
     ((and entry (org-entry-get entry my/gcal-unknown-property))
      (message "Google Calendar %s of \"%s\" not started: its last write has an unknown outcome and will be checked at the next sync"
               kind (my/gcal--heading entry))
      (deferred:succeed nil))
     ((and my/gcal--run (not my/gcal--user-op))
      (apply orig args))
     (t
      (when-let ((run (my/gcal--run-begin kind)))
        (condition-case err
            (let* ((here (point-marker))
                   (cal (and entry (org-entry-get entry org-gcal-calendar-id-property)))
                   (account (and cal (funcall my/gcal-account-function cal)))
                   (left (and account (my/gcal--seconds-left (my/gcal--token-plist account))))
                   (call (lambda ()
                           (let ((my/gcal--op-run run))
                             (with-current-buffer (marker-buffer here)
                               (save-excursion (goto-char here)
                                               (my/gcal--deferred (apply orig args))))))))
              (deferred:$
                ;; With a good token, run ORIG now, inside the user's command,
                ;; so its prompts behave normally.  Otherwise refresh first.
                (if (or (null account) (and left (> left my/gcal-single-op-token-margin)))
                    (funcall call)
                  (deferred:nextc (my/gcal--ensure-token run account my/gcal-single-op-token-margin)
                    (lambda (_)
                      (unless (member account (my/gcal-run-failed-accounts run))
                        (with-local-quit (funcall call))))))
                (deferred:error it
                  (lambda (e) (message "Google Calendar %s failed: %s" kind (my/gcal--error-text e))))
                (deferred:nextc it (lambda (_) (my/gcal--end-when-quiet run)))))
          ((error quit)
           (my/gcal--run-end run)
           (signal (car err) (cdr err)))))))))

(defun my/gcal--post-advice (orig &rest args)
  "Around `org-gcal-post-at-point'.  Called as a command, it is the user's.
Automatic syncs fetch rather than PATCH completed, canceled, or unchanged
synchronized entries."
  (let ((my/gcal--user-op (or my/gcal--user-op (called-interactively-p 'any))))
    (when (and my/gcal--run
               (not my/gcal--user-op)
               (or (org-entry-is-done-p)
                   (my/gcal--entry-sync-unchanged-p)))
      ;; SKIP-EXPORT is the second optional argument.  Keep the GET/import so
      ;; a server-side change still reaches this machine without issuing PATCH.
      (setq args (append args nil))
      (while (< (length args) 2) (setq args (append args '(nil))))
      (setf (nth 1 args) t))
    (my/gcal--single-op "push" orig args)))

(defun my/gcal--delete-advice (orig &rest args)
  "Around `org-gcal-delete-at-point'.  Called as a command, it is the user's."
  (let ((my/gcal--user-op (or my/gcal--user-op (called-interactively-p 'any))))
    (my/gcal--single-op "delete" orig args)))

(defun my/gcal--check-op-run (what heading)
  "Signal if the push or delete WHAT of HEADING outlived its run (a long prompt).
Sending it now would be outside any run, possibly inside a newer one."
  (when (and my/gcal--op-run (not (eq my/gcal--op-run my/gcal--run)))
    (let ((text (format "Google Calendar: \"%s\" not sent: the %s was stopped at its time limit while waiting; run it again"
                        heading what)))
      (message "%s" text)
      (error "%s" text))))

;;;; Writes with an unknown outcome

(defun my/gcal--post-event-advice (orig start end smry loc source desc calendar-id marker
                                        transparency &optional etag event-id &rest more)
  "Around `org-gcal--post-event': describe the write for `my/gcal--request-advice'."
  (my/gcal--check-op-run "push" smry)
  ;; Remember what this run pushed: its push phase must not push it again.
  (when (and my/gcal--run event-id)
    (push event-id (my/gcal-run-pushed my/gcal--run)))
  (let ((my/gcal--write (list :op (if event-id "update" "create") :calendar calendar-id
                              :event event-id
                              :marker (and (markerp marker) (marker-buffer marker)
                                           (copy-marker marker)))))
    (my/gcal--while-busy
      (apply orig start end smry loc source desc calendar-id marker transparency etag event-id more))))

(defun my/gcal--delete-event-advice (orig calendar-id event-id etag marker &rest more)
  "Around `org-gcal--delete-event': describe the write for `my/gcal--request-advice'."
  (my/gcal--check-op-run "delete" (if (and (markerp marker) (marker-buffer marker))
                                      (my/gcal--heading marker)
                                    event-id))
  (let ((my/gcal--write (list :op "delete" :calendar calendar-id :event event-id
                              :marker (and (markerp marker) (marker-buffer marker)
                                           (copy-marker marker)))))
    (my/gcal--while-busy (apply orig calendar-id event-id etag marker more))))

(defun my/gcal--new-event-id ()
  "A fresh Calendar event ID: 32 hex digits, valid base32hex."
  (md5 (format "%s %s %s" (random most-positive-fixnum) (float-time) (emacs-pid))))

(defun my/gcal--add-event-id (data id)
  "JSON DATA, an encoded object, with an \"id\" member ID added first."
  (if (string-prefix-p "{" data)
      (concat (encode-coding-string (format "{\"id\":\"%s\"" id) 'utf-8)
              (if (string-match-p "\\`{[[:space:]]*}" data) "" ",")
              (substring data 1))
    data))

(defun my/gcal--unknown-outcome (response)
  "Why RESPONSE leaves its write's outcome unknown, or nil when it is known."
  (let ((symbol (request-response-symbol-status response)))
    (cond ((eq symbol 'timeout) (format "timed out after %ss" my/gcal-request-timeout))
          ((eq symbol 'abort) "stopped at the time limit")
          ((request-response-status-code response) nil)
          ;; curl 6 and 7: the host was never reached, so nothing was sent.
          ((string-match-p "code \\(?:6\\|7\\)\\b"
                           (format "%s" (request-response-error-thrown response)))
           nil)
          (t "the connection was lost"))))

(defun my/gcal--mark-unknown (write why)
  "Mark WRITE's entry as having an unknown outcome because of WHY, and say so."
  (let ((m (plist-get write :marker))
        (cal (plist-get write :calendar))
        (heading "an entry"))
    (when (and (markerp m) (marker-buffer m))
      (org-with-point-at m
        (org-back-to-heading t)
        (setq heading (org-get-heading t t t t))
        (org-entry-put nil my/gcal-unknown-property
                       (format "%s %s %s %s" (plist-get write :op) cal
                               (or (plist-get write :event) "-")
                               (format-time-string "%FT%T%z")))))
    (message "Google Calendar: %s of \"%s\" on %s has an unknown outcome (%s); not retrying, it will be checked at the next sync"
             (plist-get write :op) heading (my/gcal--label cal) why)))

(defun my/gcal--unknown-entries ()
  "Marked entries in calendar and ID files, as (MARKER OP CALENDAR EVENT)."
  (let ((re (format "^[ \t]*:%s:[ \t]+\\(\\S-+\\)[ \t]+\\(\\S-+\\)[ \t]+\\(\\S-+\\)"
                    (regexp-quote my/gcal-unknown-property)))
        entries)
    (dolist (file (delete-dups (mapcar #'file-truename
                                       (append (mapcar #'cdr org-gcal-fetch-file-alist)
                                               (ignore-errors (org-generic-id-files))))))
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file 'nowarn)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             (let ((op (match-string-no-properties 1))
                   (cal (match-string-no-properties 2))
                   (event (match-string-no-properties 3)))
               (save-excursion
                 (org-back-to-heading t)
                 (push (list (point-marker) op cal event) entries))))))))
    (nreverse entries)))

(defun my/gcal--clear-event-info (marker)
  "Remove calendar info from MARKER's entry, as a successful delete does."
  (org-with-point-at marker
    (when (re-search-forward
           (format "^[ \t]*:%s:[^z-a]*?\n[ \t]*:END:[ \t]*\n?" (regexp-quote org-gcal-drawer-name))
           (save-excursion (outline-next-heading) (point)) 'noerror)
      (replace-match "" 'fixedcase))
    (org-entry-delete marker org-gcal-calendar-id-property)
    (org-entry-delete marker org-gcal-entry-id-property)
    (org-entry-delete marker my/gcal-sync-hash-property))
  ;; This runs inside a sync, so never ask: "ask" keeps the heading.
  (let ((org-gcal-remove-api-cancelled-events
         (and (not (eq org-gcal-remove-api-cancelled-events 'ask))
              org-gcal-remove-api-cancelled-events)))
    (org-with-point-at marker
      (org-back-to-heading)
      (org-gcal--handle-cancelled-entry))))

(defun my/gcal--verified (marker op cal response)
  "Settle MARKER's unknown OP on CAL from the event lookup RESPONSE."
  (let* ((status (request-response-status-code response))
         (data (request-response-data response))
         (cancelled (and (eq status 200) (equal (plist-get data :status) "cancelled")))
         (found (and (eq status 200) (not cancelled)))
         (gone (or cancelled (memq status '(404 410))))
         (heading (my/gcal--heading marker)))
    (if (not (or found gone))
        (message "Google Calendar: could not check the %s of \"%s\" (%s); writes to it stay blocked until the next sync checks it"
                 op heading (if status (format "HTTP %s" status) "no response"))
      (org-entry-delete marker my/gcal-unknown-property)
      (message
       "Google Calendar: %s"
       (pcase op
         ("create"
          (if (not found)
              (format "Google had not applied the create of \"%s\"; push it again if you still want it" heading)
            (org-with-point-at marker (org-gcal--update-entry cal data 'create-from-entry))
            (format "Google had applied the create of \"%s\"; the entry is now linked to it" heading)))
         ("delete"
          (if (not gone)
              (format "Google had not applied the delete of \"%s\"; delete it again if you still want to" heading)
            (my/gcal--clear-event-info marker)
            (format "Google had applied the delete of \"%s\"; calendar info removed from the entry" heading)))
         (_
          (cond (gone (format "the event of \"%s\" no longer exists on Google" heading))
                ((equal (plist-get data :etag) (org-entry-get marker org-gcal-etag-property))
                 (format "Google had not applied the update of \"%s\"; push it again if you still want it" heading))
                (t (org-with-point-at marker (org-gcal--update-entry cal data 'update-existing))
                   (format "Google has a newer version of \"%s\" (the update was applied, or it changed elsewhere); the entry now matches Google" heading)))))))))

(defun my/gcal--verify-one (run entry)
  "Deferred: look up ENTRY's event on Google and settle its unknown write."
  (pcase-let ((`(,marker ,op ,cal ,event) entry))
    (if (or (member (funcall my/gcal-account-function cal) (my/gcal-run-failed-accounts run))
            (equal event "-"))
        (deferred:succeed nil)
      (let ((d (deferred:new #'identity)))
        (request (concat (org-gcal-events-url cal) "/" (url-hexify-string event))
          :type "GET"
          :headers `(("Accept" . "application/json")
                     ("Authorization" . ,(format "Bearer %s" (org-gcal--get-access-token cal))))
          :parser 'org-gcal--json-read
          :my/gcal-quiet t
          :complete (cl-function
                     (lambda (&key response &allow-other-keys)
                       (deferred:callback-post d response))))
        (deferred:nextc d (lambda (response) (my/gcal--verified marker op cal response)))))))

(defun my/gcal--verify-unknown (run)
  "Deferred: settle every entry whose last write has an unknown outcome."
  (my/gcal--each run (my/gcal--unknown-entries)
                 (lambda (entry)
                   (my/gcal--guarded (lambda () (my/gcal--verify-one run entry))
                                     (lambda (err)
                                       (message "Google Calendar: could not check an unknown write: %s"
                                                (my/gcal--error-text err)))))))

;;;; Scheduling

(defvar my/gcal--idle-timer nil
  "The pending idle timer of the scheduled sync, if any.")

(defun my/gcal-schedule-sync (fn &optional idle-seconds)
  "Run FN once Emacs is idle IDLE-SECONDS (default 30), replacing a pending one.
Stacked timers would start back-to-back syncs after a busy spell or sleep."
  (when (timerp my/gcal--idle-timer)
    (cancel-timer my/gcal--idle-timer))
  (setq my/gcal--idle-timer (run-with-idle-timer (or idle-seconds 30) nil fn)))

;;;; Installation

(defun my/gcal-resilience-install ()
  "Wire this module into request.el and org-gcal."
  (require 'deferred)
  (advice-add 'request :around #'my/gcal--request-advice)
  (advice-add 'org-gcal-sync :override #'my/gcal-sync)
  (advice-add 'org-gcal--get-access-token :override #'my/gcal--access-token)
  (advice-add 'org-gcal--refresh-token :override #'my/gcal--refresh-token)
  (advice-add 'org-gcal-post-at-point :around #'my/gcal--post-advice)
  (advice-add 'org-gcal-delete-at-point :around #'my/gcal--delete-advice)
  (advice-add 'org-gcal--post-event :around #'my/gcal--post-event-advice)
  (advice-add 'org-gcal--delete-event :around #'my/gcal--delete-event-advice)
  (add-hook 'org-gcal-after-update-entry-functions #'my/gcal--remember-sync-hash))

(provide 'gcal-resilience)
;;; gcal-resilience.el ends here
