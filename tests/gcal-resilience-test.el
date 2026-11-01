;;; gcal-resilience-test.el --- Tests for gcal-resilience.el -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -L modules/personal -l tests/gcal-resilience-test.el \
;;        -f ert-run-tests-batch-and-exit
;; Every request goes to tests/gcal_mock.py on 127.0.0.1.  Tokens live in an
;; in-memory store, and every org-gcal, request and oauth2-auto file points at
;; a temporary directory, so nothing real is read or written.

(require 'ert)
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(defvar gcal-test--tmp (file-name-as-directory
                        (file-truename (make-temp-file "gcal-test-" t))))
(setq org-gcal-dir (expand-file-name "org-gcal/" gcal-test--tmp)
      persist--directory-location (expand-file-name "persist/" gcal-test--tmp)
      request-storage-directory (expand-file-name "request/" gcal-test--tmp)
      org-generic-id-locations-file (expand-file-name "ids" gcal-test--tmp)
      oauth2-auto-plstore (expand-file-name "oauth2-auto.plist" gcal-test--tmp)
      org-id-locations-file (expand-file-name "org-ids" gcal-test--tmp))

(require 'org-gcal)
(require 'gcal-resilience)
(my/gcal-resilience-install)

(setq org-gcal-notify-p nil
      org-gcal-auto-archive nil
      org-gcal-local-timezone "America/Chicago"
      org-gcal-up-days 30 org-gcal-down-days 90
      org-gcal-client-id "test-client" org-gcal-client-secret "test-secret")

;;;; Mock server

(defvar gcal-test--server nil)
(defvar gcal-test--port nil)

(defun gcal-test--start-server ()
  (unless (process-live-p gcal-test--server)
    (let ((buf (generate-new-buffer " *gcal-mock*")))
      (setq gcal-test--server
            (make-process :name "gcal-mock" :buffer buf :noquery t
                          :command (list "python3" "-I"
                                         (expand-file-name "tests/gcal_mock.py"
                                                           user-emacs-directory))))
      (with-current-buffer buf
        (while (not (progn (goto-char (point-min)) (re-search-forward "PORT \\([0-9]+\\)" nil t)))
          (accept-process-output gcal-test--server 0.1))
        (setq gcal-test--port (string-to-number (match-string 1)))))))

(add-hook 'kill-emacs-hook
          (lambda () (when (process-live-p gcal-test--server) (kill-process gcal-test--server))))

(defun gcal-test--base ()(format "http://127.0.0.1:%d" gcal-test--port))

(defun gcal-test--control (path &optional obj)
  "Call the mock's control PATH, posting OBJ as JSON when given."
  (let ((url-request-method (if obj "POST" "GET"))
        (url-request-data (and obj (encode-coding-string (json-encode obj) 'utf-8)))
        (url-request-extra-headers '(("Content-Type" . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously (concat (gcal-test--base) path) t t 5)
      (goto-char (point-min))
      (re-search-forward "^\r?$")
      (prog1 (let ((json-object-type 'plist) (json-array-type 'list)) (json-read))
        (kill-buffer)))))

(defun gcal-test--log () (gcal-test--control "/__log"))
(defun gcal-test--requests (method regexp)
  "Logged requests with METHOD whose path matches REGEXP."
  (seq-filter (lambda (e) (and (equal (plist-get e :method) method)
                               (string-match-p regexp (plist-get e :path))))
              (gcal-test--log)))

(defun gcal-test--wait (pred seconds)
  "Pump events until PRED is non-nil or SECONDS pass; return PRED's value."
  (let ((end (+ (float-time) seconds)) v)
    (while (and (not (setq v (funcall pred))) (< (float-time) end))
      (accept-process-output nil 0.05))
    v))

(defun gcal-test--curl-procs ()
  (seq-filter (lambda (p) (string-match-p "request curl" (process-name p))) (process-list)))

;;;; Tokens, calendars, messages

(defvar gcal-test--tokens (make-hash-table :test 'equal)
  "ACCOUNT -> token plist; stands in for the encrypted plstore.")
(defvar gcal-test--browser-opened nil)

(advice-add 'oauth2-auto--plstore-read :override
            (lambda (username _provider) (copy-sequence (gethash username gcal-test--tokens))))
(advice-add 'oauth2-auto--plstore-write :override
            (lambda (username _provider plist) (puthash username plist gcal-test--tokens) plist))
(advice-add 'browse-url :override
            (lambda (&rest _) (setq gcal-test--browser-opened t)))

(defun gcal-test--token (account seconds-left &optional refresh)
  (puthash account (list :access-token (format "valid-%s" account)
                         :refresh-token (or refresh (format "refresh-%s" account))
                         :expiration (+ (oauth2-auto--now) seconds-left))
           gcal-test--tokens))

(defun gcal-test--org-file (name content)
  (let ((f (expand-file-name name gcal-test--tmp)))
    (when-let ((b (get-file-buffer f))) (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b))
    (with-temp-file f (insert content))
    f))

(defvar gcal-test--messages nil)
(defun gcal-test--record-message (fmt &rest args)
  (when fmt (push (apply #'format-message fmt args) gcal-test--messages)))
(advice-add 'message :before #'gcal-test--record-message)
(defun gcal-test--messages-matching (regexp)
  (seq-filter (lambda (m) (string-match-p regexp m)) gcal-test--messages))

(defun gcal-test--event (id summary &optional days)
  (let ((day (format-time-string "%Y-%m-%d" (time-add nil (* 86400 (or days 2))))))
    `(:id ,id :summary ,summary :status "confirmed" :etag ,(format "\"%s-1\"" id)
      :start (:dateTime ,(concat day "T10:00:00-05:00"))
      :end (:dateTime ,(concat day "T11:00:00-05:00"))
      :updated "2026-10-30T00:00:00.000Z")))

(defmacro gcal-test--with-env (spec &rest body)
  "Run BODY against a fresh mock state.
SPEC is a plist: :events and :rules go to the mock, :calendars is a list
of (ID ACCOUNT) pairs, each getting an empty Org file."
  (declare (indent 1))
  `(progn
     (gcal-test--start-server)
     (when my/gcal--run (my/gcal--run-end my/gcal--run))
     (setq org-gcal--sync-lock nil gcal-test--messages nil
           gcal-test--browser-opened nil org-gcal--sync-tokens nil)
     (clrhash gcal-test--tokens)
     (let* ((spec ,spec)
            (cals (plist-get spec :calendars))
            (my/gcal-google-url-regexp "\\`http://127\\.0\\.0\\.1:")
            (my/gcal-account-function
             (lambda (cid) (or (cadr (assoc cid cals)) cid)))
            (oauth2-auto-additional-providers-alist
             `((org-gcal (authorize_url . ,(concat (gcal-test--base) "/auth"))
                         (token_url . ,(concat (gcal-test--base) "/token"))
                         (scope . "https://www.googleapis.com/auth/calendar")
                         (client_id . "test-client")
                         (client_secret . "test-secret"))))
            (org-gcal-fetch-file-alist
             (mapcar (lambda (c) (cons (car c) (gcal-test--org-file (concat (car c) ".org") "")))
                     cals))
            (org-agenda-files (mapcar #'cdr org-gcal-fetch-file-alist)))
       (dolist (c cals) (gcal-test--token (cadr c) 3000))
       (gcal-test--control "/__reset" (list :events (or (plist-get spec :events) (make-hash-table))
                                            :rules (vconcat (plist-get spec :rules))))
       (cl-letf (((symbol-function 'org-gcal-events-url)
                  (lambda (cid) (format "%s/calendar/v3/calendars/%s/events"
                                        (gcal-test--base) (url-hexify-string cid)))))
         (unwind-protect (progn ,@body)
           (when my/gcal--run (my/gcal--run-end my/gcal--run))
           (dolist (p (gcal-test--curl-procs)) (delete-process p)))))))

;;;; Task 1 — bounded, reported, sanitized requests

(ert-deftest gcal-sanitize-redacts-credentials ()
  (let ((s (my/gcal--sanitize
            (concat "https://x/events?access_token=ya29.SECRET&maxResults=5 "
                    "Authorization: Bearer ya29.SECRET2 "
                    "refresh_token=1//REFRESH client_secret=GOCSPX-abc "
                    "(refresh_token . \"1//R2\") \"access_token\": \"ya29.S3\""))))
    (dolist (secret '("ya29.SECRET" "ya29.SECRET2" "1//REFRESH" "GOCSPX-abc" "1//R2" "ya29.S3"))
      (should-not (string-search secret s)))
    (should (string-search "maxResults=5" s))))

(ert-deftest gcal-url-parts-with-encoded-calendar ()
  (should (equal (my/gcal--url-parts
                  "https://www.googleapis.com/calendar/v3/calendars/me%40example.com/events/abc_2026")
                 '("me@example.com" . "abc_2026"))))

(ert-deftest gcal-request-timeout-cleans-up ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1"))
                         :rules ((:method "GET" :path "/events$" :delay -1)))
    (let* ((my/gcal-request-timeout 1)
           (start (float-time))
           (resp (request (org-gcal-events-url "cal1")
                   :params '(("access_token" . "ya29.SECRET"))
                   :parser 'org-gcal--json-read)))
      (should (gcal-test--wait (lambda () (request-response-done-p resp)) 5))
      (should (eq (request-response-symbol-status resp) 'timeout))
      (should (< (- (float-time) start) 3))
      (should-not (gcal-test--curl-procs))
      (should (gcal-test--messages-matching "GET events on cal1 timed out after 1s"))
      (should-not (gcal-test--messages-matching "ya29")))))

(ert-deftest gcal-request-403-reported-without-credentials ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1"))
        :rules ((:method "PATCH" :path "/events/ev9$" :status 403
                 :body (:error (:code 403 :message "Calendar usage limits exceeded."
                                :errors [(:reason "quotaExceeded")])))))
    (let ((resp (request (concat (org-gcal-events-url "cal1") "/ev9")
                  :type "PATCH"
                  :headers '(("Authorization" . "Bearer ya29.SECRET"))
                  :data "{}"
                  :parser 'org-gcal--json-read)))
      (should (gcal-test--wait (lambda () (request-response-done-p resp)) 5))
      (let ((m (gcal-test--messages-matching "HTTP 403")))
        (should (= 1 (length m)))
        (should (string-match-p "PATCH event ev9 on cal1" (car m)))
        (should (string-match-p "quotaExceeded" (car m)))
        (should (string-match-p "Calendar usage limits exceeded" (car m))))
      (should-not (gcal-test--messages-matching "ya29\\|access_token\\|127\\.0\\.0\\.1")))))

(ert-deftest gcal-request-non-google-untouched ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (let* ((my/gcal-google-url-regexp "\\`https://never\\.example/")
           (resp (request (org-gcal-events-url "cal1") :parser 'org-gcal--json-read)))
      (should (gcal-test--wait (lambda () (request-response-done-p resp)) 5))
      (should-not (plist-get (request-response-settings resp) :timeout)))))

;;;; Task 2 — run owner, sync orchestration, overlap, cap

(defun gcal-test--managed-entry (heading cal id &optional days managed)
  "Org text for an event entry linked to ID on CAL, MANAGED by \"org\" by default."
  (let ((day (format-time-string "%Y-%m-%d %a" (time-add nil (* 86400 (or days 2))))))
    (format "* %s
:PROPERTIES:
:calendar-id: %s
:entry-id: %s/%s
:org-gcal-managed: %s
:ETag: \"%s-1\"
:END:
:org-gcal:
<%s 10:00-11:00>
:END:
" heading cal id cal (or managed "org") id day)))

(defun gcal-test--file-text (cal)
  (with-current-buffer (find-file-noselect (cdr (assoc cal org-gcal-fetch-file-alist)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gcal-test--sync-and-wait (&optional seconds)
  "Start `my/gcal-sync' and wait for its run to end; return its end time."
  (should (my/gcal-sync nil t))
  (should (gcal-test--wait (lambda () (null my/gcal--run)) (or seconds 10)))
  (float-time))

(ert-deftest gcal-sync-run-ends-after-last-request ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1") ("cal2" "acct2"))
        :events (:cal1 [,(gcal-test--event "a1" "Alpha")] :cal2 [,(gcal-test--event "b1" "Beta")])
        :rules ((:method "GET" :path "cal2/events$" :delay 1 :apply t)))
    (should (my/gcal-sync nil t))
    (should my/gcal--run)
    (should org-gcal--sync-lock)
    (gcal-test--wait (lambda () nil) 0.5)
    (should my/gcal--run)             ; still running while cal2 is slow
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 10))
    (let ((end (float-time)))
      (should-not org-gcal--sync-lock)
      (should-not (gcal-test--curl-procs))
      (should (string-match-p "Alpha" (gcal-test--file-text "cal1")))
      (should (string-match-p "Beta" (gcal-test--file-text "cal2")))
      (dolist (e (gcal-test--log)) (should (< (plist-get e :t) end)))
      (gcal-test--wait (lambda () nil) 1)
      (dolist (e (gcal-test--log)) (should (< (plist-get e :t) end))))))

(ert-deftest gcal-sync-refuses-overlap ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1"))
        :rules ((:method "GET" :path "cal1/events$" :delay 1 :apply t)))
    (should (my/gcal-sync nil t))
    (gcal-test--wait (lambda () nil) 0.3)
    (should-not (my/gcal-sync nil t))
    (should (gcal-test--messages-matching
             "Google Calendar sync not started: a sync started [0-9]+s ago is still running"))
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 10))
    (should (= 1 (length (gcal-test--requests "GET" "cal1/events$"))))))

(ert-deftest gcal-sync-cap-aborts-and-releases ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1") ("cal2" "acct2"))
        :rules ((:method "GET" :path "cal1/events$" :delay -1)))
    (let ((my/gcal-run-timeout 1)
          (start (float-time)))
      (should (my/gcal-sync nil t))
      (should (gcal-test--wait (lambda () (null my/gcal--run)) 5))
      (should (< (- (float-time) start) 2))
      (should-not org-gcal--sync-lock)
      (should (gcal-test--wait (lambda () (null (gcal-test--curl-procs))) 2))
      (should (gcal-test--messages-matching
               "Google Calendar sync stopped after 1s; not finished: cal1, cal2"))
      (should-not (gcal-test--messages-matching "was cancelled"))
      ;; Nothing for cal2 was attempted, and the next run starts normally.
      (should-not (gcal-test--requests "GET" "cal2/events$"))
      (gcal-test--control "/__reset" '(:rules []))
      (gcal-test--sync-and-wait)
      (should (gcal-test--requests "GET" "cal2/events$")))))

(ert-deftest gcal-sync-calendar-isolation ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1") ("cal2" "acct2"))
        :events (:cal2 [,(gcal-test--event "b1" "Beta")])
        :rules ((:method "GET" :path "cal1/events$" :status 500
                 :body (:error (:code 500 :message "Backend Error")))))
    (gcal-test--sync-and-wait)
    (should (string-match-p "Beta" (gcal-test--file-text "cal2")))
    (should (gcal-test--messages-matching "Google Calendar: cal1 failed:"))
    (should (gcal-test--messages-matching "GET events on cal1 failed: HTTP 500"))))

(ert-deftest gcal-sync-stale-response-dropped ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1"))
        :events (:cal1 [,(gcal-test--event "a1" "Late")])
        :rules ((:method "GET" :path "cal1/events$" :delay 2 :apply t)))
    (let ((my/gcal-run-timeout 1))
      (should (my/gcal-sync nil t))
      (should (gcal-test--wait (lambda () (null my/gcal--run)) 5))
      (gcal-test--wait (lambda () nil) 2)
      (should-not (string-match-p "Late" (gcal-test--file-text "cal1"))))))

(ert-deftest gcal-sync-clears-orphan-lock ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (setq org-gcal--sync-lock t)
    (gcal-test--sync-and-wait)
    (should (gcal-test--messages-matching "cleared an org-gcal sync lock"))
    (should (gcal-test--requests "GET" "cal1/events$"))))

(ert-deftest gcal-sync-pushes-org-managed-entries ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1"))
        :events (:cal1 [,(gcal-test--event "m1" "One") ,(gcal-test--event "m2" "Two")]))
    (let ((f (cdr (assoc "cal1" org-gcal-fetch-file-alist))))
      (gcal-test--org-file "cal1.org" (concat (gcal-test--managed-entry "One" "cal1" "m1")
                                              (gcal-test--managed-entry "Two" "cal1" "m2")))
      (let ((org-agenda-files (list f)))
        (let ((end (gcal-test--sync-and-wait)))
          (should (gcal-test--requests "PATCH" "/events/m1$"))
          (should (gcal-test--requests "PATCH" "/events/m2$"))
          (should-not (gcal-test--messages-matching "no-catch"))
          (dolist (e (gcal-test--log)) (should (< (plist-get e :t) end))))))))

(ert-deftest gcal-schedule-single-idle-timer ()
  (let ((before (length timer-idle-list)))
    (dotimes (_ 3) (my/gcal-schedule-sync #'ignore 3600))
    (should (= (1+ before) (length timer-idle-list)))
    (cancel-timer my/gcal--idle-timer)))

(ert-deftest gcal-org-gcal-sync-is-routed ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (org-gcal-fetch)
    (should my/gcal--run)
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 10))
    (should (gcal-test--requests "GET" "cal1/events$"))))

;;;; Task 3 — tokens

(ert-deftest gcal-token-refresh-before-sync ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (gcal-test--token "acct1" 300)       ; inside the 10-minute margin
    (gcal-test--sync-and-wait)
    (let* ((log (gcal-test--log))
           (paths (mapcar (lambda (e) (plist-get e :path)) log))
           (token (seq-find (lambda (e) (equal (plist-get e :path) "/token")) log)))
      (should (equal (car paths) "/token"))
      (should (string-match-p "grant_type=refresh_token" (plist-get token :body)))
      (should (string-match-p "refresh_token=refresh-acct1" (plist-get token :body)))
      (should (string-match-p "access_token=fresh-1"
                              (plist-get (seq-find (lambda (e) (string-match-p "/events$" (plist-get e :path)))
                                                   log)
                                         :query))))
    (should (equal "fresh-1" (plist-get (gethash "acct1" gcal-test--tokens) :access-token)))
    (should (equal "refresh-acct1" (plist-get (gethash "acct1" gcal-test--tokens) :refresh-token)))
    (should-not gcal-test--browser-opened)
    (should-not (gcal-test--messages-matching "refresh-acct1\\|fresh-1"))))

(ert-deftest gcal-token-fresh-not-refreshed ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (gcal-test--sync-and-wait)
    (should-not (gcal-test--requests "POST" "/token"))))

(ert-deftest gcal-token-invalid-grant-skips-account ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1") ("cal3" "acct1") ("cal2" "acct2"))
        :rules ((:method "POST" :path "/token" :status 400
                 :body (:error "invalid_grant" :error_description "Token has been expired or revoked."))))
    (gcal-test--token "acct1" -10)
    (gcal-test--sync-and-wait)
    (should-not (gcal-test--requests "GET" "cal1/events$"))
    (should-not (gcal-test--requests "GET" "cal3/events$"))
    (should (gcal-test--requests "GET" "cal2/events$"))
    (let ((m (gcal-test--messages-matching "acct1")))
      (should (= 1 (length m)))
      (should (string-match-p "M-x my/gcal-reauth" (car m)))
      (should (string-match-p "cal1, cal3" (car m))))
    (should (= 1 (length (gcal-test--requests "POST" "/token"))))
    (should-not gcal-test--browser-opened)))

(ert-deftest gcal-token-transient-failure ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1"))
        :rules ((:method "POST" :path "/token" :status 503 :body (:error "backend"))))
    (gcal-test--token "acct1" -10)
    (gcal-test--sync-and-wait)
    (should (gcal-test--messages-matching "token refresh for acct1 failed (HTTP 503[^)]*).*next scheduled sync"))
    (should-not (gcal-test--messages-matching "reauth"))
    (should-not (gcal-test--requests "GET" "cal1/events$"))
    (should-not gcal-test--browser-opened)))

(ert-deftest gcal-token-refresh-timeout ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1"))
        :rules ((:method "POST" :path "/token" :delay -1)))
    (gcal-test--token "acct1" -10)
    (let ((my/gcal-request-timeout 1)
          (start (float-time)))
      (gcal-test--sync-and-wait)
      (should (< (- (float-time) start) 3))
      (should (gcal-test--messages-matching "token refresh for acct1 failed (timed out after 1s)")))
    (should-not gcal-test--browser-opened)))

(ert-deftest gcal-token-config-error ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1"))
        :rules ((:method "POST" :path "/token" :status 401
                 :body (:error "invalid_client" :error_description "The OAuth client was not found."))))
    (gcal-test--token "acct1" -10)
    (gcal-test--sync-and-wait)
    (should (gcal-test--messages-matching "configuration.*invalid_client"))
    (should-not (gcal-test--messages-matching "reauth"))
    (should-not gcal-test--browser-opened)))

(ert-deftest gcal-token-missing-refresh-token ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (remhash "acct1" gcal-test--tokens)
    (gcal-test--sync-and-wait)
    (should (gcal-test--messages-matching "acct1.*no stored sign-in.*M-x my/gcal-reauth"))
    (should-not (gcal-test--log))
    (should-not gcal-test--browser-opened)))

(ert-deftest gcal-access-token-never-blocks ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (gcal-test--token "acct1" -10)
    (let ((start (float-time)))
      (should-error (org-gcal--get-access-token "cal1"))
      (should (< (- (float-time) start) 1)))
    (gcal-test--token "acct1" 3000)
    (should (equal "valid-acct1" (org-gcal--get-access-token "cal1")))
    (should-not gcal-test--browser-opened)))

(ert-deftest gcal-401-not-retried ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1"))
        :rules ((:method "GET" :path "cal1/events$" :status 401 :times -1
                 :body (:error (:code 401 :message "Invalid Credentials"
                                :errors [(:reason "authError")])))))
    (gcal-test--sync-and-wait)
    (should (= 1 (length (gcal-test--requests "GET" "cal1/events$"))))
    (should (gcal-test--messages-matching "rejected the access token for acct1"))
    (should-not gcal-test--browser-opened)))

(ert-deftest gcal-oauth2-auto-contract ()
  "Fails when an oauth2-auto upgrade changes the internals the refresh uses."
  (cl-flet ((arity (f) (func-arity (advice--cd*r (indirect-function f)))))
    (should (equal (arity 'oauth2-auto--plstore-read) '(2 . 2)))
    (should (equal (arity 'oauth2-auto--plstore-write) '(3 . 3)))
    (should (equal (arity 'oauth2-auto--make-plist) '(2 . 2)))
    (should (equal (arity 'oauth2-auto--provider-info) '(1 . 1))))
  (should (fboundp 'oauth2-auto--now))
  (should (fboundp 'oauth2-auto-force-reauth))
  (let ((p (oauth2-auto--make-plist '((access_token . "a") (expires_in . 60)) '(:refresh-token "r"))))
    (should (equal (plist-get p :access-token) "a"))
    (should (equal (plist-get p :refresh-token) "r"))
    (should (<= (- (plist-get p :expiration) (oauth2-auto--now)) 60)))
  (should (oauth2-auto--plist-needs-refreshing (list :expiration (1- (oauth2-auto--now)))))
  (should-not (oauth2-auto--plist-needs-refreshing (list :expiration (+ 60 (oauth2-auto--now))))))

;;;; Task 4 — single-event runs and writes with an unknown outcome

(defun gcal-test--new-entry (heading cal &optional days)
  "Org text for an entry not yet on Google, to be created on CAL."
  (let ((day (format-time-string "%Y-%m-%d %a" (time-add nil (* 86400 (or days 2))))))
    (format "* %s\n:PROPERTIES:\n:calendar-id: %s\n:END:\n:org-gcal:\n<%s 10:00-11:00>\n:END:\n"
            heading cal day)))

(defmacro gcal-test--at-entry (cal heading &rest body)
  "Run BODY with point on HEADING in CAL's file buffer."
  (declare (indent 2))
  `(with-current-buffer (find-file-noselect (cdr (assoc ,cal org-gcal-fetch-file-alist)))
     (goto-char (point-min))
     (re-search-forward (concat "^\\* " (regexp-quote ,heading)))
     (beginning-of-line)
     ,@body))

(defun gcal-test--unknown (cal heading)
  (gcal-test--at-entry cal heading (org-entry-get nil my/gcal-unknown-property)))

(defun gcal-test--write-file (cal text)
  (gcal-test--org-file (concat cal ".org") text)
  (setf (cdr (assoc cal org-gcal-fetch-file-alist)) (expand-file-name (concat cal ".org") gcal-test--tmp)))

(ert-deftest gcal-push-is-its-own-run ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1")) :events (:cal1 [,(gcal-test--event "m1" "One")]))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (gcal-test--at-entry "cal1" "One" (org-gcal-post-at-point))
    (should (equal "push" (my/gcal-run-kind my/gcal--run)))
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 5))
    (should (= 1 (length (gcal-test--requests "PATCH" "/events/m1$"))))
    (should-not org-gcal--sync-lock)))

(ert-deftest gcal-push-refused-during-sync ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1")) :events (:cal1 [,(gcal-test--event "m1" "One")])
        :rules ((:method "GET" :path "cal1/events$" :delay 1 :apply t)))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (should (my/gcal-sync t t))         ; skip-export: the sync itself pushes nothing
    (gcal-test--at-entry "cal1" "One"
      (my/gcal-user-command #'org-gcal-post-at-point))
    (should (gcal-test--messages-matching
             "Google Calendar push not started: a sync started [0-9]+s ago is still running"))
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 10))
    (should-not (gcal-test--requests "PATCH" "/events/m1$"))))

(ert-deftest gcal-push-refreshes-token-first ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1")) :events (:cal1 [,(gcal-test--event "m1" "One")]))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (gcal-test--token "acct1" 60)
    (gcal-test--at-entry "cal1" "One" (org-gcal-post-at-point))
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 5))
    (should (equal '("/token" "/calendar/v3/calendars/cal1/events/m1")
                   (mapcar (lambda (e) (plist-get e :path)) (gcal-test--log))))
    (should-not gcal-test--browser-opened)))

(ert-deftest gcal-create-lost-response-marked-then-linked ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1"))
        :rules ((:method "POST" :path "cal1/events$" :apply t :delay -1)))
    (gcal-test--write-file "cal1" (gcal-test--new-entry "Dentist" "cal1"))
    (let ((my/gcal-request-timeout 1))
      (gcal-test--at-entry "cal1" "Dentist" (org-gcal-post-at-point))
      (should (gcal-test--wait (lambda () (null my/gcal--run)) 5)))
    (let* ((mark (gcal-test--unknown "cal1" "Dentist"))
           (parts (split-string mark))
           (server (plist-get (gcal-test--control "/__events") :cal1)))
      (should (equal (seq-take parts 2) '("create" "cal1")))
      ;; Google applied it under the id the request carried.
      (should (equal (nth 2 parts) (plist-get (car server) :id)))
      (should (gcal-test--messages-matching
               "create of \"Dentist\" on cal1 has an unknown outcome (timed out after 1s); not retrying"))
      (should (= 1 (length (gcal-test--requests "POST" "cal1/events$"))))
      ;; The next sync checks it before anything else and links the entry.
      (gcal-test--control "/__reset" `(:events (:cal1 ,(vconcat server))))
      (gcal-test--sync-and-wait)
      (should-not (gcal-test--unknown "cal1" "Dentist"))
      (should (equal (format "%s/cal1" (nth 2 parts))
                     (gcal-test--at-entry "cal1" "Dentist" (org-entry-get nil "entry-id"))))
      (should (equal (format "/calendar/v3/calendars/cal1/events/%s" (nth 2 parts))
                     (plist-get (car (gcal-test--log)) :path)))
      (should-not (gcal-test--requests "POST" "cal1/events$"))
      (should (gcal-test--messages-matching "Google had applied the create of \"Dentist\"")))))

(ert-deftest gcal-unknown-create-not-applied ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (gcal-test--write-file "cal1" (gcal-test--new-entry "Dentist" "cal1"))
    (gcal-test--at-entry "cal1" "Dentist"
      (org-entry-put nil my/gcal-unknown-property "create cal1 abc123abc123 2026-10-31T00:00:00"))
    (gcal-test--sync-and-wait)
    (should-not (gcal-test--unknown "cal1" "Dentist"))
    (should-not (gcal-test--requests "POST" "cal1/events$"))
    (should (gcal-test--messages-matching "Google had not applied the create of \"Dentist\""))))

(ert-deftest gcal-unknown-update-not-applied-keeps-local ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1")) :events (:cal1 [,(gcal-test--event "m1" "Server title")]))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "Local title" "cal1" "m1" 2 "gcal"))
    (gcal-test--at-entry "cal1" "Local title"
      (org-entry-put nil my/gcal-unknown-property "update cal1 m1 2026-10-31T00:00:00"))
    (should (my/gcal-sync t t))
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 10))
    (should (gcal-test--messages-matching "Google had not applied the update of \"Local title\""))
    (should (equal "/calendar/v3/calendars/cal1/events/m1" (plist-get (car (gcal-test--log)) :path)))))

(ert-deftest gcal-unknown-delete-applied ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "Gone" "cal1" "m1"))
    (gcal-test--at-entry "cal1" "Gone"
      (org-entry-put nil my/gcal-unknown-property "delete cal1 m1 2026-10-31T00:00:00"))
    (gcal-test--sync-and-wait)
    (should-not (gcal-test--unknown "cal1" "Gone"))
    (should-not (gcal-test--at-entry "cal1" "Gone" (org-entry-get nil "entry-id")))
    (should (gcal-test--messages-matching "Google had applied the delete of \"Gone\""))))

(ert-deftest gcal-unknown-check-fails-keeps-mark ()
  (gcal-test--with-env
      '(:calendars (("cal1" "acct1"))
        :rules ((:method "GET" :path "/events/m1$" :status 503 :times -1)))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (gcal-test--at-entry "cal1" "One"
      (org-entry-put nil my/gcal-unknown-property "update cal1 m1 2026-10-31T00:00:00"))
    (gcal-test--sync-and-wait)
    (should (gcal-test--unknown "cal1" "One"))
    (should-not (gcal-test--requests "PATCH" "/events/m1$"))
    (should (gcal-test--messages-matching "could not check the update of \"One\""))))

(ert-deftest gcal-marked-entry-refuses-write ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1")) :events (:cal1 [,(gcal-test--event "m1" "One")]))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (gcal-test--at-entry "cal1" "One"
      (org-entry-put nil my/gcal-unknown-property "update cal1 m1 2026-10-31T00:00:00")
      (org-gcal-post-at-point)
      (org-gcal-delete-at-point))
    (gcal-test--wait (lambda () nil) 0.5)
    (should-not (gcal-test--log))
    (should-not my/gcal--run)
    (should (= 2 (length (gcal-test--messages-matching "its last write has an unknown outcome"))))))

(ert-deftest gcal-definite-results-not-marked ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1"))
        :events (:cal1 [,(gcal-test--event "m1" "One") ,(gcal-test--event "m2" "Two")])
        :rules ((:method "PATCH" :path "/events/m2$" :status 403
                 :body (:error (:code 403 :message "Forbidden")))))
    (gcal-test--write-file "cal1" (concat (gcal-test--managed-entry "One" "cal1" "m1")
                                          (gcal-test--managed-entry "Two" "cal1" "m2")))
    (gcal-test--at-entry "cal1" "One" (org-gcal-post-at-point))
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 5))
    (gcal-test--at-entry "cal1" "Two" (org-gcal-post-at-point))
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 5))
    (should-not (gcal-test--unknown "cal1" "One"))
    (should-not (gcal-test--unknown "cal1" "Two"))
    (should (gcal-test--messages-matching "PATCH event m2 on cal1 failed: HTTP 403"))))

(ert-deftest gcal-connection-refused-not-marked ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1")))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (cl-letf (((symbol-function 'org-gcal-events-url)
               (lambda (cid) (format "http://127.0.0.1:9/calendar/v3/calendars/%s/events" cid))))
      (gcal-test--at-entry "cal1" "One" (org-gcal-post-at-point))
      (should (gcal-test--wait (lambda () (null my/gcal--run)) 5)))
    (should-not (gcal-test--unknown "cal1" "One"))
    (should (gcal-test--messages-matching "PATCH event m1 on cal1 failed: no response"))))

;;;; Review fixes

(ert-deftest gcal-interactive-delete-refused-during-sync ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1")) :events (:cal1 [,(gcal-test--event "m1" "One")])
        :rules ((:method "GET" :path "cal1/events$" :delay 1 :apply t)))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (should (my/gcal-sync t t))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (gcal-test--at-entry "cal1" "One" (call-interactively #'org-gcal-delete-at-point)))
    (should (gcal-test--messages-matching "Google Calendar delete not started: a sync started"))
    (should (gcal-test--wait (lambda () (null my/gcal--run)) 10))
    (should-not (gcal-test--requests "DELETE" "/events/m1$"))))

(ert-deftest gcal-prompt-outlasting-cap-sends-nothing ()
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1")) :events (:cal1 [,(gcal-test--event "m1" "One")]))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (let ((my/gcal-run-timeout 0.5))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (gcal-test--wait (lambda () (null my/gcal--run)) 3) t)))
        (gcal-test--at-entry "cal1" "One"
          (ignore-errors (call-interactively #'org-gcal-delete-at-point)))))
    (gcal-test--wait (lambda () nil) 0.5)
    (should-not (gcal-test--requests "DELETE" "/events/m1$"))
    (should (gcal-test--messages-matching "not sent: the delete was stopped at its time limit"))))

(ert-deftest gcal-run-timeout-releases-even-if-message-fails ()
  (gcal-test--with-env '(:calendars (("cal1" "acct1"))
                         :rules ((:method "GET" :path "cal1/events$" :delay -1)))
    (let ((my/gcal-run-timeout 0.5)
          (my/gcal-calendar-label-function (lambda (_) (error "boom"))))
      (should (my/gcal-sync nil t))
      (should (gcal-test--wait (lambda () (null my/gcal--run)) 3))
      (should-not org-gcal--sync-lock))))

(ert-deftest gcal-sanitize-redacts-plist-tokens ()
  (let ((s (my/gcal--sanitize "(:access-token \"ya29.S\" :refresh-token \"1//R\" :expiration 5)")))
    (should-not (string-search "ya29.S" s))
    (should-not (string-search "1//R" s))
    (should (string-search ":expiration 5" s))))

(ert-deftest gcal-sync-pushes-each-entry-once ()
  "An entry the fetch phase already pushed is not pushed again in the same run.
An Org-managed entry the fetch never sees is still pushed once."
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1"))
        :events (:cal1 [,(gcal-test--event "m1" "One")] :cal2 [,(gcal-test--event "m3" "Three")]))
    (gcal-test--write-file "cal1" (concat (gcal-test--managed-entry "One" "cal1" "m1")
                                          (gcal-test--managed-entry "Three" "cal2" "m3")))
    (gcal-test--token "cal2" 3000)      ; cal2 is not fetched, so its account is itself
    (gcal-test--sync-and-wait)
    (should (= 1 (length (gcal-test--requests "PATCH" "/events/m1$"))))
    (should (= 1 (length (gcal-test--requests "PATCH" "/events/m3$"))))))

(ert-deftest gcal-run-covers-push-behind-slow-token-read ()
  "Reading the token decrypts with gpg, which lets timers run; the run must not
end in that gap and let org-gcal's fetch-phase push go out after it."
  (gcal-test--with-env
      `(:calendars (("cal1" "acct1")) :events (:cal1 [,(gcal-test--event "m1" "One")]))
    (gcal-test--write-file "cal1" (gcal-test--managed-entry "One" "cal1" "m1"))
    (let* ((ended nil)
           (note-end (lambda (run) (when (eq run my/gcal--run) (setq ended (float-time))))))
      (advice-add 'my/gcal--run-end :before note-end)
      (unwind-protect
          (cl-letf* ((read (symbol-function 'oauth2-auto--plstore-read))
                     ((symbol-function 'oauth2-auto--plstore-read)
                      (lambda (&rest args)
                        ;; Like epg waiting for gpg: wait on a subprocess.
                        (let ((p (start-process "fake-gpg" nil "sleep" "0.3")))
                          (while (process-live-p p) (accept-process-output p 0.05)))
                        (apply read args))))
            (should (my/gcal-sync nil t))
            (should (gcal-test--wait (lambda () (null my/gcal--run)) 15))
            (gcal-test--wait (lambda () nil) 1.5))
        (advice-remove 'my/gcal--run-end note-end))
      (let ((patches (gcal-test--requests "PATCH" "/events/m1$")))
        (should (= 1 (length patches)))
        (should (< (plist-get (car patches) :t) ended))))))

;;;; Task 5 — wiring in gcal.el

(ert-deftest gcal-config-wiring ()
  "gcal.el routes syncs, pushes and reauth through gcal-resilience."
  (let ((src (with-temp-buffer
               (insert-file-contents (expand-file-name "modules/personal/gcal.el" user-emacs-directory))
               (buffer-string))))
    (should (string-match-p "(my/gcal-resilience-install)" src))
    (should (string-match-p "(my/gcal-sync nil" src))
    (should (string-match-p "my/gcal-schedule-sync" src))
    (should (string-match-p "my/gcal-user-command #'org-gcal-post-at-point" src))
    (should (string-match-p "oauth2-auto-force-reauth" src))
    (should-not (string-match-p "600)" src))
    (should-not (string-match-p ":filter-args #'my/gcal--token-args" src))
    (should-not (string-match-p "run-with-idle-timer" src))))

(provide 'gcal-resilience-test)
;;; gcal-resilience-test.el ends here
