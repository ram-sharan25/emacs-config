;;; logs-test.el --- Tests for logs.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'logs)

(defmacro logs-test--with-org (content &rest body)
  "Run BODY in an Org temp buffer holding CONTENT, point at start."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(ert-deftest logs-find-child-nil-when-missing ()
  (logs-test--with-org "* TODO Task\n** Notes\n"
    (should-not (rsr/logs--find-child "Logs"))))

(ert-deftest logs-find-child-direct-child-only ()
  (logs-test--with-org "* TODO Task\n** Notes\n*** Logs\n** Logs\n"
    (goto-char (rsr/logs--find-child "Logs"))
    (should (= (org-current-level) 2))))

(ert-deftest logs-ensure-child-creates-last-child ()
  (logs-test--with-org "* TODO Task\nbody\n** Notes\n* TODO Next\n"
    (rsr/logs--ensure-child "Logs")
    (should (equal (buffer-string)
                   "* TODO Task\nbody\n** Notes\n** Logs\n* TODO Next\n"))))

(ert-deftest logs-ensure-child-reuses-existing ()
  (let ((text "* TODO Task\n** Logs\n- [2026-10-28 Wed 10:05] x\n"))
    (logs-test--with-org text
      (rsr/logs--ensure-child "Logs")
      (should (equal (buffer-string) text)))))

(ert-deftest logs-ensure-child-at-eob-without-newline ()
  (logs-test--with-org "* TODO Task\nbody"
    (rsr/logs--ensure-child "Logs")
    (should (equal (buffer-string) "* TODO Task\nbody\n** Logs\n"))))

(ert-deftest logs-subtree-region ()
  (logs-test--with-org "* TODO A\nx\n** B\ny\n* TODO C\n"
    (should (equal (rsr/logs--subtree-region (point-min))
                   (cons 1 (1+ (length "* TODO A\nx\n** B\ny\n")))))))

(ert-deftest logs-last-session-time-nil-when-empty ()
  (logs-test--with-org "* TODO Task\n** Logs\n"
    (goto-char (rsr/logs--find-child "Logs"))
    (should-not (rsr/logs--last-session-time))))

(ert-deftest logs-last-session-time-latest-in-logs-only ()
  (logs-test--with-org
      (concat "* TODO Task\n** Logs\n"
              "- Session [2026-10-27 Tue 09:00]\n  - [09:05] a\n"
              "- Session [2026-10-28 Wed 11:30]\n  - [11:31] b\n    more\n"
              "** Other\n- Session [2026-10-29 Thu 09:00]\n")
    (goto-char (rsr/logs--find-child "Logs"))
    (should (equal (rsr/logs--last-session-time)
                   (org-time-string-to-time "2026-10-28 Wed 11:30")))))

(ert-deftest logs-session-line-first-entry ()
  (should (equal (rsr/logs--session-line
                  nil (org-time-string-to-time "2026-10-28 Wed 10:00"))
                 "- Session [2026-10-28 Wed 10:00]\n")))

(ert-deftest logs-session-line-after-earlier-session ()
  (should (equal (rsr/logs--session-line
                  (org-time-string-to-time "2026-10-27 Tue 16:00")
                  (org-time-string-to-time "2026-10-28 Wed 10:00"))
                 "- Session [2026-10-28 Wed 10:00]\n")))

(ert-deftest logs-session-line-none-within-session ()
  (should (equal (rsr/logs--session-line
                  (org-time-string-to-time "2026-10-28 Wed 10:00")
                  (org-time-string-to-time "2026-10-28 Wed 10:00"))
                 "")))

(ert-deftest logs-session-line-none-in-clock-in-minute ()
  (should (equal (rsr/logs--session-line
                  (org-time-string-to-time "2026-10-28 Wed 10:00")
                  (encode-time (list 30 0 10 28 10 2026 nil -1 nil)))
                 "")))

(ert-deftest logs-capture-target-errors-when-not-clocked-in ()
  (let ((org-clock-marker (make-marker)))
    (should-error (rsr/logs-capture-target) :type 'user-error)))

(defmacro logs-test--with-capture (content task &rest body)
  "Run BODY with a logs template and a clock on TASK in a file holding CONTENT.
TASK is a regexp for the clocked heading; nil means the first line.
BODY sees the file's buffer as `buf'."
  (declare (indent 2))
  `(let* ((file (make-temp-file "logs-test-" nil ".org" ,content))
          (buf (find-file-noselect file))
          (org-capture-templates
           `(("d" "Logs" plain (function rsr/logs-capture-target)
              ,rsr/logs-capture-template :empty-lines 0
              :hook rsr/logs-capture-show-logs)))
          (org-clock-marker
           (with-current-buffer buf
             (goto-char (point-min))
             (when ,task (re-search-forward ,task))
             (copy-marker (line-beginning-position))))
          (org-clock-start-time (org-time-string-to-time "2000-01-01 Sat 10:00")))
     (unwind-protect
         (progn ,@body)
       ;; Detach the fake clock first, or kill-buffer asks "Clock-out …?"
       ;; and a batch run hangs forever waiting for y/n.
       (set-marker org-clock-marker nil)
       (with-current-buffer buf (set-buffer-modified-p nil))
       (kill-buffer buf)
       (delete-file file))))

(defun logs-test--capture-twice (content &optional task)
  "Capture two log lines into a temp file holding CONTENT; return its text.
TASK is a regexp for the clocked heading; nil means the first line."
  (logs-test--with-capture content task
    (dotimes (_ 2)
      (org-capture nil "d")
      (org-capture-finalize))
    (with-current-buffer buf (buffer-string))))

(defconst logs-test--session "- Session \\[2000-01-01 Sat 10:00\\]\n"
  "Regexp for the session line of the fake clock.")

(defconst logs-test--entry
  "  - \\[[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^]]+ [0-9][0-9]:[0-9][0-9]\\] *\n"
  "Regexp for one empty captured log line.")

(ert-deftest logs-capture-adds-session-once-and-two-lines ()
  (let ((text (logs-test--capture-twice "* TODO Task\n* TODO Next\n")))
    (should (string-match-p
             (concat "\\`\\* TODO Task\n\\*\\* Logs\n"
                     logs-test--session logs-test--entry logs-test--entry
                     "\\* TODO Next\n\\'")
             text))))

(ert-deftest logs-capture-appends-before-later-sibling ()
  (let ((text (logs-test--capture-twice
               (concat "* TODO Task\n** Context\n- plan\n"
                       "** Logs\n- Session [2000-01-01 Sat 10:00]\n  - old\n"
                       "** Overview\n*** Goal\n* TODO Next\n"))))
    (should (string-match-p
             (concat "\\`\\* TODO Task\n\\*\\* Context\n- plan\n"
                     "\\*\\* Logs\n" logs-test--session "  - old\n"
                     logs-test--entry logs-test--entry
                     "\\*\\* Overview\n\\*\\*\\* Goal\n\\* TODO Next\n\\'")
             text))))

(ert-deftest logs-capture-deep-task ()
  (let ((text (logs-test--capture-twice
               "* P\n** Dashboard\n*** Tasks\n**** TODO T\n**** TODO N\n"
               "^\\*+ TODO T")))
    (should (string-match-p
             (concat "\\`\\* P\n\\*\\* Dashboard\n\\*\\*\\* Tasks\n\\*\\*\\*\\* TODO T\n"
                     "\\*\\*\\*\\*\\* Logs\n"
                     logs-test--session logs-test--entry logs-test--entry
                     "\\*\\*\\*\\* TODO N\n\\'")
             text))))

(ert-deftest logs-capture-task-with-child-no-logs ()
  (let ((text (logs-test--capture-twice
               "* TODO Task\n** Notes\n- n\n* TODO Next\n")))
    (should (string-match-p
             (concat "\\`\\* TODO Task\n\\*\\* Notes\n- n\n\\*\\* Logs\n"
                     logs-test--session logs-test--entry logs-test--entry
                     "\\* TODO Next\n\\'")
             text))))

(ert-deftest logs-capture-at-eob-without-newline ()
  (let ((text (logs-test--capture-twice "* TODO Task\n** Logs\n- a")))
    (should (string-match-p
             (concat "\\`\\* TODO Task\n\\*\\* Logs\n- a\n"
                     logs-test--session logs-test--entry logs-test--entry
                     "\\'")
             text))))

(ert-deftest logs-capture-shows-whole-logs-and-kill-restores ()
  (let ((content (concat "* TODO Task\n** Logs\n- Session [1999-01-01 Fri 09:00]\n"
                         "  - old\n** Overview\n* TODO Next\n")))
    (logs-test--with-capture content nil
      (org-capture nil "d")
      (should (string-match-p
               (concat "\\`\\*\\* Logs\n- Session \\[1999[^]]+\\]\n  - old\n"
                       logs-test--session logs-test--entry "\\'")
               ;; Capture narrows to just before the new line's newline.
               (concat (buffer-string) "\n")))
      (org-capture-kill)
      (should (equal (with-current-buffer buf (buffer-string)) content)))))

(ert-deftest logs-capture-opens-on-the-right ()
  (logs-test--with-capture "* TODO Task\n" nil
    (delete-other-windows)
    (let ((origin (selected-window)))
      (org-capture nil "d")
      (should (string-prefix-p "CAPTURE-" (buffer-name)))
      (should (> (window-left-column) (window-left-column origin)))
      (org-capture-finalize))))

(ert-deftest logs-capture-buffer-p-only-for-logs ()
  (let ((org-capture-plist '(:target (function rsr/logs-capture-target))))
    (should (rsr/logs--capture-buffer-p "CAPTURE-x.org" nil))
    (should-not (rsr/logs--capture-buffer-p "x.org" nil)))
  (let ((org-capture-plist '(:target (file "x.org"))))
    (should-not (rsr/logs--capture-buffer-p "CAPTURE-x.org" nil))))

(defun logs-test--visible-line (text)
  "Return the visible part of the line containing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (let ((pos (line-beginning-position)) (end (line-end-position)) out)
    (while (< pos end)
      (unless (invisible-p pos) (push (char-after pos) out))
      (setq pos (1+ pos)))
    (concat (nreverse out))))

(defconst logs-test--stamp-text
  (concat "* TODO Task\n** Logs\n- Session [2026-10-31 Sat 22:00]\n"
          "  - [2026-10-31 Sat 22:45] entry\n"
          "  - [16:04] short\n"
          "    - [2026-10-31 Sat 22:46] nested\n"
          "- [2026-10-28 Wed 10:05] top\n"
          "*** Sub\n- [2026-10-31 Sat 22:47] deeper\n"
          "** Notes\n  - [2026-10-31 Sat 22:50] note\n")
  "A task whose Logs and Notes both hold stamped list items.")

(ert-deftest logs-short-stamps-only-under-logs ()
  (logs-test--with-org logs-test--stamp-text
    (font-lock-ensure)
    (should (equal (logs-test--visible-line "] entry") "  - [22:45] entry"))
    (should (equal (logs-test--visible-line "Session")
                   "- Session [2026-10-31 Sat 22:00]"))
    (should (equal (logs-test--visible-line "short") "  - [16:04] short"))
    (should (equal (logs-test--visible-line "nested") "    - [22:46] nested"))
    (should (equal (logs-test--visible-line "top") "- [10:05] top"))
    (should (equal (logs-test--visible-line "deeper") "- [22:47] deeper"))
    (should (equal (logs-test--visible-line "] note")
                   "  - [2026-10-31 Sat 22:50] note"))
    (should (equal (buffer-string) logs-test--stamp-text))))

(ert-deftest logs-short-stamps-toggle ()
  (logs-test--with-org logs-test--stamp-text
    (font-lock-ensure)
    (rsr/logs-toggle-short-stamps)
    (font-lock-ensure)
    (should (equal (logs-test--visible-line "] entry")
                   "  - [2026-10-31 Sat 22:45] entry"))
    (rsr/logs-toggle-short-stamps)
    (font-lock-ensure)
    (should (equal (logs-test--visible-line "] entry") "  - [22:45] entry"))))

(ert-deftest logs-task-heading-from-inside-logs ()
  (logs-test--with-org "* Project\n** TODO Task\n*** Logs\n- [2026-10-28 Wed 10:05] x\n"
    (search-forward "] x")
    (goto-char (rsr/logs--task-heading))
    (should (equal (org-get-heading t t t t) "Task"))))

(ert-deftest logs-task-heading-errors-outside-task ()
  (logs-test--with-org "* Project\nnotes\n"
    (search-forward "notes")
    (should-error (rsr/logs--task-heading) :type 'user-error)))

(ert-deftest logs-task-text-excludes-overview ()
  (logs-test--with-org
      (concat "* TODO Task\nbody\n** Logs\n- a\n"
              "** Overview\n*** Goal\n- old\n"
              "** Notes\n- keep\n* TODO Next\n")
    (should (equal (rsr/logs--task-text)
                   "* TODO Task\nbody\n** Logs\n- a\n** Notes\n- keep\n"))))

(ert-deftest logs-task-text-whole-subtree-without-overview ()
  (logs-test--with-org "* TODO Task\nbody\n* TODO Next\n"
    (should (equal (rsr/logs--task-text) "* TODO Task\nbody\n"))))

(ert-deftest logs-overview-command ()
  (let ((rsr/logs-overview-prompt-file "/p/logs-overview.org"))
    (should (equal (rsr/logs--overview-command "/tmp/task.org")
                   '("pi" "-p" "--no-session" "--no-context-files"
                     "--thinking" "low" "--tools" "read"
                     "--append-system-prompt" "/p/logs-overview.org"
                     "@/tmp/task.org" "Write the Overview for this task.")))))

(ert-deftest logs-normalize-demotes ()
  (should (equal (rsr/logs--normalize-output "* Goal\n- g\n* Next steps\n- n\n" 2)
                 "**** Goal\n- g\n**** Next steps\n- n\n")))

(ert-deftest logs-normalize-strips-preamble-and-fences ()
  (should (equal (rsr/logs--normalize-output
                  "Here you go:\n```org\n* Goal\n- g\n```\n" 1)
                 "*** Goal\n- g\n")))

(ert-deftest logs-normalize-rejects-unknown-section ()
  (should-error (rsr/logs--normalize-output "* Summary\n- x\n" 1)
                :type 'user-error))

(ert-deftest logs-normalize-rejects-no-headings ()
  (should-error (rsr/logs--normalize-output "nothing useful" 1)
                :type 'user-error))

(ert-deftest logs-replace-overview-creates-then-replaces ()
  (logs-test--with-org "* TODO Task\n** Context\n- plan\n** Logs\n- a\n* TODO Next\n"
    (rsr/logs--replace-overview (point-min) "*** Goal\n- one\n")
    (should (equal (buffer-string)
                   (concat "* TODO Task\n** Context\n- plan\n** Logs\n- a\n"
                           "** Overview\n*** Goal\n- one\n"
                           "* TODO Next\n")))
    (rsr/logs--replace-overview (point-min) "*** Goal\n- two\n")
    (should (equal (buffer-string)
                   (concat "* TODO Task\n** Context\n- plan\n** Logs\n- a\n"
                           "** Overview\n*** Goal\n- two\n"
                           "* TODO Next\n")))))

(ert-deftest logs-task-directory-is-task-file-directory ()
  (let* ((dir (make-temp-file "logs-dir-" t))
         (file (expand-file-name "notes.org" dir))
         (buf (find-file-noselect file)))
    (unwind-protect
        (should (equal (rsr/logs--task-directory
                        (with-current-buffer buf (point-marker)))
                       (file-name-as-directory dir)))
      (kill-buffer buf)
      (delete-directory dir t))))

;;; logs-test.el ends here
