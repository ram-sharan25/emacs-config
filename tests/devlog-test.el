;;; devlog-test.el --- Tests for devlog.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'devlog)

(defmacro devlog-test--with-org (content &rest body)
  "Run BODY in an Org temp buffer holding CONTENT, point at start."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(ert-deftest devlog-find-child-nil-when-missing ()
  (devlog-test--with-org "* TODO Task\n** Notes\n"
    (should-not (rsr/devlog--find-child "Devlog"))))

(ert-deftest devlog-find-child-direct-child-only ()
  (devlog-test--with-org "* TODO Task\n** Notes\n*** Devlog\n** Devlog\n"
    (goto-char (rsr/devlog--find-child "Devlog"))
    (should (= (org-current-level) 2))))

(ert-deftest devlog-ensure-child-creates-last-child ()
  (devlog-test--with-org "* TODO Task\nbody\n** Notes\n* TODO Next\n"
    (rsr/devlog--ensure-child "Devlog")
    (should (equal (buffer-string)
                   "* TODO Task\nbody\n** Notes\n** Devlog\n* TODO Next\n"))))

(ert-deftest devlog-ensure-child-reuses-existing ()
  (let ((text "* TODO Task\n** Devlog\n- [2026-10-28 Wed 10:05] x\n"))
    (devlog-test--with-org text
      (rsr/devlog--ensure-child "Devlog")
      (should (equal (buffer-string) text)))))

(ert-deftest devlog-ensure-child-at-eob-without-newline ()
  (devlog-test--with-org "* TODO Task\nbody"
    (rsr/devlog--ensure-child "Devlog")
    (should (equal (buffer-string) "* TODO Task\nbody\n** Devlog\n"))))

(ert-deftest devlog-subtree-region ()
  (devlog-test--with-org "* TODO A\nx\n** B\ny\n* TODO C\n"
    (should (equal (rsr/devlog--subtree-region (point-min))
                   (cons 1 (1+ (length "* TODO A\nx\n** B\ny\n")))))))

(ert-deftest devlog-last-session-time-nil-when-empty ()
  (devlog-test--with-org "* TODO Task\n** Devlog\n"
    (goto-char (rsr/devlog--find-child "Devlog"))
    (should-not (rsr/devlog--last-session-time))))

(ert-deftest devlog-last-session-time-latest-in-devlog-only ()
  (devlog-test--with-org
      (concat "* TODO Task\n** Devlog\n"
              "- Session [2026-10-27 Tue 09:00]\n  - [09:05] a\n"
              "- Session [2026-10-28 Wed 11:30]\n  - [11:31] b\n    more\n"
              "** Other\n- Session [2026-10-29 Thu 09:00]\n")
    (goto-char (rsr/devlog--find-child "Devlog"))
    (should (equal (rsr/devlog--last-session-time)
                   (org-time-string-to-time "2026-10-28 Wed 11:30")))))

(ert-deftest devlog-session-line-first-entry ()
  (should (equal (rsr/devlog--session-line
                  nil (org-time-string-to-time "2026-10-28 Wed 10:00"))
                 "- Session [2026-10-28 Wed 10:00]\n")))

(ert-deftest devlog-session-line-after-earlier-session ()
  (should (equal (rsr/devlog--session-line
                  (org-time-string-to-time "2026-10-27 Tue 16:00")
                  (org-time-string-to-time "2026-10-28 Wed 10:00"))
                 "- Session [2026-10-28 Wed 10:00]\n")))

(ert-deftest devlog-session-line-none-within-session ()
  (should (equal (rsr/devlog--session-line
                  (org-time-string-to-time "2026-10-28 Wed 10:00")
                  (org-time-string-to-time "2026-10-28 Wed 10:00"))
                 "")))

(ert-deftest devlog-session-line-none-in-clock-in-minute ()
  (should (equal (rsr/devlog--session-line
                  (org-time-string-to-time "2026-10-28 Wed 10:00")
                  (encode-time (list 30 0 10 28 10 2026 nil -1 nil)))
                 "")))

(ert-deftest devlog-capture-target-errors-when-not-clocked-in ()
  (let ((org-clock-marker (make-marker)))
    (should-error (rsr/devlog-capture-target) :type 'user-error)))

(defun devlog-test--capture-twice (content)
  "Capture two devlog lines into a temp file holding CONTENT; return its text."
  (let* ((file (make-temp-file "devlog-test-" nil ".org" content))
         (buf (find-file-noselect file))
         (org-capture-templates
          `(("d" "Devlog" plain (function rsr/devlog-capture-target)
             ,rsr/devlog-capture-template :immediate-finish t :empty-lines 0)))
         (org-clock-marker (with-current-buffer buf (copy-marker (point-min))))
         (org-clock-start-time (org-time-string-to-time "2000-01-01 Sat 10:00")))
    (unwind-protect
        (progn
          (org-capture nil "d")
          (org-capture nil "d")
          (with-current-buffer buf (buffer-string)))
      ;; Detach the fake clock first, or kill-buffer asks "Clock-out …?"
      ;; and a batch run hangs forever waiting for y/n.
      (set-marker org-clock-marker nil)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file file))))

(ert-deftest devlog-capture-adds-session-once-and-two-lines ()
  (let ((text (devlog-test--capture-twice "* TODO Task\n* TODO Next\n")))
    (should (string-match-p
             (concat "\\`\\* TODO Task\n\\*\\* Devlog\n"
                     "- Session \\[2000-01-01 Sat 10:00\\]\n"
                     "  - \\[[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^]]+ [0-9][0-9]:[0-9][0-9]\\] *\n"
                     "  - \\[[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^]]+ [0-9][0-9]:[0-9][0-9]\\] *\n"
                     "\\* TODO Next\n")
             text))))

(ert-deftest devlog-task-heading-from-inside-devlog ()
  (devlog-test--with-org "* Project\n** TODO Task\n*** Devlog\n- [2026-10-28 Wed 10:05] x\n"
    (search-forward "] x")
    (goto-char (rsr/devlog--task-heading))
    (should (equal (org-get-heading t t t t) "Task"))))

(ert-deftest devlog-task-heading-errors-outside-task ()
  (devlog-test--with-org "* Project\nnotes\n"
    (search-forward "notes")
    (should-error (rsr/devlog--task-heading) :type 'user-error)))

(ert-deftest devlog-task-text-excludes-overview ()
  (devlog-test--with-org
      (concat "* TODO Task\nbody\n** Devlog\n- a\n"
              "** Overview\n*** Goal\n- old\n"
              "** Notes\n- keep\n* TODO Next\n")
    (should (equal (rsr/devlog--task-text)
                   "* TODO Task\nbody\n** Devlog\n- a\n** Notes\n- keep\n"))))

(ert-deftest devlog-task-text-whole-subtree-without-overview ()
  (devlog-test--with-org "* TODO Task\nbody\n* TODO Next\n"
    (should (equal (rsr/devlog--task-text) "* TODO Task\nbody\n"))))

(ert-deftest devlog-overview-command ()
  (let ((rsr/devlog-overview-prompt-file "/p/devlog-overview.org"))
    (should (equal (rsr/devlog--overview-command "/tmp/task.org")
                   '("pi" "-p" "--no-session" "--no-context-files"
                     "--thinking" "low" "--tools" "read"
                     "--append-system-prompt" "/p/devlog-overview.org"
                     "@/tmp/task.org" "Write the Overview for this task.")))))

(ert-deftest devlog-normalize-demotes ()
  (should (equal (rsr/devlog--normalize-output "* Goal\n- g\n* Next steps\n- n\n" 2)
                 "**** Goal\n- g\n**** Next steps\n- n\n")))

(ert-deftest devlog-normalize-strips-preamble-and-fences ()
  (should (equal (rsr/devlog--normalize-output
                  "Here you go:\n```org\n* Goal\n- g\n```\n" 1)
                 "*** Goal\n- g\n")))

(ert-deftest devlog-normalize-rejects-unknown-section ()
  (should-error (rsr/devlog--normalize-output "* Summary\n- x\n" 1)
                :type 'user-error))

(ert-deftest devlog-normalize-rejects-no-headings ()
  (should-error (rsr/devlog--normalize-output "nothing useful" 1)
                :type 'user-error))

(ert-deftest devlog-replace-overview-creates-then-replaces ()
  (devlog-test--with-org "* TODO Task\n** Context\n- plan\n** Devlog\n- a\n* TODO Next\n"
    (rsr/devlog--replace-overview (point-min) "*** Goal\n- one\n")
    (should (equal (buffer-string)
                   (concat "* TODO Task\n** Context\n- plan\n** Devlog\n- a\n"
                           "** Overview\n*** Goal\n- one\n"
                           "* TODO Next\n")))
    (rsr/devlog--replace-overview (point-min) "*** Goal\n- two\n")
    (should (equal (buffer-string)
                   (concat "* TODO Task\n** Context\n- plan\n** Devlog\n- a\n"
                           "** Overview\n*** Goal\n- two\n"
                           "* TODO Next\n")))))

(ert-deftest devlog-task-directory-is-task-file-directory ()
  (let* ((dir (make-temp-file "devlog-dir-" t))
         (file (expand-file-name "notes.org" dir))
         (buf (find-file-noselect file)))
    (unwind-protect
        (should (equal (rsr/devlog--task-directory
                        (with-current-buffer buf (point-marker)))
                       (file-name-as-directory dir)))
      (kill-buffer buf)
      (delete-directory dir t))))

;;; devlog-test.el ends here
