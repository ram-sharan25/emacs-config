;;; rests-test.el --- Tests for rest blocks -*- lexical-binding: t; -*-

;; Run: cd ~/.emacs.d && /Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch \
;;        -L modules/personal -l tests/rests-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'org)
(require 'rests)

(defmacro rests-test--with-file (content &rest body)
  "Run BODY with `my/rests-file' bound to a temp file holding CONTENT."
  (declare (indent 1))
  `(let ((my/rests-file (make-temp-file "rests" nil ".org" ,content)))
     (unwind-protect (progn ,@body)
       (when-let ((b (find-buffer-visiting my/rests-file))) (kill-buffer b))
       (delete-file my/rests-file))))

(defun rests-test--time (string)
  (encode-time (parse-time-string string)))

(ert-deftest rests-timestamp-is-the-day-only ()
  (should (equal (my/rest--timestamp (rests-test--time "2026-11-03 10:30:00"))
                 "<2026-11-03 Tue>")))

(ert-deftest rests-insert-writes-a-todo-scheduled-to-the-day ()
  (rests-test--with-file ""
    (my/rest--insert (rests-test--time "2026-11-03 10:30:00"))
    (with-temp-buffer
      (insert-file-contents my/rests-file)
      (should (string-match-p
               (concat "^\\* TODO Rest\nSCHEDULED: <2026-11-03 Tue>\n"
                       ":PROPERTIES:\n:ID: [-0-9A-Fa-f]+\n:TIMER_MINUTES: 20\n:END:\n")
               (buffer-string)))
      ;; One day's rest: no repeater, so DONE on the phone or laptop ends it.
      (should-not (string-match-p "[.+]+1d" (buffer-string))))))

(ert-deftest rests-clock-in-sets-the-timer-from-the-scheduled-range ()
  (rests-test--with-file
      "* TODO Rest\nSCHEDULED: <2099-01-01 Thu 10:30-10:55>\n:PROPERTIES:\n:TIMER_MINUTES: 20\n:END:\n"
    (with-current-buffer (find-file-noselect my/rests-file)
      (goto-char (point-min))
      (my/rest--timer-from-schedule)
      (should (equal (org-entry-get nil "TIMER_MINUTES") "25"))))
  (should (memq #'my/rest--timer-from-schedule org-clock-in-hook)))

(ert-deftest rests-clock-in-leaves-the-timer-when-there-is-no-range ()
  (rests-test--with-file
      "* TODO Rest\nSCHEDULED: <2099-01-01 Thu 10:30>\n:PROPERTIES:\n:TIMER_MINUTES: 20\n:END:\n"
    (with-current-buffer (find-file-noselect my/rests-file)
      (goto-char (point-min))
      (my/rest--timer-from-schedule)
      (should (equal (org-entry-get nil "TIMER_MINUTES") "20")))))

(ert-deftest rests-prune-drops-rests-before-today-and-keeps-the-rest ()
  (rests-test--with-file
      (concat "* DONE Rest\nSCHEDULED: <2020-01-01 Wed 10:30-10:50>\n:PROPERTIES:\n:TIMER_MINUTES: 20\n:END:\n"
              "* TODO Rest\nSCHEDULED: <2020-01-02 Thu 10:30-10:50>\n:PROPERTIES:\n:TIMER_MINUTES: 20\n:END:\n"
              "* TODO Rest\nSCHEDULED: <2099-01-01 Thu 10:30-10:50>\n:PROPERTIES:\n:TIMER_MINUTES: 20\n:END:\n")
    (my/rest--insert (rests-test--time "2099-01-02 15:00:00"))
    (with-temp-buffer
      (insert-file-contents my/rests-file)
      (should-not (string-match-p "2020-01-0[12]" (buffer-string)))
      (should (string-match-p "2099-01-01 Thu 10:30-10:50" (buffer-string)))
      (should (string-match-p "2099-01-02 Fri>" (buffer-string)))
      (should (= 2 (cl-count ?* (buffer-string)))))))

(ert-deftest rests-grid-key-is-bound-once-timeblock-loads ()
  (should (assq 'org-timeblock after-load-alist)))

(provide 'rests-test)
;;; rests-test.el ends here
