;;; sittings-test.el --- Tests for the sitting model -*- lexical-binding: t; -*-

;; Run: cd ~/.emacs.d && /Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch \
;;        -L modules/personal -l tests/sittings-test.el -f ert-run-tests-batch-and-exit
;; Plan: ~/Stillness/Agent_Stuffs/Plans/Brain/working-sessions/

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'habits)
(require 'toggl)

(defmacro sittings-test--with-org (content &rest body)
  "Run BODY in an Org temp buffer holding CONTENT, point at start.
`org-mode' starts after the insert, so #+FILETAGS and #+CATEGORY are read."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defconst sittings-test--carved
  (concat "* TODO Trace one request through Flask :deep:\n"
          ":PROPERTIES:\n"
          ":OUTCOME: a note on the request path from WSGI to the view\n"
          ":NEEDS: flask checked out, the WSGI note open\n"
          ":Effort: 1:30\n"
          ":END:\n")
  "A carved deep sitting: heading, OUTCOME, NEEDS, Effort 1:30, :deep:.")

(ert-deftest sittings-deep-clock-mode-is-gone ()
  (should-not (fboundp 'my/org-clock-in-deep))
  (should-not (fboundp 'my/habit--record-clock-mode))
  (should-not (fboundp 'my/habit--add-mode-tag))
  (should-not (boundp 'my/habit-clock-mode))
  (should-not (memq 'my/habit--record-clock-mode org-clock-in-hook)))

(ert-deftest sittings-clock-in-out-writes-one-clock-line-only ()
  (sittings-test--with-org sittings-test--carved
    (let ((org-clock-out-remove-zero-time-clocks nil))
      (org-clock-in)
      (org-clock-out))
    (should (= 1 (count-matches "^[ \t]*CLOCK: " (point-min) (point-max))))
    (should (= 0 (count-matches "^[ \t]*- mode:" (point-min) (point-max))))
    ;; heading, drawer (5 lines), :LOGBOOK:, CLOCK, :END: — nothing else
    (should (= 9 (count-lines (point-min) (point-max))))))

(provide 'sittings-test)
;;; sittings-test.el ends here
