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

(ert-deftest sittings-no-deep-clock-key ()
  (unless (fboundp 'rsr/global-prefix-map)
    (define-prefix-command 'rsr/global-prefix-map))
  (load "keymaps")
  (should-not (keymap-lookup rsr/time-map "I"))
  (should-not (keymap-lookup (current-global-map) "C-c t I"))
  (should (eq (keymap-lookup rsr/time-map "k") #'my/habit-skip)))

(defun sittings-test--with-temp-org-file (content fn)
  "Call FN with the path of a temp .org file holding CONTENT; clean up after."
  (let ((file (make-temp-file "sittings-" nil ".org" content)))
    (unwind-protect
        (funcall fn file)
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest sittings-clock-intervals-have-no-mode ()
  (sittings-test--with-temp-org-file
   "* TODO Read\n:LOGBOOK:\nCLOCK: [2026-11-02 Mon 10:00]--[2026-11-02 Mon 11:30] =>  1:30\n:END:\n"
   (lambda (file)
     (should (equal (my/habit--clock-intervals file "2026-11-02")
                    '((90 "Read")))))))

(ert-deftest sittings-measure-file-spec-sums-intervals ()
  (sittings-test--with-temp-org-file
   "* TODO Read\n:LOGBOOK:\nCLOCK: [2026-11-02 Mon 10:00]--[2026-11-02 Mon 11:30] =>  1:30\n:END:\n"
   (lambda (file)
     (should (equal (my/habit--measure (list :measure (concat "file=" file)) "2026-11-02")
                    (list 1 90 '((90 "Read"))))))))

(ert-deftest sittings-hide-tags-regexp-keeps-only-deep ()
  (dolist (tg '("build" "work" "hub" "deeper" "deepwork" "d" "de" "dee"))
    (should (string-match-p org-agenda-hide-tags-regexp tg)))
  (should-not (string-match-p org-agenda-hide-tags-regexp "deep")))

(defun sittings-test--day-agenda (content)
  "Day agenda text, log mode on, for a temp agenda file holding CONTENT."
  (sittings-test--with-temp-org-file
   content
   (lambda (file)
     (let ((org-agenda-files (list file))
           (org-agenda-sticky nil)
           (org-agenda-start-with-log-mode '(closed clock state)))
       (org-agenda-list nil nil 'day)
       (prog1 (with-current-buffer org-agenda-buffer-name
                (buffer-substring-no-properties (point-min) (point-max)))
         (kill-buffer org-agenda-buffer-name))))))

(defun sittings-test--clocked (heading tag start end duration)
  "One task clocked today from START to END (HH:MM) => DURATION, TAG or nil."
  (let ((day (format-time-string "%Y-%m-%d %a")))
    (format "** TODO %s%s\n:LOGBOOK:\nCLOCK: [%s %s]--[%s %s] =>  %s\n:END:\n"
            heading (if tag (concat " " tag) "") day start day end duration)))

(ert-deftest sittings-day-agenda-shows-deep-tag-only ()
  (let ((text (sittings-test--day-agenda
               (concat "#+FILETAGS: :build:\n* Tasks [/]\n"
                       (sittings-test--clocked "Trace one request through Flask" ":deep:" "10:00" "11:30" "1:30")
                       (sittings-test--clocked "Answer two emails" nil "14:00" "14:30" "0:30")))))
    (should (string-match-p "Clocked: +(1:30) TODO Trace one request through Flask +:deep:" text))
    (should (string-match-p "Clocked: +(0:30) TODO Answer two emails *$" text))
    (should-not (string-match-p ":build:" text))))

(ert-deftest sittings-day-agenda-two-deep-sittings ()
  (let ((text (sittings-test--day-agenda
               (concat "#+FILETAGS: :build:\n* Tasks [/]\n"
                       (sittings-test--clocked "Trace one request through Flask" ":deep:" "10:00" "11:30" "1:30")
                       (sittings-test--clocked "Answer two emails" nil "13:00" "13:30" "0:30")
                       (sittings-test--clocked "Reimplement the routing commit" ":deep:" "15:00" "16:30" "1:30")))))
    (should (= 3 (length (seq-filter (lambda (l) (string-match-p "Clocked:" l))
                                     (split-string text "\n")))))
    (should (= 2 (length (seq-filter (lambda (l) (string-match-p "Clocked:.*:deep:" l))
                                     (split-string text "\n")))))
    (should-not (string-match-p "[Dd]eep hours\\|sessions" text))))

(ert-deftest sittings-toggl-tags-carry-deep ()
  (sittings-test--with-org (concat "#+FILETAGS: :build:\n#+CATEGORY: flask\n"
                                   sittings-test--carved)
    (org-next-visible-heading 1)
    (should (equal (rsr/toggl-tags-at-point) '("build" "deep" "flask")))))

(ert-deftest sittings-toggl-tags-shallow-and-hub ()
  (sittings-test--with-org "#+FILETAGS: :build:\n#+CATEGORY: flask\n* TODO Answer two emails\n"
    (org-next-visible-heading 1)
    (should (equal (rsr/toggl-tags-at-point) '("build" "flask"))))
  (sittings-test--with-org "#+FILETAGS: :hub:\n#+CATEGORY: Craft\n* TODO Answer two emails\n"
    (org-next-visible-heading 1)
    (should (equal (rsr/toggl-tags-at-point) nil)))
  (sittings-test--with-org "#+FILETAGS: :hub:\n#+CATEGORY: Craft\n* TODO Reimplement the routing commit :deep:\n"
    (org-next-visible-heading 1)
    (should (equal (rsr/toggl-tags-at-point) '("deep")))))

(defconst sittings-test--deep-clocktable
  "#+BEGIN: clocktable :scope agenda :block thisweek :maxlevel 2 :match \"deep\" :tags t :properties (\"AREA\") :inherit-props t :link t"
  "The dynamic block line the weekly review template must carry.")

(ert-deftest sittings-weekly-review-template-has-deep-clocktable ()
  (with-temp-buffer
    (insert-file-contents my/weekly-review-template-file)
    (goto-char (point-min))
    (re-search-forward "^# TEMPLATE-BEGIN")
    (let ((end (save-excursion (re-search-forward "^# TEMPLATE-END") (point))))
      (should (re-search-forward (concat "^" (regexp-quote sittings-test--deep-clocktable) "$") end t))
      (should (re-search-forward "^#\\+END:$" end t))
      (should (re-search-backward "C-c C-c" nil t)))))

(defun sittings-test--weekday (n)
  "\"YYYY-MM-DD Day\" of day N of the current week, 0 being Monday."
  (let* ((now (current-time))
         (dow (string-to-number (format-time-string "%u" now)))
         (day (time-add now (days-to-time (- n (1- dow))))))
    (format-time-string "%Y-%m-%d %a" day)))

(defun sittings-test--sitting-on (day heading tag duration &optional end)
  "One task clocked on DAY from 10:00 to END (default 11:30) => DURATION.
Org recomputes the duration from the timestamps, so END must agree with it."
  (format "** TODO %s%s\n:LOGBOOK:\nCLOCK: [%s 10:00]--[%s %s] =>  %s\n:END:\n"
          heading (if tag (concat " " tag) "") day day (or end "11:30") duration))

(defun sittings-test--clocktable (file params)
  "Text of a clocktable with PARAMS appended to FILE and updated, FILE being the agenda."
  (with-current-buffer (find-file-noselect file)
    (let ((org-agenda-files (list file)))
      (goto-char (point-max))
      (insert "\n" params "\n#+END:\n")
      (search-backward "#+BEGIN:")
      (org-dblock-update)
      (prog1 (buffer-substring-no-properties (point) (point-max))
        (set-buffer-modified-p nil)))))

(ert-deftest sittings-deep-clocktable-lists-each-sitting ()
  (sittings-test--with-temp-org-file
   (concat "#+PROPERTY: AREA Craft\n#+FILETAGS: :build:\n* Tasks [/]\n"
           (sittings-test--sitting-on (sittings-test--weekday 0) "Trace one request through Flask" ":deep:" "1:30")
           (sittings-test--sitting-on (sittings-test--weekday 1) "Reimplement the routing commit" ":deep:" "1:30")
           (sittings-test--sitting-on (sittings-test--weekday 2) "Answer two emails" nil "0:30" "10:30"))
   (lambda (file)
     (let ((deep (sittings-test--clocktable file sittings-test--deep-clocktable))
           (all (sittings-test--clocktable
                 file "#+BEGIN: clocktable :scope agenda :block thisweek :maxlevel 2")))
       (should (string-match-p "Trace one request through Flask" deep))
       (should (string-match-p "Reimplement the routing commit" deep))
       (should-not (string-match-p "Answer two emails" deep))
       (should (= 2 (length (seq-filter (lambda (l) (string-match-p "build, deep" l))
                                        (split-string deep "\n")))))
       (should (string-match-p "| Craft |" deep))
       (should (string-match-p "\\*3:00\\*" deep))
       ;; the unfiltered table on the project sums all three sittings
       (should (string-match-p "\\*3:30\\*" all))
       (should (string-match-p "Answer two emails" all))))))

(provide 'sittings-test)
;;; sittings-test.el ends here
