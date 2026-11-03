;;; mind-state-test.el --- Tests for mind-state.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mind-state)

(defconst mind-state-test--time (encode-time 0 30 9 2 11 2026)
  "2026-11-02 09:30, a Monday.")

(defmacro mind-state-test--with-log-dir (&rest body)
  "Run BODY with `mind-state-log-dir' bound to a fresh temp directory.
Buffers visiting files in it are killed and the directory deleted afterwards."
  (declare (indent 0))
  `(let ((mind-state-log-dir (make-temp-file "mind-state-" t)))
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p mind-state-log-dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory mind-state-log-dir t))))

(defmacro mind-state-test--with-clock (content &rest body)
  "Run BODY with the Org clock in the LAST heading of CONTENT, a temp Org buffer.
Binds `org-clock-marker' and `org-clock-hd-marker' to that heading and
`org-clock-start-time' to `mind-state-test--time'."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-max))
     (re-search-backward org-heading-regexp)
     (let ((org-clock-marker (point-marker))
           (org-clock-hd-marker (point-marker))
           (org-clock-start-time mind-state-test--time))
       ,@body)))

(defmacro mind-state-test--with-input (inputs calls &rest body)
  "Run BODY answering the prompts from INPUTS, strings \"word note...\".
For each string `completing-read' returns the first word and the following
`read-string' returns the rest.  Each `completing-read' argument list is
pushed onto the variable named by CALLS."
  (declare (indent 2))
  `(let ((mind-state-test--queue (copy-sequence ,inputs))
         (mind-state-test--note ""))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (&rest args)
                  (push args ,calls)
                  (let ((next (split-string (pop mind-state-test--queue) " " t)))
                    (setq mind-state-test--note (string-join (cdr next) " "))
                    (car next))))
               ((symbol-function 'read-string)
                (lambda (&rest _) mind-state-test--note)))
       ,@body)))

(defmacro mind-state-test--at (time &rest body)
  "Run BODY with `mind-state--now' returning TIME."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'mind-state--now) (lambda () ,time)))
     ,@body))

(ert-deftest mind-state-words-are-the-nine ()
  (should (equal mind-state-words
                 '("absorbed" "steady" "stuck" "tense" "scattered"
                   "restless" "fresh" "tired" "flat"))))

(ert-deftest mind-state-every-word-has-a-meaning ()
  (dolist (w mind-state-words)
    (should (stringp (cdr (assoc w mind-state-word-meanings)))))
  (should (= 9 (length mind-state-word-meanings))))

(ert-deftest mind-state-interruption-causes-include-walk ()
  (should (equal mind-state-interruption-causes
                 '("slack" "coworker" "phone" "pitstop" "walk" "other"))))

(ert-deftest mind-state-settings-defaults ()
  (should (equal mind-state-log-dir my/trackers-dir))
  (should (equal mind-state-deep-tag "deep")))

(ert-deftest mind-state-event-re-groups ()
  (let ((line "**** 09:40 break scattered [[id:AB12][Write the parser]] slack ping :auto:"))
    (should (string-match mind-state-event-re line))
    (should (equal (match-string 1 line) "09:40"))
    (should (equal (match-string 2 line) "break"))
    (should (equal (match-string 3 line) "scattered"))
    (should (equal (match-string 4 line) "AB12"))
    (should (equal (match-string 5 line) "Write the parser"))
    (should (equal (string-trim (match-string 6 line)) "slack ping :auto:")))
  (let ((line "**** 09:30 check tired"))
    (should (string-match mind-state-event-re line))
    (should-not (match-string 4 line))
    (should (equal (match-string 6 line) "")))
  (let ((line "**** 10:05 interrupt pitstop [[id:AB12][Write the parser]] coffee"))
    (should (string-match mind-state-event-re line))
    (should (equal (match-string 2 line) "interrupt"))
    (should (equal (match-string 3 line) "pitstop"))
    (should (equal (match-string 4 line) "AB12"))))

(ert-deftest mind-state-format-line-minimal ()
  (should (equal (mind-state--format-line mind-state-test--time "check" "tired")
                 "**** 09:30 check tired")))

(ert-deftest mind-state-format-line-full ()
  (should (equal (mind-state--format-line mind-state-test--time "break" "scattered"
                                          "AB12" "Write the parser" "slack ping" t)
                 "**** 09:30 break scattered [[id:AB12][Write the parser]] slack ping :auto:")))

(ert-deftest mind-state-format-line-blank-note-omitted ()
  (should (equal (mind-state--format-line mind-state-test--time "start" "fresh"
                                          "AB12" "T" "   ")
                 "**** 09:30 start fresh [[id:AB12][T]]")))

(ert-deftest mind-state-format-line-auto-without-link ()
  (should (equal (mind-state--format-line mind-state-test--time "check" "flat"
                                          nil nil "" t)
                 "**** 09:30 check flat :auto:")))

(defun mind-state-test--file-text (time)
  "Contents of the log file for TIME."
  (with-temp-buffer
    (insert-file-contents (mind-state-log-file time))
    (buffer-string)))

(ert-deftest mind-state-log-file-is-yearly-under-log-dir ()
  (mind-state-test--with-log-dir
    (should (equal (mind-state-log-file mind-state-test--time)
                   (expand-file-name "mind-state-log-2026.org" mind-state-log-dir)))))

(ert-deftest mind-state-append-line-creates-file-header-and-datetree ()
  (mind-state-test--with-log-dir
    (should-not (file-exists-p (mind-state-log-file mind-state-test--time)))
    (mind-state--append-line mind-state-test--time "**** 09:30 check tired")
    (let ((text (mind-state-test--file-text mind-state-test--time)))
      (should (string-prefix-p "#+TITLE: Mind-state log 2026\n#+STARTUP: overview\n" text))
      (should (string-match-p "^# One line per event: HH:MM kind value \\[\\[id:TASK\\]\\[Title\\]\\] note :auto:$" text))
      (should (string-suffix-p
               "\n* 2026\n** 2026-11 November\n*** 2026-11-02 Monday\n**** 09:30 check tired\n"
               text)))))

(ert-deftest mind-state-append-line-second-line-same-day ()
  (mind-state-test--with-log-dir
    (mind-state--append-line mind-state-test--time "**** 09:30 check tired")
    (mind-state--append-line (time-add mind-state-test--time 600) "**** 09:40 break scattered")
    (let ((text (mind-state-test--file-text mind-state-test--time)))
      (should (= 1 (with-temp-buffer (insert text)
                                     (count-matches "^\\*\\*\\* " (point-min) (point-max)))))
      (should (string-suffix-p
               "*** 2026-11-02 Monday\n**** 09:30 check tired\n**** 09:40 break scattered\n"
               text)))))

(ert-deftest mind-state-append-line-next-day-gets-new-heading ()
  (mind-state-test--with-log-dir
    (mind-state--append-line mind-state-test--time "**** 09:30 check tired")
    (mind-state--append-line (encode-time 0 0 10 3 11 2026) "**** 10:00 check flat :auto:")
    (should (string-suffix-p
             "*** 2026-11-02 Monday\n**** 09:30 check tired\n*** 2026-11-03 Tuesday\n**** 10:00 check flat :auto:\n"
             (mind-state-test--file-text mind-state-test--time)))))

(ert-deftest mind-state-annotate-shows-meaning-in-brackets ()
  (should (equal (mind-state--annotate "scattered") " (pulled outward)"))
  (should-not (mind-state--annotate "foo")))

(ert-deftest mind-state-read-event-asks-word-then-note ()
  (let (calls prompts)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args) (push args calls) (push 'word prompts) "flat"))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _) (push prompt prompts) "  slack ping ")))
      (should (equal (mind-state--read-event) '("flat" . "slack ping"))))
    (should (equal (reverse prompts) '(word "Note (optional): ")))
    (should (= 1 (length calls)))
    (let ((args (car calls)))
      (should (equal (nth 0 args) "Mind-state: "))
      (should (equal (nth 1 args) mind-state-words))
      (should (eq (nth 3 args) t)))))                 ; require-match

(ert-deftest mind-state-read-event-empty-note-is-empty-string ()
  (let (calls)
    (mind-state-test--with-input '("fresh") calls
      (should (equal (mind-state--read-event) '("fresh" . ""))))))

(ert-deftest mind-state-read-event-asks-again-until-a-word ()
  (let (calls)
    (mind-state-test--with-input '("" "foo" "tense x") calls
      (should (equal (mind-state--read-event) '("tense" . "x")))
      (should (= 3 (length calls))))))

(ert-deftest mind-state-read-event-sets-annotation-function ()
  (let (seen)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _)
                 (setq seen completion-extra-properties)
                 "flat"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (mind-state--read-event))
    (should (equal seen '(:annotation-function mind-state--annotate)))))

(ert-deftest mind-state-read-interruption-asks-cause-then-note ()
  (let (completion-args note-prompt)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args) (setq completion-args args) "slack"))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _) (setq note-prompt prompt) " channel ping ")))
      (should (equal (mind-state--read-interruption) '("slack" . "channel ping"))))
    (should (equal (nth 0 completion-args) "Interruption: "))
    (should (equal (nth 1 completion-args) mind-state-interruption-causes))
    (should (eq (nth 3 completion-args) t))
    (should (equal note-prompt "Note (optional): "))))

(ert-deftest mind-state-read-interruption-other-requires-note ()
  (let ((notes '("" "  " "delivery at door")) prompts)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "other"))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (push prompt prompts)
                 (pop notes))))
      (should (equal (mind-state--read-interruption)
                     '("other" . "delivery at door"))))
    (should (equal (reverse prompts)
                   '("What interrupted you: "
                     "Describe the interruption: "
                     "Describe the interruption: ")))))

(ert-deftest mind-state-clocked-deep-p-own-tag ()
  (mind-state-test--with-clock "* TODO Sitting :deep:\n"
    (should (mind-state--clocked-deep-p))))

(ert-deftest mind-state-clocked-deep-p-inherited-tag ()
  (mind-state-test--with-clock "* Parent :deep:\n** TODO Sitting\n"
    (should (mind-state--clocked-deep-p))))

(ert-deftest mind-state-clocked-deep-p-nil-when-shallow ()
  (mind-state-test--with-clock "* TODO Sitting :work:\n"
    (should-not (mind-state--clocked-deep-p))))

(ert-deftest mind-state-clocked-deep-p-nil-when-not-clocking ()
  (should-not (mind-state--clocked-deep-p)))

(ert-deftest mind-state-clocked-deep-p-follows-the-variable ()
  (mind-state-test--with-clock "* TODO Sitting :focus:\n"
    (let ((mind-state-deep-tag "focus"))
      (should (mind-state--clocked-deep-p)))))

(ert-deftest mind-state-clock-context-with-id ()
  (mind-state-test--with-clock "* TODO Write the parser :deep:\n:PROPERTIES:\n:ID: AB12\n:END:\n"
    (let ((ctx (mind-state--clock-context)))
      (should (equal (plist-get ctx :id) "AB12"))
      (should (equal (plist-get ctx :title) "Write the parser"))
      (should (equal (plist-get ctx :start) mind-state-test--time)))))

(ert-deftest mind-state-clock-context-without-id-creates-none ()
  (mind-state-test--with-clock "* TODO Write the parser\n"
    (let ((before (buffer-string))
          (ctx (mind-state--clock-context)))
      (should-not (plist-get ctx :id))
      (should (equal (plist-get ctx :title) "Write the parser"))
      (should (equal (buffer-string) before)))))

(ert-deftest mind-state-clock-context-nil-when-not-clocking ()
  (should-not (mind-state--clock-context)))

(defmacro mind-state-test--guards-open (&rest body)
  "Run BODY with active hours, idle guard and counter all permitting a prompt."
  (declare (indent 0))
  `(let ((mind-state-active-hours '(0 24))
         (mind-state-prompts-today 0)
         (mind-state-target-prompts 10))
     ,@body))

(ert-deftest mind-state-should-prompt-p-t-when-shallow ()
  (mind-state-test--guards-open
    (mind-state-test--with-clock "* TODO Sitting\n"
      (should (mind-state--should-prompt-p)))))

(ert-deftest mind-state-should-prompt-p-t-when-not-clocking ()
  (mind-state-test--guards-open
    (should (mind-state--should-prompt-p))))

(ert-deftest mind-state-should-prompt-p-nil-when-deep ()
  (mind-state-test--guards-open
    (mind-state-test--with-clock "* TODO Sitting :deep:\n"
      (should-not (mind-state--should-prompt-p)))))

(ert-deftest mind-state-should-prompt-p-nil-when-deep-inherited ()
  (mind-state-test--guards-open
    (mind-state-test--with-clock "* Parent :deep:\n** TODO Sitting\n"
      (should-not (mind-state--should-prompt-p)))))

(defun mind-state-test--ctx (&optional id)
  "A clock context for task ID (nil for a task without one) started at TIME."
  (list :id id :title "T" :start mind-state-test--time))

(ert-deftest mind-state-kind-check-when-not-clocked ()
  (should (equal "check" (mind-state--kind nil nil mind-state-test--time))))

(ert-deftest mind-state-kind-check-for-random-prompt-even-when-clocked ()
  (mind-state-test--with-log-dir
    (should (equal "check" (mind-state--kind (mind-state-test--ctx "AB12") t
                                             mind-state-test--time)))))

(ert-deftest mind-state-kind-start-when-log-empty ()
  (mind-state-test--with-log-dir
    (should (equal "start" (mind-state--kind (mind-state-test--ctx "AB12") nil
                                             mind-state-test--time)))
    (should-not (file-exists-p (mind-state-log-file mind-state-test--time)))))

(ert-deftest mind-state-kind-break-after-start-line-this-clock ()
  (mind-state-test--with-log-dir
    (mind-state--append-line mind-state-test--time "**** 09:31 start fresh [[id:AB12][T]]")
    (should (equal "break" (mind-state--kind (mind-state-test--ctx "AB12") nil
                                             (time-add mind-state-test--time 600))))))

(ert-deftest mind-state-kind-start-when-earlier-line-predates-clock ()
  (mind-state-test--with-log-dir
    (mind-state--append-line mind-state-test--time "**** 09:00 break scattered [[id:AB12][T]] ping")
    (should (equal "start" (mind-state--kind (mind-state-test--ctx "AB12") nil
                                             mind-state-test--time)))))

(ert-deftest mind-state-kind-start-when-line-is-another-task ()
  (mind-state-test--with-log-dir
    (mind-state--append-line mind-state-test--time "**** 09:31 start fresh [[id:ZZ99][Other]]")
    (should (equal "start" (mind-state--kind (mind-state-test--ctx "AB12") nil
                                             (time-add mind-state-test--time 600))))))

(ert-deftest mind-state-kind-ignores-random-check-line ()
  (mind-state-test--with-log-dir
    (mind-state--append-line mind-state-test--time "**** 09:31 check steady [[id:AB12][T]] :auto:")
    (should (equal "start" (mind-state--kind (mind-state-test--ctx "AB12") nil
                                             (time-add mind-state-test--time 600))))))

(ert-deftest mind-state-kind-break-across-midnight ()
  (mind-state-test--with-log-dir
    (let* ((start (encode-time 0 50 23 2 11 2026))
           (ctx (list :id "AB12" :title "T" :start start)))
      (mind-state--append-line start "**** 23:55 start tense [[id:AB12][T]]")
      (should (equal "break" (mind-state--kind ctx nil (encode-time 0 10 0 3 11 2026)))))))

(ert-deftest mind-state-kind-without-id-matches-unlinked-lines ()
  (mind-state-test--with-log-dir
    (mind-state--append-line mind-state-test--time "**** 09:31 start fresh [[id:AB12][T]]")
    (should (equal "start" (mind-state--kind (mind-state-test--ctx nil) nil
                                             (time-add mind-state-test--time 600))))
    (mind-state--append-line mind-state-test--time "**** 09:32 start fresh")
    (should (equal "break" (mind-state--kind (mind-state-test--ctx nil) nil
                                             (time-add mind-state-test--time 600))))))

(defmacro mind-state-test--with-schedule-stub (count &rest body)
  "Run BODY with `mind-state--schedule-next-prompt' replaced by a counter in COUNT."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'mind-state--schedule-next-prompt)
              (lambda () (cl-incf ,count))))
     ,@body))

(ert-deftest mind-state-manual-log-start-line-with-link ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (mind-state-test--with-clock "* TODO Write the parser\n:PROPERTIES:\n:ID: AB12\n:END:\n"
        (let (calls (mind-state-prompts-today 3))
          (mind-state-test--with-input '("fresh") calls
            (mind-state-manual-log))
          (should (= mind-state-prompts-today 3))
          (should (equal (mind-state--lines-for-day mind-state-test--time)
                         '("**** 09:30 start fresh [[id:AB12][Write the parser]]"))))))))

(ert-deftest mind-state-manual-log-second-line-is-break-with-note ()
  (mind-state-test--with-log-dir
    (mind-state-test--with-clock "* TODO Write the parser\n:PROPERTIES:\n:ID: AB12\n:END:\n"
      (let (calls)
        (mind-state-test--at mind-state-test--time
          (mind-state-test--with-input '("fresh") calls (mind-state-manual-log)))
        (mind-state-test--at (time-add mind-state-test--time 900)
          (mind-state-test--with-input '("scattered slack ping") calls (mind-state-manual-log)))
        (should (equal (mind-state--lines-for-day mind-state-test--time)
                       '("**** 09:30 start fresh [[id:AB12][Write the parser]]"
                         "**** 09:45 break scattered [[id:AB12][Write the parser]] slack ping")))))))

(ert-deftest mind-state-manual-log-task-without-id-has-no-link-and-is-untouched ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (mind-state-test--with-clock "* TODO Write the parser\n"
        (let (calls (before (buffer-string)))
          (mind-state-test--with-input '("fresh") calls (mind-state-manual-log))
          (should (equal (buffer-string) before))
          (should (equal (mind-state--lines-for-day mind-state-test--time)
                         '("**** 09:30 start fresh"))))))))

(ert-deftest mind-state-manual-log-not-clocked-is-check ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (let (calls)
        (mind-state-test--with-input '("tired") calls (mind-state-manual-log))
        (should (equal (mind-state--lines-for-day mind-state-test--time)
                       '("**** 09:30 check tired")))))))

(ert-deftest mind-state-log-interruption-with-clock-links-task ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (mind-state-test--with-clock "* TODO Write the parser\n:PROPERTIES:\n:ID: AB12\n:END:\n"
        (cl-letf (((symbol-function 'mind-state--read-interruption)
                   (lambda () '("pitstop" . "coffee"))))
          (mind-state-log-interruption nil))
        (should (equal (mind-state--lines-for-day mind-state-test--time)
                       '("**** 09:30 interrupt pitstop [[id:AB12][Write the parser]] coffee")))))))

(ert-deftest mind-state-log-interruption-without-clock-has-no-link ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (cl-letf (((symbol-function 'mind-state--read-interruption)
                 (lambda () '("coworker" . "question"))))
        (mind-state-log-interruption nil))
      (should (equal (mind-state--lines-for-day mind-state-test--time)
                     '("**** 09:30 interrupt coworker question"))))))

(ert-deftest mind-state-log-interruption-after-is-marked ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (cl-letf (((symbol-function 'mind-state--read-interruption)
                 (lambda () '("phone" . "family call"))))
        (mind-state-log-interruption t))
      (should (equal (mind-state--lines-for-day mind-state-test--time)
                     '("**** 09:30 interrupt phone family call :after:"))))))

(ert-deftest mind-state-prompt-user-shallow-task-logs-check-auto ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (mind-state-test--guards-open
        (mind-state-test--with-clock "* TODO Write the parser\n:PROPERTIES:\n:ID: AB12\n:END:\n"
          (let (calls (scheduled 0) (mind-state-today-date nil))
            (mind-state-test--with-schedule-stub scheduled
              (mind-state-test--with-input '("flat") calls (mind-state--prompt-user)))
            (should (= scheduled 1))
            (should (= mind-state-prompts-today 1))
            (should (equal (mind-state--lines-for-day mind-state-test--time)
                           '("**** 09:30 check flat [[id:AB12][Write the parser]] :auto:")))))))))

(ert-deftest mind-state-prompt-user-deep-task-asks-nothing-and-reschedules ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (mind-state-test--guards-open
        (mind-state-test--with-clock "* Parent :deep:\n** TODO Sitting\n"
          (let (calls (scheduled 0) (mind-state-today-date nil))
            (mind-state-test--with-schedule-stub scheduled
              (mind-state-test--with-input '("flat") calls (mind-state--prompt-user)))
            (should-not calls)
            (should (= scheduled 1))
            (should (= mind-state-prompts-today 0))
            (should-not (file-exists-p (mind-state-log-file mind-state-test--time)))))))))

(ert-deftest mind-state-prompt-user-quit-logs-nothing-and-reschedules ()
  (mind-state-test--with-log-dir
    (mind-state-test--at mind-state-test--time
      (mind-state-test--guards-open
        (let ((scheduled 0) (mind-state-today-date nil))
          (mind-state-test--with-schedule-stub scheduled
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (signal 'quit nil))))
              (condition-case nil (mind-state--prompt-user) (quit nil))))
          (should (= scheduled 1))
          (should (= mind-state-prompts-today 0))
          (should-not (file-exists-p (mind-state-log-file mind-state-test--time))))))))

(ert-deftest mind-state-never-opens-the-journal ()
  (should-not (get-file-buffer my/journal-file))
  (let ((src (with-temp-buffer
               (insert-file-contents (locate-library "mind-state.el"))
               (buffer-string))))
    (should-not (string-match-p "my/journal-file" src))
    (should-not (string-match-p "mind-state-options" src))
    (should-not (string-match-p "mind-state--ensure-daily-heading" src))
    (should-not (string-match-p "gtd-config" src))
    (should-not (string-match-p "journal--ensure-daily-heading" src))))

(ert-deftest mind-state-header-explains-the-design ()
  (let ((src (with-temp-buffer
               (insert-file-contents (locate-library "mind-state.el"))
               (buffer-string))))
    (should (string-match-p "^;;   \\*\\*\\*\\* HH:MM kind value \\[\\[id:TASK\\]\\[Title\\]\\] note :auto:$" src))
    (should (string-match-p "^;; .*start\\b.*break\\b.*check\\b" src))
    (should (string-match-p "^;; .*interrupt" src))
    (should (string-match-p "^;; .*absorbed.*steady.*stuck.*tense.*scattered.*restless.*fresh.*tired.*flat" src))
    (should (string-match-p "^;; .*deep" src))))

;;; mind-state-test.el ends here
