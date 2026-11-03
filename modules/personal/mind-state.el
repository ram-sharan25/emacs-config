;;; mind-state.el --- Attention states and interruptions, one line per event -*- lexical-binding: t; -*-

;; A private log of attention states and interruptions.  Every reading, by
;; hand or from the random prompt, is one line in
;; Trackers/mind-state-log-<year>.org (git-crypt, not an agenda or roam
;; file) under the year/month/day tree that habits.el's `my/habit--goto-day'
;; builds, so the file reads like the habits log:
;;
;;   **** HH:MM kind value [[id:TASK][Title]] note :auto:
;;
;; kind is written by the code: start is the first mind-state line logged in
;; this clock-in, break a later mind-state line, check not clocked in or the
;; random prompt, and interrupt a dedicated interruption line.  VALUE is the
;; state for start/break/check and the cause for interrupt.  Random check and
;; interruption lines never decide start versus break
;; (`mind-state--logged-since-p' looks only at start and break lines).
;;
;; state is one of nine words:
;;   absorbed steady stuck tense scattered restless fresh tired flat
;; They answer one question, "what has my attention right now, and how much
;; do I have to give the work?": direction (on the work and moving, on it
;; but stuck, pulled inward, pulled outward, on nothing and restless, on
;; nothing and at rest) and capacity (full or low).  Bodily states go in the
;; note; the word is the attention state they produced.
;;
;; The link is the clocked task's existing :ID: with its heading as the
;; description.  No ID is ever created; a task without one is logged without
;; a link, and nothing is ever written into a task file.
;;
;; The random prompt keeps its schedule (target per day, active hours,
;; interval bounds, idle guard) but is silent while the clock runs in a task
;; tagged `mind-state-deep-tag' (default "deep"), own or inherited; the slot
;; is simply rescheduled.  Its lines end with :auto: and count towards the
;; daily target; manual lines do neither.
;;
;; How to log state: C-c t m l (`mind-state-manual-log'), pick the word, RET,
;; type the note or nothing, RET.  Each choice shows its meaning in brackets,
;; "scattered (pulled outward)"; the bracket is display only and the file gets
;; the bare word.  How to log an interruption: C-c t x
;; (`mind-state-log-interruption'), pick slack, coworker, phone, pitstop, walk
;; or other, then add an optional note; other requires a note.  The timestamp is
;; the interruption's start by default; C-u C-c t x logs after returning and
;; adds :after:.  Both commands use the clocked task's existing link when
;; available.
;;
;; Not here: no report or count (the raw file is read by hand first), no
;; journal entry, no ID creation, no new key, no prompt on clock-in or
;; clock-out.  Plan and decisions:
;; ~/Stillness/Agent_Stuffs/Plans/Brain/mind-state-log/.

;;; Code:

(require 'paths)
(require 'cl-lib)
(require 'habits)   ; `my/habit--goto-day' builds the year/month/day tree

;;; --- Configuration ---

(defvar mind-state-log-dir my/trackers-dir
  "Directory holding mind-state-log-<year>.org.")

(defvar mind-state-deep-tag "deep"
  "Tag that marks a deep sitting, own or inherited.
The random prompt is silent while the clock runs in a task carrying it.")

(defconst mind-state-words
  '("absorbed" "steady" "stuck" "tense" "scattered" "restless" "fresh" "tired" "flat")
  "The closed vocabulary: one lowercase word per line.  See the comment block.")

(defconst mind-state-word-meanings
  '(("absorbed"  . "on the work, carried by it")
    ("steady"    . "on the work, by effort")
    ("stuck"     . "on the work, nothing moves")
    ("tense"     . "pulled inward")
    ("scattered" . "pulled outward")
    ("restless"  . "nothing pulls, keep reaching")
    ("fresh"     . "at rest, capacity full")
    ("tired"     . "capacity low")
    ("flat"      . "capacity there, no want"))
  "Short meaning per word, shown in brackets as a completion annotation.")

(defconst mind-state-interruption-causes
  '("slack" "coworker" "phone" "pitstop" "walk" "other")
  "Closed vocabulary used by `mind-state-log-interruption'.")

(defconst mind-state-event-re
  (concat "^\\*\\*\\*\\* \\([0-9][0-9]:[0-9][0-9]\\) \\(start\\|break\\|check\\|interrupt\\) \\([a-z]+\\)"
          "\\(?: \\[\\[id:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\]\\)?\\(.*\\)$")
  "One event line.  Groups: 1 time, 2 kind, 3 state, 4 task id, 5 title, 6 rest.")

;;; --- The log ---

(defun mind-state--format-line (time kind word &optional id title note auto)
  "One event line for TIME: \"**** HH:MM KIND WORD [[id:ID][TITLE]] NOTE :auto:\".
The link is present when ID is non-nil, NOTE when it has non-blank text, and
\":auto:\" when AUTO is non-nil.  No trailing newline."
  (let ((note (and note (string-trim note))))
    (concat (format "**** %s %s %s" (format-time-string "%H:%M" time) kind word)
            (when id (format " [[id:%s][%s]]" id (or title "")))
            (when (and note (not (string-empty-p note))) (concat " " note))
            (when auto " :auto:"))))

(defun mind-state-log-file (&optional time)
  "Path of the mind-state log for the year of TIME (default now)."
  (expand-file-name (format-time-string "mind-state-log-%Y.org" time)
                    mind-state-log-dir))

(defun mind-state--log-buffer (time)
  "Buffer of the log file for TIME, created with its header when missing."
  (let* ((file (mind-state-log-file time))
         (new (not (file-exists-p file)))
         (buf (find-file-noselect file)))
    (when new
      (with-current-buffer buf
        (insert (format-time-string "#+TITLE: Mind-state log %Y\n" time)
                "#+STARTUP: overview\n"
                "# One line per event: HH:MM kind value [[id:TASK][Title]] note :auto:\n"
                "# kind: start = first state in this clock-in; break = later state; check = not clocked in or random; interrupt = interruption cause.\n"
                "# state: absorbed steady stuck tense scattered restless fresh tired flat.  interruption: slack coworker phone pitstop walk other.\n"
                "# :auto: marks the random prompt's lines.\n"
                "# Not an agenda file, not a roam file.  Design: modules/personal/mind-state.el in ~/.emacs.d.\n\n"
                (format-time-string "* %Y\n" time))
        (save-buffer)))
    buf))

(defun mind-state--append-line (time line)
  "Append LINE under the day heading for TIME in the log, creating the
year, month and day headings as needed.  Saves the buffer.  Returns LINE."
  (with-current-buffer (mind-state--log-buffer time)
    (save-excursion
      (let ((day-end (my/habit--goto-day time)))
        (goto-char day-end)
        (unless (bolp) (insert "\n"))
        (insert line "\n")))
    (save-buffer)
    line))

;;; --- Input ---

(defun mind-state--annotate (word)
  "Completion annotation for WORD: its meaning in brackets, or nil."
  (let ((meaning (cdr (assoc word mind-state-word-meanings))))
    (and meaning (format " (%s)" meaning))))

(defun mind-state--read-event ()
  "Ask for the state word, then for an optional note; return (WORD . NOTE).
The first prompt completes over the nine words, each shown with its meaning
in brackets, and accepts nothing else; RET on a word moves to the note
prompt, RET there closes.  NOTE is trimmed and may be \"\"."
  (let ((completion-extra-properties '(:annotation-function mind-state--annotate))
        (word nil))
    (while (not (member word mind-state-words))
      (setq word (completing-read "Mind-state: " mind-state-words nil t)))
    (cons word (string-trim (read-string "Note (optional): ")))))

(defun mind-state--read-interruption ()
  "Ask for an interruption cause and note; return (CAUSE . NOTE).
The cause must be in `mind-state-interruption-causes'.  A note is optional
except for `other', which keeps prompting until the cause is described."
  (let* ((cause (completing-read "Interruption: "
                                 mind-state-interruption-causes nil t))
         (prompt (if (equal cause "other")
                     "What interrupted you: "
                   "Note (optional): "))
         (note (string-trim (read-string prompt))))
    (while (and (equal cause "other") (string-empty-p note))
      (setq note (string-trim (read-string "Describe the interruption: "))))
    (cons cause note)))

;;; --- The clock ---

(defun mind-state--clocked-deep-p ()
  "Non-nil when the Org clock runs in a task tagged `mind-state-deep-tag'.
Own and inherited tags both count.  Reads only; never changes the task."
  (and (org-clocking-p)
       (marker-buffer org-clock-hd-marker)
       (org-with-point-at org-clock-hd-marker
         (and (member mind-state-deep-tag (org-get-tags)) t))))

(defun mind-state--clock-context ()
  "The clocked task as (:id ID :title TITLE :start TIME), or nil.
Nil when no clock runs.  ID is the task's existing :ID: property or nil;
nothing is ever created."
  (when (and (org-clocking-p) (marker-buffer org-clock-hd-marker))
    (org-with-point-at org-clock-hd-marker
      (list :id (org-entry-get nil "ID")
            :title (org-get-heading t t t t)
            :start org-clock-start-time))))

(defun mind-state--now ()
  "The current time.  Tests replace this."
  (current-time))

(defun mind-state--lines-for-day (time)
  "Event lines under the day heading for TIME, oldest first, or nil.
Nil when the log file or the day heading does not exist.  Reads only."
  (let ((file (mind-state-log-file time)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward
                (concat "^\\*\\*\\* " (format-time-string "%Y-%m-%d" time) "\\( \\|$\\)")
                nil t)
           (let ((end (save-excursion (org-end-of-subtree t t) (point)))
                 (lines nil))
             (while (re-search-forward mind-state-event-re end t)
               (push (match-string-no-properties 0) lines))
             (nreverse lines))))))))

(defun mind-state--logged-since-p (id since now)
  "Non-nil when a start or break line for ID was logged between SINCE and NOW.
On SINCE's day a line counts when its HH:MM is at or after SINCE's; on NOW's
day, when different, every line counts.  ID nil matches lines without a link."
  (let ((since-day (format-time-string "%Y-%m-%d" since))
        (since-hhmm (format-time-string "%H:%M" since))
        (now-day (format-time-string "%Y-%m-%d" now))
        (found nil))
    (dolist (day-time (if (equal since-day now-day) (list since) (list since now)))
      (let ((same-day (equal (format-time-string "%Y-%m-%d" day-time) since-day)))
        (dolist (line (mind-state--lines-for-day day-time))
          (when (and (string-match mind-state-event-re line)
                     (member (match-string 2 line) '("start" "break"))
                     (equal (match-string 4 line) id)
                     (or (not same-day)
                         (not (string< (match-string 1 line) since-hhmm))))
            (setq found t)))))
    found))

(defun mind-state--kind (context auto now)
  "The kind word for a new line: \"check\", \"start\" or \"break\".
\"check\" for the random prompt (AUTO) or when CONTEXT is nil; otherwise
\"break\" when a start or break line for the clocked task exists since the
clock started, else \"start\"."
  (cond ((or auto (null context)) "check")
        ((mind-state--logged-since-p (plist-get context :id)
                                     (plist-get context :start) now)
         "break")
        (t "start")))

(defvar mind-state-target-prompts 10
  "Target number of mind-state prompts per day.")

(defvar mind-state-active-hours '(8 22)
  "Hours between which mind-state prompts are active (24-hour format).")

(defvar mind-state-min-interval 30
  "Minimum minutes between mind-state prompts.")

(defvar mind-state-max-interval 180
  "Maximum minutes between mind-state prompts.")

(defvar mind-state-idle-threshold 300
  "Minimum seconds of Emacs activity required before showing prompt.")

;;; --- Internal State ---

(defvar mind-state-timer nil)
(defvar mind-state-prompts-today 0)
(defvar mind-state-today-date nil)

;;; --- Internal Helpers ---

(defun mind-state--current-date-string ()
  "Return today's date as YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d"))

(defun mind-state--reset-daily-counter ()
  "Reset prompt counter if the date has changed."
  (let ((today (mind-state--current-date-string)))
    (unless (string= today mind-state-today-date)
      (setq mind-state-today-date today
            mind-state-prompts-today 0))))

(defun mind-state--within-active-hours-p ()
  "Return t if current time is within active hours."
  (let ((hour (string-to-number (format-time-string "%H")))
        (start (car mind-state-active-hours))
        (end (cadr mind-state-active-hours)))
    (and (>= hour start) (< hour end))))

(defun mind-state--user-is-active-p ()
  "Return t if user has been active recently (not idle)."
  (let ((idle (current-idle-time)))
    (if idle
        (< (float-time idle) mind-state-idle-threshold)
      t)))

(defun mind-state--should-prompt-p ()
  "Return t if conditions are met to show a prompt.
Active hours, a recently active user, the daily target not yet reached, and
the clock is not in a deep sitting."
  (and (mind-state--within-active-hours-p)
       (mind-state--user-is-active-p)
       (< mind-state-prompts-today mind-state-target-prompts)
       (not (mind-state--clocked-deep-p))))

(defun mind-state--calculate-next-interval ()
  "Calculate random interval in seconds until next prompt."
  (let* ((remaining (max 1 (- mind-state-target-prompts mind-state-prompts-today)))
         (hours-left (max 1 (- (cadr mind-state-active-hours)
                               (string-to-number (format-time-string "%H")))))
         (avg (/ (* hours-left 60) remaining))
         (lo (max mind-state-min-interval (/ avg 2)))
         (hi (max lo (min mind-state-max-interval (* avg 2)))))
    (* (+ lo (random (1+ (- hi lo)))) 60)))

;;; --- Core Prompt ---

(defun mind-state--log-event (word note auto)
  "Append one event line for WORD and NOTE now and return it.
AUTO non-nil marks a line the random prompt produced (kind \"check\", tag
\":auto:\").  The link points at the clocked task's existing :ID:, if any."
  (let* ((time (mind-state--now))
         (ctx (mind-state--clock-context))
         (kind (mind-state--kind ctx auto time))
         (line (mind-state--format-line time kind word
                                        (plist-get ctx :id) (plist-get ctx :title)
                                        note auto)))
    (mind-state--append-line time line)
    (message "Mind-state: %s" line)
    line))

(defun mind-state--log-interruption (cause note after)
  "Append an interrupt line for CAUSE and NOTE now and return it.
When AFTER is non-nil, append :after: to mark a retrospective log."
  (let* ((time (mind-state--now))
         (ctx (mind-state--clock-context))
         (line (concat (mind-state--format-line
                        time "interrupt" cause
                        (plist-get ctx :id) (plist-get ctx :title)
                        note nil)
                       (when after " :after:"))))
    (mind-state--append-line time line)
    (message "Interruption: %s" line)
    line))

(defun mind-state--prompt-user ()
  "Ask for the current state when the guards allow and log it with :auto:.
Always schedules the next prompt afterwards, also when the user quits."
  (mind-state--reset-daily-counter)
  (unwind-protect
      (when (mind-state--should-prompt-p)
        (pcase (mind-state--read-event)
          (`(,word . ,note)
           (mind-state--log-event word note t)
           (setq mind-state-prompts-today (1+ mind-state-prompts-today)))))
    (mind-state--schedule-next-prompt)))

;;; --- Timer Management ---

(defun mind-state--schedule-next-prompt ()
  "Schedule the next mind-state prompt.
Once today's target is reached, wake at the start of tomorrow's active
hours instead, so a long-running Emacs keeps prompting on later days."
  (mind-state--reset-daily-counter)
  (when mind-state-timer
    (cancel-timer mind-state-timer))
  (if (< mind-state-prompts-today mind-state-target-prompts)
      (let ((interval (mind-state--calculate-next-interval)))
        (setq mind-state-timer
              (run-at-time interval nil #'mind-state--prompt-user))
        (message "Next mind-state check in ~%d minutes" (/ interval 60)))
    (let ((now (decode-time)))
      (setq mind-state-timer
            (run-at-time (encode-time
                          (list 0 0 (car mind-state-active-hours)
                                (1+ (decoded-time-day now))
                                (decoded-time-month now)
                                (decoded-time-year now)
                                nil -1 nil))
                         nil #'mind-state--prompt-user)))))

;;; --- Public Commands ---

(defun mind-state-start-tracking ()
  "Start mind-state tracking for today."
  (interactive)
  (mind-state--reset-daily-counter)
  (mind-state--schedule-next-prompt)
  (message "Mind-state tracking started. Target: %d prompts today."
           mind-state-target-prompts))

(defun mind-state-stop-tracking ()
  "Stop mind-state tracking."
  (interactive)
  (when mind-state-timer
    (cancel-timer mind-state-timer)
    (setq mind-state-timer nil))
  (message "Mind-state tracking stopped."))

(defun mind-state-manual-log ()
  "Log one mind-state line by hand.
While clocked in it is the sitting's start line or a break line; otherwise a
check line.  Does not count towards the day's random-prompt target."
  (interactive)
  (pcase (mind-state--read-event)
    (`(,word . ,note) (mind-state--log-event word note nil))))

(defun mind-state-log-interruption (after)
  "Log an interruption with the current task link.
With prefix argument AFTER, mark the line :after: because it was logged on
return; otherwise its timestamp is assumed to be the interruption's start."
  (interactive "P")
  (pcase (mind-state--read-interruption)
    (`(,cause . ,note) (mind-state--log-interruption cause note after))))

(defun mind-state-status ()
  "Show current mind-state tracking status."
  (interactive)
  (mind-state--reset-daily-counter)
  (message "Tracking: %s | Today: %d/%d | Active hours: %02d:00-%02d:00"
           (if mind-state-timer "ACTIVE" "INACTIVE")
           mind-state-prompts-today mind-state-target-prompts
           (car mind-state-active-hours) (cadr mind-state-active-hours)))

(defun mind-state-configure ()
  "Interactively configure mind-state tracking settings."
  (interactive)
  (setq mind-state-target-prompts
        (read-number "Target prompts per day: " mind-state-target-prompts))
  (setq mind-state-active-hours
        (list (read-number "Start hour (24-hour): " (car mind-state-active-hours))
              (read-number "End hour (24-hour): " (cadr mind-state-active-hours))))
  (setq mind-state-min-interval
        (read-number "Minimum interval (minutes): " mind-state-min-interval))
  (setq mind-state-max-interval
        (read-number "Maximum interval (minutes): " mind-state-max-interval))
  (message "Configuration updated. Restart tracking for changes to take effect."))

;;; --- Auto-start ---

(defun mind-state--auto-start ()
  "Auto-start tracking if within active hours."
  (when (mind-state--within-active-hours-p)
    (mind-state-start-tracking)))

(add-hook 'emacs-startup-hook #'mind-state--auto-start)

;;; Keybindings
;; C-c t m s / q / l / ? / c and C-c t x: keymaps.el.

(provide 'mind-state)
;;; mind-state.el ends here
