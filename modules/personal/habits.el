;;; habits.el --- Habit events, measurements and the day snapshot -*- lexical-binding: t; -*-

;; Design D18 (Brain notes-structure findings): habits are defined in
;; gtd/rituals.org, every habit event is one line in gtd/habits-log-<year>.org,
;; and the day snapshot is computed from those lines plus the clock.
;;
;;   **** HH:MM done|skip|note [[id:ID][Title]] [H:MM] [toggl:ID] [=value unit]
;;   notes for the event on the lines under it
;;
;; What lives here:
;;  - `my/habit--log-state-change' writes a done or skip line when a habit in
;;    rituals.org is marked done (any done keyword).  Duration and Toggl id
;;    come from the clock that `org-clock-out-when-done' just closed; that
;;    CLOCK line is then moved out of rituals.org, the event carries it.
;;  - `my/habit-skip' marks the habit done for the day without a done event.
;;  - `my/habit-note' appends the day's note event.
;;  - The day agenda is the snapshot: log mode is on by default and shows
;;    the habit events from the log like closed tasks.
;;  - `my/habits-log-merge-conflicts' folds Syncthing .sync-conflict-* copies
;;    of a log file back into it, line-identity union per day.

(require 'org)
(require 'org-clock)
(require 'org-element)
(require 'cl-lib)
(require 'subr-x)
(require 'paths)

;;;; Settings

(defvar my/habits-log-dir my/gtd-dir
  "Directory holding habits-log-<year>.org.")

(defvar my/habit-move-clock-to-log t
  "When non-nil, the CLOCK line of a finished habit leaves rituals.org.
The event line in the log carries its duration and Toggl id instead, so
rituals.org does not grow by one line per habit per day.")

(defvar my/habit-measure-files-function
  (lambda () (seq-filter #'file-regular-p (org-agenda-files)))
  "Function returning the files whose CLOCK lines feed tag measurements.")

;; The log holds the history now, so org need not add a
;; `- State "DONE" from "TODO"' line to rituals.org at every repeat.
(setq org-log-repeat nil)

(defconst my/habit-event-re
  (concat "^\\*\\*\\*\\* \\([0-9][0-9]:[0-9][0-9]\\) \\(done\\|skip\\|note\\)"
          "\\(?: \\[\\[id:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\]\\)?\\(.*\\)$")
  "Groups: 1 time, 2 kind, 3 habit id, 4 title, 5 rest of line.")

(defconst my/habit-day-re "^\\*\\*\\* \\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\)"
  "Day heading in the log; group 1 is the ISO date.")

;;;; Time helpers

(defun my/habit--day-start (time)
  "Midnight at the start of the day holding TIME."
  (let ((d (decode-time time)))
    (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d))))

(defun my/habit--iso (time) (format-time-string "%Y-%m-%d" time))

(defun my/habit--minutes-string (minutes)
  (format "%d:%02d" (/ minutes 60) (% minutes 60)))

(defun my/habit--parse-hhmm (s)
  "Minutes since midnight for S like \"09:30\"."
  (when (and s (string-match "\\`\\([0-9]+\\):\\([0-9][0-9]\\)\\'" s))
    (+ (* 60 (string-to-number (match-string 1 s)))
       (string-to-number (match-string 2 s)))))

;;;; The log file

(defun my/habits-log-file (&optional time)
  "Path of the log file for the year of TIME (default now)."
  (expand-file-name (format-time-string "habits-log-%Y.org" time)
                    my/habits-log-dir))

(defun my/habit--log-buffer (time)
  "Buffer of the log file for TIME, created with its header if missing."
  (let* ((file (my/habits-log-file time))
         (new (not (file-exists-p file)))
         (buf (find-file-noselect file)))
    (when new
      (with-current-buffer buf
        (insert (format-time-string "#+TITLE: Habits log %Y\n" time)
                "#+STARTUP: overview\n"
                "# One line per habit event: HH:MM done|skip|note [[id:HABIT][Title]] [duration] [toggl:ID] [=value unit]\n"
                "# Notes for an event go on the lines under it. Not an agenda file. See design D18.\n\n"
                (format-time-string "* %Y\n" time))
        (save-buffer)))
    buf))

(defun my/habit--goto-or-insert-heading (level prefix text bound)
  "Find heading of LEVEL whose title starts with PREFIX before BOUND, or insert TEXT.
Point ends on the heading line.  New headings go before the first sibling
that sorts after PREFIX, so the datetree stays in order."
  (let* ((stars (make-string level ?*))
         (re (concat "^" (regexp-quote stars) " \\(.*\\)$"))
         (found nil) (insert-at nil))
    (save-excursion
      (while (and (not found) (re-search-forward re bound t))
        (let ((title (match-string 1)))
          (cond ((string-prefix-p prefix title)
                 (setq found (line-beginning-position)))
                ((and (not insert-at) (string> title prefix))
                 (setq insert-at (line-beginning-position)))))))
    (cond (found (goto-char found))
          (t (goto-char (or insert-at bound))
             (unless (bolp) (insert "\n"))
             (insert stars " " text "\n")
             (forward-line -1)))))

(defun my/habit--goto-day (time)
  "In the current log buffer, go to the day heading for TIME, creating it.
Returns the end position of the day's subtree."
  (widen)
  (goto-char (point-min))
  (let* ((year (format-time-string "%Y" time))
         (month (format-time-string "%Y-%m" time))
         (day (my/habit--iso time)))
    (my/habit--goto-or-insert-heading 1 year year (point-max))
    (let ((year-end (save-excursion (org-end-of-subtree t t) (point))))
      (forward-line 1)
      (my/habit--goto-or-insert-heading
       2 month (format-time-string "%Y-%m %B" time) year-end))
    (let ((month-end (save-excursion (org-end-of-subtree t t) (point))))
      (forward-line 1)
      (my/habit--goto-or-insert-heading
       3 day (format-time-string "%Y-%m-%d %A" time) month-end))
    (save-excursion (org-end-of-subtree t t) (point))))

(defun my/habit--day-has-line-p (line day-end)
  "Non-nil if LINE already appears before DAY-END (from point)."
  (save-excursion
    (let ((case-fold-search nil))
      (re-search-forward (concat "^" (regexp-quote line) "$") day-end t))))

(defun my/habit--sort-day-events ()
  "Sort the event blocks of the day at point by their HH:MM.  Point on day heading."
  (let* ((end (save-excursion (org-end-of-subtree t t) (point-marker)))
         (start (progn (forward-line 1) (point)))
         (text (buffer-substring start end))
         (blocks nil) (cur nil))
    (dolist (l (split-string text "\n"))
      (if (string-match-p "\\`\\*\\*\\*\\* " l)
          (progn (when cur (push (nreverse cur) blocks)) (setq cur (list l)))
        (when cur (push l cur))))
    (when cur (push (nreverse cur) blocks))
    (setq blocks (nreverse blocks))
    (let ((sorted (cl-stable-sort
                   (mapcar (lambda (b)
                             (let ((lines (seq-remove #'string-empty-p b)))
                               (cons (if (string-match "\\`\\*\\*\\*\\* \\([0-9:]+\\)" (car b))
                                         (match-string 1 (car b))
                                       "99:99")
                                     lines)))
                           blocks)
                   #'string< :key #'car)))
      (delete-region start end)
      (goto-char start)
      (dolist (b sorted)
        (insert (string-join (cdr b) "\n") "\n"))
      (set-marker end nil))))

(defun my/habit-append-event (time kind &optional id title duration toggl-id value notes)
  "Append one event line to the log for TIME.
KIND is \"done\", \"skip\" or \"note\".  DURATION is minutes.  VALUE is a
string such as \"12.3 km\".  NOTES is a string placed on the lines below.
Returns the line, or nil when the identical line was already there."
  (let* ((line (concat (format "**** %s %s" (format-time-string "%H:%M" time) kind)
                       (when id (format " [[id:%s][%s]]" id (or title "")))
                       (when (and duration (> duration 0))
                         (concat " " (my/habit--minutes-string duration)))
                       (when toggl-id (format " toggl:%s" toggl-id))
                       (when (and value (not (string-empty-p value)))
                         (concat " =" value "="))))
         (buf (my/habit--log-buffer time)))
    (with-current-buffer buf
      (save-excursion
        (let ((day-end (my/habit--goto-day time)))
          (if (and (not (equal kind "note")) (my/habit--day-has-line-p line day-end))
              (progn (message "Habit log already has: %s" line) nil)
            (goto-char day-end)
            (unless (bolp) (insert "\n"))
            (insert line "\n")
            (when (and notes (not (string-empty-p (string-trim notes))))
              (insert (string-trim notes) "\n"))
            (save-buffer)
            line))))))

;;;; Reading the log

(defun my/habit--events-in-buffer (from to)
  "Events in the current log buffer with dates in [FROM, TO] (ISO strings).
Each is a plist (:date :time :kind :id :title :rest :notes)."
  (let ((events nil) (day nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\*\\*\\*? " nil t)
        (beginning-of-line)
        (cond
         ((looking-at my/habit-day-re)
          (setq day (match-string-no-properties 1))
          (forward-line 1))
         ((and day (not (string< day from)) (not (string< to day))
               (looking-at my/habit-event-re))
          (let ((ev (list :date day
                          :marker (copy-marker (line-beginning-position))
                          :time (match-string-no-properties 1)
                          :kind (match-string-no-properties 2)
                          :id (match-string-no-properties 3)
                          :title (match-string-no-properties 4)
                          :rest (string-trim (match-string-no-properties 5))))
                (notes nil))
            (forward-line 1)
            (while (and (not (eobp)) (not (looking-at "^\\*")))
              (push (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))
                    notes)
              (forward-line 1))
            (push (plist-put ev :notes (string-join (nreverse notes) "\n")) events)))
         (t (forward-line 1)))))
    (nreverse events)))

(defun my/habit-events-between (from-time to-time)
  "Events in the log between FROM-TIME and TO-TIME (inclusive days)."
  (let* ((from (my/habit--iso from-time))
         (to (my/habit--iso to-time))
         (years (number-sequence (string-to-number (substring from 0 4))
                                 (string-to-number (substring to 0 4))))
         (events nil))
    (dolist (y years)
      (let ((file (expand-file-name (format "habits-log-%d.org" y) my/habits-log-dir)))
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (setq events (append events (my/habit--events-in-buffer from to)))))))
    events))

(defun my/habit--event-duration (ev)
  "Minutes recorded on EV's line, or nil."
  (let ((rest (plist-get ev :rest)))
    (when (string-match "\\(?:\\`\\| \\)\\([0-9]+\\):\\([0-9][0-9]\\)\\(?: \\|\\'\\)" rest)
      (+ (* 60 (string-to-number (match-string 1 rest)))
         (string-to-number (match-string 2 rest))))))

;;;; Definitions in rituals.org

(defun my/habit--definitions ()
  "Habits defined in `my/rituals-file', newest file contents, as plists."
  (with-current-buffer (find-file-noselect my/rituals-file)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (list :id (org-entry-get nil "ID")
              :title (org-get-heading t t t t)
              :state (org-get-todo-state)
              :role (org-entry-get nil "HABIT_ROLE")
              :track (org-entry-get nil "HABIT_TRACK")
              :window (org-entry-get nil "HABIT_WINDOW")
              :freq (org-entry-get nil "HABIT_FREQ")
              :days (org-entry-get nil "HABIT_DAYS")
              :end (org-entry-get nil "HABIT_END")))
      "STYLE=\"habit\"" 'file))))

(defun my/habit--measures ()
  "Measurement rows under \"* Measurements\" in `my/rituals-file'."
  (with-current-buffer (find-file-noselect my/rituals-file)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (list :title (org-get-heading t t t t)
              :measure (org-entry-get nil "MEASURE")
              :floor (org-entry-get nil "FLOOR")))
      "MEASURE={.}" 'file))))

(defun my/habit--active-p (def date-iso)
  "Non-nil if DEF was not retired on or before DATE-ISO."
  (let ((end (plist-get def :end)))
    (or (null end)
        (and (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]" end)
             (string< date-iso (match-string 0 end))))))

(defun my/habit--due-p (def time)
  "Non-nil when DEF is expected on the day of TIME.
Daily habits are due every day, habits with HABIT_DAYS on those weekdays.
Weekly and monthly habits without fixed days return `window'."
  (let* ((days (plist-get def :days))
         (freq (or (plist-get def :freq) "1/1"))
         (period (if (string-match "/\\([0-9]+\\)" freq)
                     (string-to-number (match-string 1 freq))
                   1)))
    (cond (days (and (member (format-time-string "%a" time) (split-string days)) t))
          ((= period 1) t)
          (t 'window))))

(defun my/habit--period-range (def time)
  "Start and end times of DEF's current period around TIME: week or month."
  (let* ((freq (or (plist-get def :freq) "1/1"))
         (period (if (string-match "/\\([0-9]+\\)" freq)
                     (string-to-number (match-string 1 freq))
                   1))
         (d (decode-time time)))
    (cond ((>= period 28)
           (cons (encode-time 0 0 0 1 (nth 4 d) (nth 5 d))
                 (time-subtract (encode-time 0 0 0 1 (1+ (nth 4 d)) (nth 5 d)) 86400)))
          (t (let* ((dow (nth 6 d))                ; 0 Sunday
                    (back (mod (+ dow 6) 7))        ; days since Monday
                    (mon (time-subtract (my/habit--day-start time) (* back 86400))))
               (cons mon (time-add mon (* 6 86400))))))))

;;;; Capturing events from rituals.org

(defvar my/habit--skipping nil
  "Dynamically bound to t or a note string while `my/habit-skip' marks a habit.")

(defvar my/habit--last-toggl-id nil
  "Id of the Toggl entry stopped by the most recent clock-out.")

(defun my/habit--remember-toggl-id ()
  "Keep the Toggl entry id before `org-toggl-clock-out' clears it."
  (setq my/habit--last-toggl-id
        (and (boundp 'toggl-current-time-entry)
             (alist-get 'id toggl-current-time-entry))))

(add-hook 'org-clock-out-hook #'my/habit--remember-toggl-id -90)

(defun my/habit--in-rituals-p ()
  (and buffer-file-name
       (file-equal-p buffer-file-name my/rituals-file)))

(defun my/habit--take-recent-clock ()
  "Duration in minutes of a CLOCK line in this entry closed within 3 minutes.
With `my/habit-move-clock-to-log', that line is removed.  Returns nil when
there is no such clock."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          (now (float-time)) (found nil))
      (while (and (not found)
                  (re-search-forward org-clock-line-re end t))
        (let ((el (org-element-at-point)))
          (when (and (eq (org-element-type el) 'clock)
                     (eq (org-element-property :status el) 'closed))
            (let* ((ts (org-element-property :value el))
                   (end-time (org-timestamp-to-time ts t))
                   (dur (org-element-property :duration el)))
              (when (and end-time dur
                         (< (abs (- now (float-time end-time))) 180))
                (setq found (my/habit--parse-hhmm dur))
                (when my/habit-move-clock-to-log
                  (let ((b (line-beginning-position))
                        (e (progn (forward-line 1) (point))))
                    (delete-region b e))))))))
      found)))

(defun my/habit--log-state-change ()
  "On a done keyword for a habit in rituals.org, append its event line."
  (when (and (my/habit--in-rituals-p)
             (member org-state org-done-keywords)
             (equal (org-entry-get nil "STYLE") "habit"))
    (let* ((id (org-entry-get nil "ID"))
           (title (org-get-heading t t t t))
           (skip my/habit--skipping)
           (duration (unless skip (my/habit--take-recent-clock)))
           (toggl (and duration my/habit--last-toggl-id))
           (line (my/habit-append-event (current-time)
                                        (if skip "skip" "done")
                                        id title duration toggl nil
                                        (and (stringp skip) skip))))
      (setq my/habit--last-toggl-id nil)
      (when line (message "Habit log: %s" line)))))

(add-hook 'org-after-todo-state-change-hook #'my/habit--log-state-change 50)

(defun my/habit-skip (&optional note)
  "Mark the habit at point skipped today: a skip event, the repeater advances.
Works in an org buffer and in the agenda.  NOTE goes under the event line."
  (interactive (list (read-string "Skip, because (optional): ")))
  (let ((my/habit--skipping (if (and note (not (string-empty-p note))) note t)))
    (if (derived-mode-p 'org-agenda-mode)
        (org-agenda-todo "DONE")
      (org-todo "DONE"))))

(defun my/habit-note (text)
  "Append the day's note event to the log with TEXT under it."
  (interactive (list (read-string "What did today want that it did not get? ")))
  (my/habit-append-event (current-time) "note" nil nil nil nil nil text)
  (message "Day note logged."))

;;;; Measurements from CLOCK lines

(defun my/habit--clock-intervals (file date-iso)
  "Closed CLOCK intervals in FILE that start on DATE-ISO.
Each is (minutes mode heading)."
  (when (file-exists-p file)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((res nil)
             (re (concat "^[ \t]*CLOCK: \\[" (regexp-quote date-iso)
                         "[^]]*\\]--\\[[^]]*\\] => +\\([0-9]+\\):\\([0-9][0-9]\\)")))
         (while (re-search-forward re nil t)
           (let ((mins (+ (* 60 (string-to-number (match-string 1)))
                          (string-to-number (match-string 2))))
                 (mode (save-excursion
                         (forward-line 1)
                         (when (looking-at "^[ \t]*- mode: \\(.*\\)$")
                           (string-trim (match-string 1)))))
                 (heading (save-excursion
                            (ignore-errors (org-back-to-heading t)
                                           (org-get-heading t t t t)))))
             (push (list mins mode heading) res)))
         (nreverse res))))))

(defun my/habit--measure (row date-iso)
  "Evaluate measurement ROW for DATE-ISO: (sessions minutes details)."
  (let* ((spec (or (plist-get row :measure) ""))
         (files (cond ((string-match "file=\\(\\S-+\\)" spec)
                       (list (expand-file-name (match-string 1 spec) my/brain-dir)))
                      (t (funcall my/habit-measure-files-function))))
         (tag (and (string-match "tag=\\(\\S-+\\)" spec) (match-string 1 spec)))
         (intervals (apply #'append
                           (mapcar (lambda (f) (my/habit--clock-intervals f date-iso))
                                   files))))
    (when tag
      (setq intervals (seq-filter (lambda (i) (equal (nth 1 i) tag)) intervals)))
    (list (length intervals)
          (apply #'+ (mapcar #'car intervals))
          intervals)))

;;;; The day in the agenda

;; The day agenda (`C-c a a') is the snapshot.  Log mode is on by default,
;; so clocked and closed tasks show on the time grid, and so do habit events:
;; log mode only knows CLOSED, CLOCK and State lines in agenda files, and a
;; finished habit leaves none in rituals.org (the repeater drops CLOSED,
;; `org-log-repeat' is nil, the CLOCK line moved to the log).  So when log
;; mode scans rituals.org, the day's lines are read from the habits log and
;; handed back as log items.  RET on one jumps to the line in the log.

(setq org-agenda-start-with-log-mode '(closed clock state))

(defun my/habit--agenda-log-items (date)
  "Agenda log items for the habit events on DATE, a gregorian (M D Y) list."
  (let* ((time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
         (items nil))
    (dolist (ev (my/habit-events-between time time))
      (let* ((marker (plist-get ev :marker))
             (kind (plist-get ev :kind))
             (title (or (plist-get ev :title) "Day note"))
             (dur (my/habit--event-duration ev))
             (extra (pcase kind
                      ("done" (if dur (format "Done:      (%s) " (my/habit--minutes-string dur))
                                "Done:       "))
                      ("skip" "Skipped:    ")
                      (_ "Note:       ")))
             ;; A full <timestamp> puts the item on the time grid at HH:MM.
             (stamp (format-time-string (concat "<%Y-%m-%d %a " (plist-get ev :time) ">") time))
             (txt (org-agenda-format-item extra title 1 "rituals" nil stamp)))
        (push (org-add-props txt nil
                'org-marker marker 'org-hd-marker marker
                'face (if (equal kind "skip") 'org-agenda-dimmed-todo-face 'org-agenda-done)
                'type "closed" 'date date 'priority 100000 'level 1
                'undone-face 'org-warning 'done-face 'org-agenda-done)
              items)))
    (nreverse items)))

(defun my/habit--agenda-progress-advice (orig &rest args)
  "Append the log's habit events when log mode scans rituals.org."
  (let ((items (apply orig args)))
    (if (and (my/habit--in-rituals-p) (boundp 'date) (listp date))
        (append items (my/habit--agenda-log-items date))
      items)))

(advice-add 'org-agenda-get-progress :around #'my/habit--agenda-progress-advice)

(defun my/habit--status (def events time)
  "Status of DEF on the day of TIME given that day's EVENTS:
done, skip, pending, missed, or nil when not due that day."
  (let* ((id (plist-get def :id))
         (mine (seq-filter (lambda (e) (equal (plist-get e :id) id)) events))
         (due (my/habit--due-p def time))
         (today-p (equal (my/habit--iso time) (my/habit--iso (current-time))))
         (past-p (string< (my/habit--iso time) (my/habit--iso (current-time))))
         (win-end (and (plist-get def :window)
                       (string-match "-\\([0-9:]+\\)" (plist-get def :window))
                       (my/habit--parse-hhmm (match-string 1 (plist-get def :window)))))
         (now-min (let ((d (decode-time))) (+ (* 60 (nth 2 d)) (nth 1 d)))))
    (cond
     ((seq-find (lambda (e) (equal (plist-get e :kind) "done")) mine) 'done)
     ((seq-find (lambda (e) (equal (plist-get e :kind) "skip")) mine) 'skip)
     ((not (eq due t)) nil)
     (past-p 'missed)
     ((and today-p win-end (> win-end 180) (> now-min win-end)) 'missed)
     (t 'pending))))

;;;; Syncthing conflict copies

(defun my/habits-log-merge-conflicts ()
  "Fold every habits-log-*.sync-conflict-*.org into its log file.
Events are a union per day on exact line identity; the days touched are
re-sorted by time.  Merged conflict copies move to Archives/sync-conflicts/."
  (interactive)
  (let ((copies (directory-files my/habits-log-dir t
                                 "\\`habits-log-[0-9]\\{4\\}\\.sync-conflict-.*\\.org\\'"))
        (merged 0))
    (dolist (copy copies)
      (let* ((year (and (string-match "habits-log-\\([0-9]\\{4\\}\\)" copy)
                        (match-string 1 copy)))
             (main (expand-file-name (format "habits-log-%s.org" year) my/habits-log-dir))
             (events (with-current-buffer (find-file-noselect copy)
                       (my/habit--events-in-buffer "0000-00-00" "9999-99-99")))
             (days nil))
        (with-current-buffer (find-file-noselect main)
          (dolist (ev events)
            (let* ((time (org-time-string-to-time
                          (format "%s %s" (plist-get ev :date) (plist-get ev :time))))
                   (day-end (my/habit--goto-day time))
                   (line (concat (format "**** %s %s" (plist-get ev :time) (plist-get ev :kind))
                                 (when (plist-get ev :id)
                                   (format " [[id:%s][%s]]" (plist-get ev :id) (plist-get ev :title)))
                                 (if (string-empty-p (plist-get ev :rest)) ""
                                   (concat " " (plist-get ev :rest))))))
              (unless (my/habit--day-has-line-p line day-end)
                (goto-char day-end)
                (unless (bolp) (insert "\n"))
                (insert line "\n")
                (unless (string-empty-p (plist-get ev :notes))
                  (insert (plist-get ev :notes) "\n"))
                (cl-incf merged)
                (cl-pushnew (plist-get ev :date) days :test #'equal))))
          (dolist (d days)
            (goto-char (point-min))
            (when (re-search-forward (concat "^\\*\\*\\* " (regexp-quote d)) nil t)
              (beginning-of-line)
              (my/habit--sort-day-events)))
          (save-buffer))
        (let ((dest-dir (expand-file-name "sync-conflicts/" my/archive-dir)))
          (make-directory dest-dir t)
          (rename-file copy (expand-file-name (file-name-nondirectory copy) dest-dir) t))))
    (message "Merged %d event(s) from %d conflict cop%s"
             merged (length copies) (if (= (length copies) 1) "y" "ies"))))

;;;; Keys in the agenda (C-c t keys are in keymaps.el)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "K" #'my/habit-skip))

(provide 'habits)
;;; habits.el ends here
