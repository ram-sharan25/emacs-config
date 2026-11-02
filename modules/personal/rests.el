;;; rests.el --- Rest blocks placed on the day's time block -*- lexical-binding: t; -*-

;; A rest is a block of the day placed on purpose, not a habit and not a
;; reward after a sitting.  Each one is a TODO in gtd/rests.org scheduled
;; to a time range, so it shows on the timeblock grid, on the day agenda
;; and in the phone app's agenda as a task for that time, and is ticked
;; DONE wherever it was taken, laptop or phone.  No repeater: each rest is
;; one day's.  Clocking it in (i on the grid) starts its TIMER_MINUTES
;; countdown, the same way Grounding Break ran.  Rests are placed fresh
;; each evening in Shutdown's time block for tomorrow, not from defaults.
;;
;; `my/rest-block' asks nothing: it adds one rest for the day shown (today
;; elsewhere), scheduled to the date only.  The time comes later with s on
;; the timeblock list and the length with d, so a rest is placed first and
;; fitted once the sittings are in.  Rests dated before today, done or not,
;; are dropped at each placement, since the habits log does not track them.
;; Each rest carries an :ID:, the key the phone sync uses for state changes.
;; R in the timeblock grid and list, C-c t r anywhere.

;;; Code:

(require 'org)
(require 'org-id)
(require 'paths)

(declare-function org-timeblock-list-get-current-date "org-timeblock")
(declare-function org-timeblock-get-dates "org-timeblock")
(declare-function org-timeblock-redraw-buffers "org-timeblock")
(defvar org-timeblock-column)
(defvar org-timeblock-daterange)

(defvar my/rest-default-minutes 20
  "Timer length for a rest whose SCHEDULED time has no end, in minutes.
A rest with a range gets its timer from the range on clock-in.")

(defun my/rest--view-date ()
  "The day shown at point in a timeblock view, as a decoded time, else nil."
  (cond ((derived-mode-p 'org-timeblock-mode)
         (nth (1- org-timeblock-column)
              (org-timeblock-get-dates (car org-timeblock-daterange)
                                       (cdr org-timeblock-daterange))))
        ((derived-mode-p 'org-timeblock-list-mode)
         (org-timeblock-list-get-current-date))))

(defun my/rest--timestamp (day)
  "Active Org date stamp for the day of time DAY, no time of day."
  (format-time-string "<%Y-%m-%d %a>" day))

(defun my/rest--prune (today)
  "Delete rest headings in the current buffer dated before TODAY (YYYY-MM-DD)."
  (org-with-wide-buffer
   (goto-char (point-max))
   (while (re-search-backward org-heading-regexp nil t)
     (let ((end (save-excursion (org-end-of-subtree t t) (point))))
       (when (and (= (org-outline-level) 1)
                  (save-excursion
                    (re-search-forward org-ts-regexp end t))
                  (string< (match-string 1) today))
         (delete-region (point) end))))))

(defun my/rest--insert (day)
  "Append a rest heading for the day of time DAY to `my/rests-file'."
  (with-current-buffer (find-file-noselect my/rests-file)
    (my/rest--prune (format-time-string "%Y-%m-%d"))
    (org-with-wide-buffer
     (goto-char (point-max))
     (unless (bolp) (insert "\n"))
     ;; The ID is what the phone sync keys state changes on.
     (insert (format "* TODO Rest\nSCHEDULED: %s\n:PROPERTIES:\n:ID: %s\n:TIMER_MINUTES: %d\n:END:\n"
                     (my/rest--timestamp day) (org-id-new) my/rest-default-minutes)))
    (save-buffer)))

(defun my/rest-block ()
  "Add one rest for the day shown in a timeblock view, else for today.
No prompt: the rest is a TODO scheduled to the date in `my/rests-file'.
Give it a time with s on the timeblock list and a length with d."
  (interactive)
  (let* ((day (my/rest--view-date))
         (time (if day (encode-time day) (current-time))))
    (my/rest--insert time)
    (when (derived-mode-p 'org-timeblock-mode 'org-timeblock-list-mode)
      (org-timeblock-redraw-buffers))
    (message "Rest added for %s; s sets its time, d its length"
             (format-time-string "%a %d %b" time))))

(defun my/rest--range-minutes (timestamp)
  "Length in minutes of Org TIMESTAMP's time range, or nil without one."
  (when-let* ((hs (org-element-property :hour-start timestamp))
              (ms (org-element-property :minute-start timestamp))
              (he (org-element-property :hour-end timestamp))
              (me (org-element-property :minute-end timestamp))
              (len (- (+ (* 60 he) me) (+ (* 60 hs) ms))))
    (and (> len 0) len)))

(defun my/rest--timer-from-schedule ()
  "In `my/rests-file', make TIMER_MINUTES the SCHEDULED range's length.
Runs on clock-in before the timer starts, so a rest fitted with d on
the grid runs for exactly that long."
  (when (and buffer-file-name
             (file-equal-p buffer-file-name my/rests-file))
    (when-let ((len (my/rest--range-minutes
                     (org-element-property :scheduled (org-element-at-point)))))
      (org-entry-put nil "TIMER_MINUTES" (number-to-string len)))))

;; Depth -10: before `my/org-timer-on-clock-in' reads the property.
(add-hook 'org-clock-in-hook #'my/rest--timer-from-schedule -10)

(with-eval-after-load 'org-timeblock
  (keymap-set org-timeblock-mode-map "R" #'my/rest-block)
  (keymap-set org-timeblock-list-mode-map "R" #'my/rest-block))

(provide 'rests)
;;; rests.el ends here
