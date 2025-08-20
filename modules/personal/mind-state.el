;;;; mind-state-tracker.el --- Random mind-state prompts for daily awareness -*- lexical-binding: t; -*-

(require 'org-capture)

;; === ADDED: Missing Variable and Helper Function Definitions ===
(defvar journal-file (expand-file-name "~/org/journal.org")
  "Path to the Org mode journal file for mind-state tracking.")

(defun ensure-date-tree ()
  "Ensure the journal file has a heading for today's date."
  (let* ((today (format-time-string "%Y-%m-%d %A"))
	 (year (format-time-string "%Y"))
	 (month-name (format-time-string "%B")))
    (with-current-buffer (find-file-noselect journal-file)
      (unless (org-find-heading-in-buffer (concat "* " year))
	(goto-char (point-max)) (insert (concat "\n* " year "\n")))
      (unless (org-find-heading-in-buffer (concat "** " month-name))
	(org-find-heading-in-buffer (concat "* " year)) (org-end-of-subtree) (insert (concat "\n** " month-name "\n")))
      (unless (org-find-heading-in-buffer (concat "*** " today))
	(org-find-heading-in-buffer (concat "** " month-name)) (org-end-of-subtree) (insert (concat "\n*** " today "\n"))))))

(defun org-find-heading-in-buffer (heading)
  "Search for an Org heading from the beginning of the buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward heading nil t)))

;; === Configuration Variables ===
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

;; === Internal Variables ===
(defvar mind-state-timer nil)
(defvar mind-state-prompts-today 0)
(defvar mind-state-today-date nil)

;; === Predefined Mind-State Options ===
(defvar mind-state-options
  '("Focused" "Distracted" "Calm" "Anxious" "Energetic" "Tired"
    "Creative" "Analytical" "Overwhelmed" "Content" "Restless"
    "Motivated" "Procrastinating" "Clear" "Confused" "Happy"
    "Stressed" "Relaxed" "Curious" "Bored" "Inspired"))

;; === Utility Functions ===
(defun mind-state--current-date-string ()
  (format-time-string "%Y-%m-%d"))

(defun mind-state--reset-daily-counter ()
  (interactive)
  (let ((today (mind-state--current-date-string)))
    (unless (string= today mind-state-today-date)
      (setq mind-state-today-date today
	    mind-state-prompts-today 0))))

(defun mind-state--within-active-hours-p ()
  (let* ((current-hour (string-to-number (format-time-string "%H")))
	 (start-hour (car mind-state-active-hours))
	 (end-hour (cadr mind-state-active-hours)))
    (and (>= current-hour start-hour)
	 (< current-hour end-hour))))

(defun mind-state--user-is-active-p ()
  "Return t if user has been active in Emacs recently."
  ;; FIX: Convert the time object from `current-idle-time` to seconds.
  (< (time-to-seconds (or (current-idle-time) '(0 . 0))) mind-state-idle-threshold))

(defun mind-state--should-prompt-p ()
  (interactive)
  (and (mind-state--within-active-hours-p)
       (mind-state--user-is-active-p)
       (< mind-state-prompts-today mind-state-target-prompts)))

(defun mind-state--calculate-next-interval ()
  "Calculate random interval in seconds until next prompt."
  (let* ((remaining-prompts (max 1 (- mind-state-target-prompts mind-state-prompts-today)))
	 (remaining-hours (max 1 (- (cadr mind-state-active-hours)
				    (string-to-number (format-time-string "%H")))))
	 (avg-interval (/ (* remaining-hours 60) remaining-prompts))
	 (min-interval (max mind-state-min-interval (/ avg-interval 2)))
	 ;; FIX: Ensure max-interval is never smaller than min-interval.
	 (max-interval (max min-interval (min mind-state-max-interval (* avg-interval 2))))
	 (random-interval (+ min-interval (random (1+ (- max-interval min-interval))))))
    (* random-interval 60))) ; Convert to seconds

;; === Core Prompt Function ===
(defun mind-state--prompt-user ()
  (interactive)
  (when (mind-state--should-prompt-p)
    (let* ((state-with-completion
	    (completing-read "How are you feeling right now? " mind-state-options nil nil))
	   (custom-state (if (string-empty-p state-with-completion)
			     (read-string "Describe your current mind-state: ")
			   state-with-completion))
	   (additional-notes (read-string "Any additional notes (optional): ")))
      (ensure-date-tree)
      (let ((entry-text (concat "Mind-State: " custom-state
				(if (not (string-empty-p additional-notes))
				    (concat " | Notes: " additional-notes)
				  ""))))
	(with-current-buffer (find-file-noselect journal-file)
	  (save-excursion
	    (let ((day-heading (format-time-string "^\\*\\*\\* %Y-%m-%d")))
	      (goto-char (point-min))
	      (when (re-search-forward day-heading nil t)
		(org-end-of-subtree)
		(unless (bolp) (insert "\n"))
		(insert (format "**** %s :MIND-STATE:\n - %s\n"
				(format-time-string "%I:%M %p")
				entry-text)))))))
      (setq mind-state-prompts-today (1+ mind-state-prompts-today))
      (message "Mind-state logged: %s (%d/%d today)"
	       custom-state mind-state-prompts-today mind-state-target-prompts)))
  (mind-state--schedule-next-prompt))

;; === Timer Management ===
(defun mind-state--schedule-next-prompt ()
  (when mind-state-timer
    (cancel-timer mind-state-timer))
  (when (< mind-state-prompts-today mind-state-target-prompts)
    (let ((next-interval (mind-state--calculate-next-interval)))
      (setq mind-state-timer
	    (run-at-time next-interval nil #'mind-state--prompt-user))
      (message "Next mind-state check in ~%d minutes"
	       (/ next-interval 60)))))

;; === Interactive Commands ===
(defun mind-state-start-tracking ()
  (interactive)
  (mind-state--reset-daily-counter)
  (mind-state--schedule-next-prompt)
  (message "Mind-state tracking started. Target: %d prompts today." mind-state-target-prompts))

(defun mind-state-stop-tracking ()
  (interactive)
  (when mind-state-timer
    (cancel-timer mind-state-timer)
    (setq mind-state-timer nil))
  (message "Mind-state tracking stopped."))

(defun mind-state-status ()
  (interactive)
  (mind-state--reset-daily-counter)
  (message "Mind-state tracking: %s | Today: %d/%d prompts | Active hours: %02d:00-%02d:00"
	   (if mind-state-timer "ACTIVE" "INACTIVE")
	   mind-state-prompts-today mind-state-target-prompts
	   (car mind-state-active-hours) (cadr mind-state-active-hours)))

(defun mind-state-manual-log ()
  (interactive)
  (mind-state--reset-daily-counter)
  (mind-state--prompt-user))

(defun mind-state-configure ()
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
  (message "Mind-state tracking configured. Restart tracking for changes to take effect."))

;; === Auto-start on Emacs startup ===
(defun mind-state-auto-start ()
  (when (mind-state--within-active-hours-p)
    (mind-state-start-tracking)))

(define-prefix-command 'mind-state-map)
(global-set-key (kbd "C-c s") 'mind-state-map)
(define-key mind-state-map (kbd "s") #'mind-state-start-tracking)
(define-key mind-state-map (kbd "q") #'mind-state-stop-tracking)
(define-key mind-state-map (kbd "l") #'mind-state-manual-log)
(define-key mind-state-map (kbd "?") #'mind-state-status)
(define-key mind-state-map (kbd "c") #'mind-state-configure)

(add-hook 'emacs-startup-hook #'mind-state-auto-start)

(provide 'mind-state-tracker)
