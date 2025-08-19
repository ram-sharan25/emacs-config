;;; mind-state-tracker.el --- Random mind-state prompts for daily awareness -*- lexical-binding: t; -*-

(require 'org-capture)

;; === Configuration Variables ===
(defvar mind-state-target-prompts 10
  "Target number of mind-state prompts per day.")

(defvar mind-state-active-hours '(8 22)
  "Hours between which mind-state prompts are active (24-hour format).
First element is start hour, second is end hour.")

(defvar mind-state-min-interval 30
  "Minimum minutes between mind-state prompts.")

(defvar mind-state-max-interval 180
  "Maximum minutes between mind-state prompts.")

(defvar mind-state-idle-threshold 300
  "Minimum seconds of Emacs activity required before showing prompt.")

;; === Internal Variables ===
(defvar mind-state-timer nil
  "Timer object for the next mind-state prompt.")

(defvar mind-state-last-prompt-time nil
  "Time of the last mind-state prompt.")

(defvar mind-state-prompts-today 0
  "Number of prompts shown today.")

(defvar mind-state-today-date nil
  "Date string for tracking daily reset.")

;; === Predefined Mind-State Options ===
(defvar mind-state-options
  '("Focused" "Distracted" "Calm" "Anxious" "Energetic" "Tired"
    "Creative" "Analytical" "Overwhelmed" "Content" "Restless"
    "Motivated" "Procrastinating" "Clear" "Confused" "Happy"
    "Stressed" "Relaxed" "Curious" "Bored" "Inspired")
  "List of predefined mind-state options.")

;; === Utility Functions ===
(defun mind-state--current-date-string ()
  "Return current date as YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d"))

(defun mind-state--reset-daily-counter ()
  (interactive)
  "Reset the daily counter if it's a new day."
  (let ((today (mind-state--current-date-string)))
    (unless (string= today mind-state-today-date)
      (setq mind-state-today-date today
	    mind-state-prompts-today 0))))

(defun mind-state--within-active-hours-p ()
  "Return t if current time is within active hours."
  (let* ((current-hour (string-to-number (format-time-string "%H")))
	 (start-hour (car mind-state-active-hours))
	 (end-hour (cadr mind-state-active-hours)))
    (and (>= current-hour start-hour)
	 (< current-hour end-hour))))

(defun mind-state--user-is-active-p ()
  "Return t if user has been active in Emacs recently."
  (< (or (current-idle-time) 0) mind-state-idle-threshold))

(defun mind-state--should-prompt-p ()
  "Return t if we should show a mind-state prompt now."
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
	 (max-interval (min mind-state-max-interval (* avg-interval 2)))
	 (random-interval (+ min-interval (random (- max-interval min-interval)))))
    (* random-interval 60))) ; Convert to seconds

;; === Core Prompt Function ===
(defun mind-state--prompt-user ()
  "Prompt user for current mind-state and log it."
  (when (mind-state--should-prompt-p)
    (let* ((state-with-completion
	    (completing-read "How are you feeling right now? "
			   mind-state-options nil nil))
	   (custom-state (if (string-empty-p state-with-completion)
			    (read-string "Describe your current mind-state: ")
			  state-with-completion))
	   (additional-notes (read-string "Any additional notes (optional): ")))

      ;; Ensure the journal file has today's date tree
      (with-current-buffer (find-file-noselect journal-file)
	(ensure-date-tree))

      ;; Create the journal entry
      (let ((entry-text (concat "Mind-State: " custom-state
			       (if (not (string-empty-p additional-notes))
				   (concat " | Notes: " additional-notes)
				 ""))))
	(with-current-buffer (find-file-noselect journal-file)
	  (save-excursion
	    ;; Navigate to today's entry
	    (let ((day-heading (format-time-string "^\\*\\*\\* %Y-%m-%d")))
	      (goto-char (point-min))
	      (when (re-search-forward day-heading nil t)
		(org-end-of-subtree)
		(unless (bolp) (insert "\n"))
		(insert (format "**** %s :MIND-STATE:\n - %s\n - [[%s][source]]\n"
			      (format-time-string "%I:%M %p")
			      entry-text
			      (format-time-string "%F")))))))

	;; Update counters
	(setq mind-state-prompts-today (1+ mind-state-prompts-today)
	      mind-state-last-prompt-time (current-time))

	(message "Mind-state logged: %s (%d/%d today)"
		 custom-state mind-state-prompts-today mind-state-target-prompts))))

  ;; Schedule next prompt
  (mind-state--schedule-next-prompt))

;; === Timer Management ===
(defun mind-state--schedule-next-prompt ()
  "Schedule the next random mind-state prompt."
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
  "Start the mind-state tracking system."
  (interactive)
  (mind-state--reset-daily-counter)
  (mind-state--schedule-next-prompt)
  (message "Mind-state tracking started. Target: %d prompts today."
	   mind-state-target-prompts))

(defun mind-state-stop-tracking ()
  "Stop the mind-state tracking system."
  (interactive)
  (when mind-state-timer
    (cancel-timer mind-state-timer)
    (setq mind-state-timer nil))
  (message "Mind-state tracking stopped."))

(defun mind-state-status ()
  "Show current mind-state tracking status."
  (interactive)
  (mind-state--reset-daily-counter)
  (message "Mind-state tracking: %s | Today: %d/%d prompts | Active hours: %02d:00-%02d:00"
	   (if mind-state-timer "ACTIVE" "INACTIVE")
	   mind-state-prompts-today mind-state-target-prompts
	   (car mind-state-active-hours) (cadr mind-state-active-hours)))

(defun mind-state-manual-log ()
  "Manually log a mind-state entry."
  (interactive)
  (mind-state--reset-daily-counter)
  (mind-state--prompt-user))

(defun mind-state-configure ()
  "Configure mind-state tracking settings."
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
  "Auto-start mind-state tracking if within active hours."
  (when (mind-state--within-active-hours-p)
    (mind-state-start-tracking)))

(define-prefix-command 'mind-state-map)
(global-set-key (kbd "C-c s") 'mind-state-map)

;; Define mind-state commands under the prefix
(define-key mind-state-map (kbd "s") #'mind-state-start-tracking)   ; Start
(define-key mind-state-map (kbd "q") #'mind-state-stop-tracking)    ; Quit/Stop
(define-key mind-state-map (kbd "l") #'mind-state-manual-log)       ; Log manually
(define-key mind-state-map (kbd "?") #'mind-state-status)           ; Status
(define-key mind-state-map (kbd "c") #'mind-state-configure)        ; Configure

(add-hook 'emacs-startup-hook #'mind-state-auto-start)

(provide 'mind-state-tracker)
