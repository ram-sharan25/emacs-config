;;; logs.el --- Per-task logs and generated Overview -*- lexical-binding: t; -*-

;;; Commentary:
;; Logs: timestamped lines under the clocked task's "Logs" heading.
;; Overview: pi rebuilds a generated "Overview" heading of a task.
;; Plan: ~/Stillness/Agent_Stuffs/Plans/emacs-d/devlog/

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-capture)

(defconst rsr/logs-heading "Logs"
  "Title of the child heading that holds log lines.")

(defconst rsr/logs-overview-heading "Overview"
  "Title of the child heading that Overview rebuilds.")

(defun rsr/logs--find-child (title)
  "Return the position of the direct child TITLE of the heading at point, or nil."
  (save-excursion
    (org-back-to-heading t)
    (when (org-goto-first-child)
      (catch 'found
        (while t
          (when (string= (org-get-heading t t t t) title)
            (throw 'found (point)))
          (unless (org-get-next-sibling)
            (throw 'found nil)))))))

(defun rsr/logs--ensure-child (title)
  "Return the position of child TITLE of the heading at point.
Create it as the last child when missing."
  (or (rsr/logs--find-child title)
      (save-excursion
        (org-back-to-heading t)
        (let ((level (org-current-level)))
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert (make-string (1+ level) ?*) " " title "\n")
          (forward-line -1)
          (point)))))

(defun rsr/logs--subtree-region (pos)
  "Return (START . END) of the subtree whose heading is at POS."
  (save-excursion
    (goto-char pos)
    (cons (point) (progn (org-end-of-subtree t t) (point)))))

(defconst rsr/logs--session-regexp "^- Session \\[\\([^]]+\\)\\]"
  "Matches a session line; group 1 is its timestamp.")

(defun rsr/logs--last-session-time ()
  "Return the latest session time under the heading at point, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (cdr (rsr/logs--subtree-region (point))))
          latest)
      (while (re-search-forward rsr/logs--session-regexp end t)
        (let ((time (org-time-string-to-time (match-string 1))))
          (when (or (null latest) (time-less-p latest time))
            (setq latest time))))
      latest)))

(defun rsr/logs--session-line (last-session clock-start)
  "Return a session line for CLOCK-START, or \"\" if LAST-SESSION is that session.
A new session starts when LAST-SESSION is before CLOCK-START's minute."
  (let ((stamp (format-time-string (org-time-stamp-format t t) clock-start)))
    (if (or (null last-session)
            (time-less-p last-session (org-time-string-to-time stamp)))
        (concat "- Session " stamp "\n")
      "")))

(defvar rsr/logs--pending-session ""
  "Session line decided by `rsr/logs-capture-target' for the template.")

;; Entries nest under their session line, so TAB on it folds the session.
;; Each keeps a full inactive timestamp so agenda and searches find it.
(defconst rsr/logs-capture-template
  "%(identity rsr/logs--pending-session)  - %U %?"
  "Capture template for a log line.")

(defvar rsr/logs--capture-heading nil
  "Marker at the Logs heading chosen by `rsr/logs-capture-target'.")

(defun rsr/logs-capture-target ()
  "Move to the end of the clocked task's Logs heading, creating it if needed."
  (unless (org-clocking-p)
    (user-error "Not clocked in: clock into a task first"))
  (set-buffer (marker-buffer org-clock-marker))
  (goto-char org-clock-marker)
  (goto-char (rsr/logs--ensure-child rsr/logs-heading))
  (setq rsr/logs--capture-heading (point-marker))
  (setq rsr/logs--pending-session
        (rsr/logs--session-line (rsr/logs--last-session-time) org-clock-start-time))
  ;; A function target always inserts at point (Org's :exact-position), so
  ;; point must be at the Logs subtree's end, not on its heading.
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n")))

(defun rsr/logs-capture-show-logs ()
  "Widen the capture buffer to show the whole Logs above the new line.
Use as the logs template's :hook."
  (when (markerp rsr/logs--capture-heading)
    (narrow-to-region rsr/logs--capture-heading (point-max))
    (set-marker rsr/logs--capture-heading nil)
    (setq rsr/logs--capture-heading nil)))

(defun rsr/logs--capture-buffer-p (buffer _action)
  "Return non-nil when BUFFER is the capture buffer of a log capture."
  (and (string-prefix-p "CAPTURE-" (if (stringp buffer) buffer (buffer-name buffer)))
       (equal (org-capture-get :target) '(function rsr/logs-capture-target))))

;; Org deletes other windows, then splits; open the capture on the right.
(add-to-list 'display-buffer-alist
             '(rsr/logs--capture-buffer-p
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.5)))

;; Entry stamps under a Logs heading show as [HH:MM] in Org's own date
;; face.  The file keeps the full stamp; only its date part is invisible.
(defvar-local rsr/logs-short-stamps t
  "Non-nil shows log entry stamps as their time only.")

(defconst rsr/logs--entry-stamp-regexp
  (concat "^[ \t]*[-+] \\[\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^]\n]*? \\)"
          "[0-9][0-9]:[0-9][0-9]\\]")
  "Matches an entry stamp; group 1 is the date part that is hidden.")

(defun rsr/logs--in-logs-p ()
  "Return non-nil when point is anywhere inside a Logs heading's subtree."
  (save-excursion
    (and (re-search-backward org-outline-regexp-bol nil t)
         (catch 'found
           (while t
             (when (string= (org-get-heading t t t t) rsr/logs-heading)
               (throw 'found t))
             (unless (org-up-heading-safe)
               (throw 'found nil)))))))

(defun rsr/logs--match-entry-stamp (limit)
  "Font-lock matcher for entry stamps under a Logs heading, up to LIMIT."
  (let (found)
    (while (and (not found)
                rsr/logs-short-stamps
                (re-search-forward rsr/logs--entry-stamp-regexp limit t))
      (setq found (save-match-data (rsr/logs--in-logs-p))))
    found))

(defconst rsr/logs--stamp-font-lock-keywords
  '((rsr/logs--match-entry-stamp
     (1 '(face nil invisible rsr-logs-stamp))))
  "Font-lock keywords that shorten log entry stamps.")

(defun rsr/logs--activate-short-stamps ()
  "Shorten log entry stamps in this Org buffer."
  (add-to-invisibility-spec 'rsr-logs-stamp)
  (font-lock-add-keywords nil rsr/logs--stamp-font-lock-keywords 'append))

(add-hook 'org-mode-hook #'rsr/logs--activate-short-stamps)

(defun rsr/logs-toggle-short-stamps ()
  "Toggle between short and full log entry stamps in this buffer."
  (interactive)
  (setq rsr/logs-short-stamps (not rsr/logs-short-stamps))
  (font-lock-flush)
  (message "Logs stamps: %s" (if rsr/logs-short-stamps "time only" "full")))

(defun rsr/logs--task-heading ()
  "Return the position of the nearest task heading at or above point."
  (save-excursion
    (org-back-to-heading t)
    (while (and (not (org-get-todo-state)) (org-up-heading-safe)))
    (or (and (org-get-todo-state) (point))
        (user-error "Not inside a task"))))

(defun rsr/logs--task-text ()
  "Return the text of the task at point, minus its generated Overview."
  (save-excursion
    (org-back-to-heading t)
    (let ((task (rsr/logs--subtree-region (point)))
          (gen (rsr/logs--find-child rsr/logs-overview-heading)))
      (if (not gen)
          (buffer-substring-no-properties (car task) (cdr task))
        (let ((g (rsr/logs--subtree-region gen)))
          (concat (buffer-substring-no-properties (car task) (car g))
                  (buffer-substring-no-properties (cdr g) (cdr task))))))))

(defvar rsr/logs-overview-prompt-file
  (expand-file-name "prompts/logs-overview.org"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "System prompt file for Overview.")

(defun rsr/logs--overview-command (input-file)
  "Return the pi command line that builds the Overview of INPUT-FILE."
  (list "pi" "-p" "--no-session" "--no-context-files"
        "--thinking" "low" "--tools" "read"
        "--append-system-prompt" rsr/logs-overview-prompt-file
        (concat "@" input-file) "Write the Overview for this task."))

(defconst rsr/logs-overview-sections
  '("Sources" "Goal" "Findings" "Questions" "Next steps")
  "The only sections Overview may write, in order.")

(defun rsr/logs--top-headings (text)
  "Return the titles of level-1 headings in TEXT."
  (let ((pos 0) titles)
    (while (string-match "^\\* \\(.+\\)$" text pos)
      (push (string-trim (match-string 1 text)) titles)
      (setq pos (match-end 0)))
    (nreverse titles)))

(defun rsr/logs--normalize-output (text task-level)
  "Clean pi's TEXT and demote it under a task at TASK-LEVEL.
Signal `user-error' when TEXT has no headings or an unknown section."
  (let* ((clean (replace-regexp-in-string "^```.*\n?" "" text))
         (start (string-match "^\\* " clean)))
    (unless start
      (user-error "Overview: pi returned no sections"))
    (let ((body (concat (string-trim-right (substring clean start)) "\n")))
      (dolist (title (rsr/logs--top-headings body))
        (unless (member title rsr/logs-overview-sections)
          (user-error "Overview: unexpected section %S" title)))
      (replace-regexp-in-string
       "^\\*+ "
       (lambda (stars) (concat (make-string (1+ task-level) ?*) stars))
       body t t))))

(defun rsr/logs--replace-overview (task-pos body)
  "Replace the generated Overview of the task at TASK-POS with BODY."
  (save-excursion
    (goto-char task-pos)
    (let ((old (rsr/logs--find-child rsr/logs-overview-heading)))
      (when old
        (let ((region (rsr/logs--subtree-region old)))
          (delete-region (car region) (cdr region)))))
    (goto-char task-pos)
    (goto-char (rsr/logs--ensure-child rsr/logs-overview-heading))
    (forward-line 1)
    (insert body)))

(defun rsr/logs--overview-target ()
  "Return a marker at the task to build the Overview for.
In Org, the task at point; elsewhere, the clocked task."
  (cond ((derived-mode-p 'org-mode) (copy-marker (rsr/logs--task-heading)))
        ((org-clocking-p) (copy-marker org-clock-marker))
        (t (user-error "Not in an Org task and not clocked in"))))

(defun rsr/logs--task-directory (task)
  "Return the directory of the file holding marker TASK.
pi runs there, so the task's relative file: links resolve."
  (with-current-buffer (marker-buffer task)
    (if buffer-file-name
        (file-name-directory buffer-file-name)
      default-directory)))

(defun rsr/logs-overview ()
  "Rebuild the task's \"Overview\" heading with pi, in the background."
  (interactive)
  (unless (executable-find "pi")
    (user-error "pi not found on exec-path"))
  (let* ((task (rsr/logs--overview-target))
         (input (with-current-buffer (marker-buffer task)
                  (save-excursion
                    (goto-char task)
                    (make-temp-file "logs-" nil ".org" (rsr/logs--task-text)))))
         (default-directory (rsr/logs--task-directory task))
         (out (generate-new-buffer " *logs-overview*"))
         (err (get-buffer-create "*logs-overview-errors*")))
    (with-current-buffer err (erase-buffer))
    ;; pi reads piped stdin to EOF before starting, so close it at once.
    (process-send-eof
     (make-process
      :name "logs-overview"
      :command (rsr/logs--overview-command input)
      :buffer out
      :stderr err
      :connection-type 'pipe
      :sentinel
      (lambda (proc _event)
        (unless (process-live-p proc)
          (delete-file input)
          (if (not (zerop (process-exit-status proc)))
              (message "Overview failed (pi exit %d); see %s"
                       (process-exit-status proc) (buffer-name err))
            (condition-case e
                (with-current-buffer (marker-buffer task)
                  (let ((level (save-excursion (goto-char task) (org-current-level))))
                    (rsr/logs--replace-overview
                     task
                     (rsr/logs--normalize-output
                      (with-current-buffer out (buffer-string)) level)))
                  (message "Overview updated"))
              (user-error (message "%s" (error-message-string e)))))
          (kill-buffer out)))))
    (message "Building Overview with pi…")))

(when (boundp 'rsr/ai-map)
  (keymap-set rsr/ai-map "o" #'rsr/logs-overview))

(provide 'logs)
;;; logs.el ends here
