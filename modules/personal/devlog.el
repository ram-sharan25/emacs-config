;;; devlog.el --- Per-task devlog and generated Overview -*- lexical-binding: t; -*-

;;; Commentary:
;; Devlog: timestamped lines under the clocked task's "Devlog" heading.
;; Overview: pi rebuilds a generated "Overview" heading of a task.
;; Plan: ~/Stillness/Agent_Stuffs/Plans/emacs-d/devlog/

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-capture)

(defconst rsr/devlog-heading "Devlog"
  "Title of the child heading that holds devlog lines.")

(defconst rsr/devlog-overview-heading "Overview"
  "Title of the child heading that Overview rebuilds.")

(defun rsr/devlog--find-child (title)
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

(defun rsr/devlog--ensure-child (title)
  "Return the position of child TITLE of the heading at point.
Create it as the last child when missing."
  (or (rsr/devlog--find-child title)
      (save-excursion
        (org-back-to-heading t)
        (let ((level (org-current-level)))
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert (make-string (1+ level) ?*) " " title "\n")
          (forward-line -1)
          (point)))))

(defun rsr/devlog--subtree-region (pos)
  "Return (START . END) of the subtree whose heading is at POS."
  (save-excursion
    (goto-char pos)
    (cons (point) (progn (org-end-of-subtree t t) (point)))))

(defconst rsr/devlog--session-regexp "^- Session \\[\\([^]]+\\)\\]"
  "Matches a session line; group 1 is its timestamp.")

(defun rsr/devlog--last-session-time ()
  "Return the latest session time under the heading at point, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (cdr (rsr/devlog--subtree-region (point))))
          latest)
      (while (re-search-forward rsr/devlog--session-regexp end t)
        (let ((time (org-time-string-to-time (match-string 1))))
          (when (or (null latest) (time-less-p latest time))
            (setq latest time))))
      latest)))

(defun rsr/devlog--session-line (last-session clock-start)
  "Return a session line for CLOCK-START, or \"\" if LAST-SESSION is that session.
A new session starts when LAST-SESSION is before CLOCK-START's minute."
  (let ((stamp (format-time-string (org-time-stamp-format t t) clock-start)))
    (if (or (null last-session)
            (time-less-p last-session (org-time-string-to-time stamp)))
        (concat "- Session " stamp "\n")
      "")))

(defvar rsr/devlog--pending-session ""
  "Session line decided by `rsr/devlog-capture-target' for the template.")

;; Entries nest under their session line, so TAB on it folds the session.
;; Each keeps a full inactive timestamp so agenda and searches find it.
(defconst rsr/devlog-capture-template
  "%(identity rsr/devlog--pending-session)  - %U %?"
  "Capture template for a devlog line.")

(defun rsr/devlog-capture-target ()
  "Move to the end of the clocked task's Devlog heading, creating it if needed."
  (unless (org-clocking-p)
    (user-error "Not clocked in: clock into a task first"))
  (set-buffer (marker-buffer org-clock-marker))
  (goto-char org-clock-marker)
  (goto-char (rsr/devlog--ensure-child rsr/devlog-heading))
  (setq rsr/devlog--pending-session
        (rsr/devlog--session-line (rsr/devlog--last-session-time) org-clock-start-time))
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n")))

(defun rsr/devlog--task-heading ()
  "Return the position of the nearest task heading at or above point."
  (save-excursion
    (org-back-to-heading t)
    (while (and (not (org-get-todo-state)) (org-up-heading-safe)))
    (or (and (org-get-todo-state) (point))
        (user-error "Not inside a task"))))

(defun rsr/devlog--task-text ()
  "Return the text of the task at point, minus its generated Overview."
  (save-excursion
    (org-back-to-heading t)
    (let ((task (rsr/devlog--subtree-region (point)))
          (gen (rsr/devlog--find-child rsr/devlog-overview-heading)))
      (if (not gen)
          (buffer-substring-no-properties (car task) (cdr task))
        (let ((g (rsr/devlog--subtree-region gen)))
          (concat (buffer-substring-no-properties (car task) (car g))
                  (buffer-substring-no-properties (cdr g) (cdr task))))))))

(defvar rsr/devlog-overview-prompt-file
  (expand-file-name "prompts/devlog-overview.org"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "System prompt file for Overview.")

(defun rsr/devlog--overview-command (input-file)
  "Return the pi command line that builds the Overview of INPUT-FILE."
  (list "pi" "-p" "--no-session" "--no-context-files"
        "--thinking" "low" "--tools" "read"
        "--append-system-prompt" rsr/devlog-overview-prompt-file
        (concat "@" input-file) "Write the Overview for this task."))

(defconst rsr/devlog-overview-sections
  '("Sources" "Goal" "Findings" "Questions" "Next steps")
  "The only sections Overview may write, in order.")

(defun rsr/devlog--top-headings (text)
  "Return the titles of level-1 headings in TEXT."
  (let ((pos 0) titles)
    (while (string-match "^\\* \\(.+\\)$" text pos)
      (push (string-trim (match-string 1 text)) titles)
      (setq pos (match-end 0)))
    (nreverse titles)))

(defun rsr/devlog--normalize-output (text task-level)
  "Clean pi's TEXT and demote it under a task at TASK-LEVEL.
Signal `user-error' when TEXT has no headings or an unknown section."
  (let* ((clean (replace-regexp-in-string "^```.*\n?" "" text))
         (start (string-match "^\\* " clean)))
    (unless start
      (user-error "Overview: pi returned no sections"))
    (let ((body (concat (string-trim-right (substring clean start)) "\n")))
      (dolist (title (rsr/devlog--top-headings body))
        (unless (member title rsr/devlog-overview-sections)
          (user-error "Overview: unexpected section %S" title)))
      (replace-regexp-in-string
       "^\\*+ "
       (lambda (stars) (concat (make-string (1+ task-level) ?*) stars))
       body t t))))

(defun rsr/devlog--replace-overview (task-pos body)
  "Replace the generated Overview of the task at TASK-POS with BODY."
  (save-excursion
    (goto-char task-pos)
    (let ((old (rsr/devlog--find-child rsr/devlog-overview-heading)))
      (when old
        (let ((region (rsr/devlog--subtree-region old)))
          (delete-region (car region) (cdr region)))))
    (goto-char task-pos)
    (goto-char (rsr/devlog--ensure-child rsr/devlog-overview-heading))
    (forward-line 1)
    (insert body)))

(defun rsr/devlog--overview-target ()
  "Return a marker at the task to build the Overview for.
In Org, the task at point; elsewhere, the clocked task."
  (cond ((derived-mode-p 'org-mode) (copy-marker (rsr/devlog--task-heading)))
        ((org-clocking-p) (copy-marker org-clock-marker))
        (t (user-error "Not in an Org task and not clocked in"))))

(defun rsr/devlog--task-directory (task)
  "Return the directory of the file holding marker TASK.
pi runs there, so the task's relative file: links resolve."
  (with-current-buffer (marker-buffer task)
    (if buffer-file-name
        (file-name-directory buffer-file-name)
      default-directory)))

(defun rsr/devlog-overview ()
  "Rebuild the task's \"Overview\" heading with pi, in the background."
  (interactive)
  (unless (executable-find "pi")
    (user-error "pi not found on exec-path"))
  (let* ((task (rsr/devlog--overview-target))
         (input (with-current-buffer (marker-buffer task)
                  (save-excursion
                    (goto-char task)
                    (make-temp-file "devlog-" nil ".org" (rsr/devlog--task-text)))))
         (default-directory (rsr/devlog--task-directory task))
         (out (generate-new-buffer " *devlog-overview*"))
         (err (get-buffer-create "*devlog-overview-errors*")))
    (with-current-buffer err (erase-buffer))
    ;; pi reads piped stdin to EOF before starting, so close it at once.
    (process-send-eof
     (make-process
      :name "devlog-overview"
      :command (rsr/devlog--overview-command input)
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
                    (rsr/devlog--replace-overview
                     task
                     (rsr/devlog--normalize-output
                      (with-current-buffer out (buffer-string)) level)))
                  (message "Overview updated"))
              (user-error (message "%s" (error-message-string e)))))
          (kill-buffer out)))))
    (message "Building Overview with pi…")))

(when (boundp 'rsr/ai-map)
  (keymap-set rsr/ai-map "o" #'rsr/devlog-overview))

(provide 'devlog)
;;; devlog.el ends here
