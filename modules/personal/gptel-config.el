(require 'my-secrets)
(use-package gptel
  :ensure t
  :init
  ;; Ensure context functions are loaded
  (with-eval-after-load 'gptel
    (require 'gptel-context))
  :bind (:map rsr/global-prefix-map
              ;; Core workflows
              (("a a" . rsr/gptel-quick-chat)     ; quick chat in current buffer
               ("a A" . rsr/gptel-dedicated-chat) ; dedicated chat buffer
               ("a s" . gptel-send)               ; send message

               ;; Context management
               ("a f" . gptel-add-file)           ; add file to context
               ("a F" . rsr/gptel-add-buffer)     ; add current buffer to context
               ("a +" . gptel-context-add)        ; add to context
               ("a -" . gptel-context-remove-all) ; clear context
               ("a =" . rsr/gptel-show-context)   ; show current context

               ;; Model switching
               ("a t" . rsr/gptel-toggle-model)   ; quick toggle models
               ("a T" . rsr/gptel-switch-to-chat) ; explicit chat model
               ("a C" . rsr/gptel-switch-to-coding) ; explicit coding model

               ;; Specialized sessions
               ("a c" . rsr/gptel-coding-session) ; coding workflow
               ("a p" . rsr/gptel-plan-project)   ; project planner
               ("a w" . rsr/gptel-weekly-review-assistant) ; weekly review

               ;; GTD integrations
               ("a i" . rsr/gptel-capture-as-task)    ; capture as inbox task
               ("a n" . rsr/gptel-capture-as-note)    ; capture as fleeting note
               ("a b" . rsr/gptel-break-down-task)    ; break down task
               ("a e" . rsr/gptel-elaborate-task)     ; elaborate task
               ("a l" . rsr/gptel-load-area-context)  ; load Area context
               ("a P" . rsr/gptel-load-project-context) ; load Project context
               ("a g" . rsr/gptel-suggest-tags)       ; suggest GTD tags

               ;; Utilities
               ("a q" . gptel-abort)              ; quit/abort
               ("a m" . gptel-menu)))             ; full menu
  :config
  ;; Customizable variables for local models
  (defvar rsr/gptel-local-host "localhost:11434"
    "Host address for local Ollama instance.")

  (defvar rsr/gptel-chat-model 'qwen2.5:14b
    "Model to use for general chat mode.")

  (defvar rsr/gptel-coding-model 'qwen2.5-coder:32b
    "Model to use for coding mode.")

  ;; Define backends
  (defvar rsr/gptel-chat-backend
    (gptel-make-ollama "Local-Chat"
      :host rsr/gptel-local-host
      :stream t
      :models (list rsr/gptel-chat-model))
    "Backend for general chat with local model.")

  (defvar rsr/gptel-coding-backend
    (gptel-make-ollama "Local-Coding"
      :host rsr/gptel-local-host
      :stream t
      :models (list rsr/gptel-coding-model))
    "Backend for coding tasks with local model.")

  (defvar rsr/gptel-gemini-backend
    (gptel-make-gemini "Gemini" :stream t :key my/gemini-key)
    "Gemini backend as fallback.")

  ;; Switch functions
  (defun rsr/gptel-switch-to-chat ()
    "Switch to general chat model."
    (interactive)
    (setq gptel-backend rsr/gptel-chat-backend
          gptel-model rsr/gptel-chat-model)
    (message "Switched to chat model: %s" rsr/gptel-chat-model))

  (defun rsr/gptel-switch-to-coding ()
    "Switch to coding model."
    (interactive)
    (setq gptel-backend rsr/gptel-coding-backend
          gptel-model rsr/gptel-coding-model)
    (message "Switched to coding model: %s" rsr/gptel-coding-model))

  (defun rsr/gptel-switch-to-gemini ()
    "Switch to Gemini backend."
    (interactive)
    (setq gptel-backend rsr/gptel-gemini-backend
          gptel-model 'gemini-flash-latest)
    (message "Switched to Gemini"))

  ;; Auto-select coding model in programming modes
  (defun rsr/gptel-auto-select-backend ()
    "Automatically select coding backend in programming modes."
    (when (derived-mode-p 'prog-mode)
      (setq-local gptel-backend rsr/gptel-coding-backend
                  gptel-model rsr/gptel-coding-model)))

  (add-hook 'gptel-mode-hook #'rsr/gptel-auto-select-backend)

  ;; Workflow: Quick chat in current buffer/context
  (defun rsr/gptel-quick-chat ()
    "Start gptel in current buffer with appropriate model.
    Auto-selects coding model for code files."
    (interactive)
    (if (derived-mode-p 'prog-mode)
        (rsr/gptel-switch-to-coding)
      (rsr/gptel-switch-to-chat))
    (gptel-mode 1)
    (message "gptel active in current buffer (%s)"
             (if (derived-mode-p 'prog-mode) "coding" "chat")))

  ;; Workflow: Dedicated chat buffer
  (defun rsr/gptel-dedicated-chat ()
    "Open dedicated chat buffer with chat model."
    (interactive)
    (rsr/gptel-switch-to-chat)
    (let ((buf (get-buffer-create "*gptel-chat*")))
      (with-current-buffer buf
        (unless (eq major-mode 'org-mode)
          (org-mode))
        (gptel-mode 1))
      (pop-to-buffer buf))
    (message "Chat buffer ready (model: %s)" rsr/gptel-chat-model))

  ;; Workflow: Coding session with current file in context
  (defun rsr/gptel-coding-session ()
    "Start coding session: coding model + current file in context."
    (interactive)
    (let ((current-file (buffer-file-name)))
      (rsr/gptel-switch-to-coding)
      (let ((buf (get-buffer-create "*gptel-coding*")))
        (with-current-buffer buf
          (unless (eq major-mode 'org-mode)
            (org-mode))
          (gptel-mode 1))
        (pop-to-buffer buf)
        (when current-file
          (gptel-add-file current-file)
          (message "Coding session: %s in context"
                   (file-name-nondirectory current-file))))))

  ;; Quick toggle between models
  (defun rsr/gptel-toggle-model ()
    "Toggle between chat and coding models."
    (interactive)
    (if (eq gptel-backend rsr/gptel-coding-backend)
        (rsr/gptel-switch-to-chat)
      (rsr/gptel-switch-to-coding)))

  ;; Context management helpers
  (defun rsr/gptel-add-buffer ()
    "Add current buffer to gptel context."
    (interactive)
    (when (buffer-file-name)
      (gptel-add-file (buffer-file-name))
      (message "Added %s to context" (buffer-name))))

  (defun rsr/gptel-show-context ()
    "Show current gptel context in a temporary buffer."
    (interactive)
    (require 'gptel-context)
    (let ((context-items (gptel-context--collect)))
      (if context-items
          (with-output-to-temp-buffer "*gptel-context*"
            (princ "Current gptel context:\n\n")
            (dolist (item context-items)
              (let ((source (car item)))
                (princ (format "- %s\n"
                              (if (bufferp source)
                                  (buffer-name source)
                                source))))))
        (message "No context items added"))))

  ;; Mode-line indicator
  (defvar rsr/gptel-mode-line-format
    '(:eval (when gptel-mode
              (propertize
               (format " [AI:%s]"
                       (cond
                        ((eq gptel-backend rsr/gptel-coding-backend) "CODE")
                        ((eq gptel-backend rsr/gptel-chat-backend) "CHAT")
                        ((eq gptel-backend rsr/gptel-gemini-backend) "GEM")
                        (t "?")))
               'face '(:foreground "cyan" :weight bold))))
    "Mode-line indicator for gptel model.")

  ;; Add to mode-line
  (add-to-list 'mode-line-misc-info rsr/gptel-mode-line-format t)

  ;; ============================================================================
  ;; GTD WORKFLOW INTEGRATIONS
  ;; ============================================================================

  (defun rsr/gptel-capture-as-task ()
    "Capture selected AI response as a TODO in inbox."
    (interactive)
    (if (use-region-p)
        (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
          (org-capture-string (concat "* TODO " content) "i")
          (message "Captured to inbox"))
      (message "No region selected")))

  (defun rsr/gptel-capture-as-note ()
    "Capture selected AI response as a fleeting note."
    (interactive)
    (if (use-region-p)
        (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
          (org-capture-string content "u")
          (message "Captured as fleeting note"))
      (message "No region selected")))

  (defun rsr/gptel-break-down-task ()
    "AI breaks down current task into subtasks."
    (interactive)
    (if (derived-mode-p 'org-mode)
        (let* ((task (org-get-heading t t t t))
               (prompt (format "Break down this task into 3-5 concrete, actionable subtasks:\n\n%s\n\nFormat: Just list the subtasks, one per line, starting with a dash." task)))
          (rsr/gptel-switch-to-chat)
          (let ((buf (get-buffer-create "*gptel-task-breakdown*")))
            (with-current-buffer buf
              (erase-buffer)
              (org-mode)
              (gptel-mode 1)
              (insert prompt)
              (goto-char (point-max)))
            (pop-to-buffer buf)
            (gptel-send)
            (message "Breaking down task...")))
      (message "Not in org-mode")))

  (defun rsr/gptel-elaborate-task ()
    "AI elaborates on a vague task to make it more specific."
    (interactive)
    (if (derived-mode-p 'org-mode)
        (let* ((task (org-get-heading t t t t))
               (prompt (format "This task is vague. Rewrite it as a specific, actionable task with clear success criteria:\n\n%s\n\nFormat: Just write the improved task description." task)))
          (rsr/gptel-switch-to-chat)
          (let ((buf (get-buffer-create "*gptel-elaborate*")))
            (with-current-buffer buf
              (erase-buffer)
              (org-mode)
              (gptel-mode 1)
              (insert prompt)
              (goto-char (point-max)))
            (pop-to-buffer buf)
            (gptel-send)
            (message "Elaborating task...")))
      (message "Not in org-mode")))

  (defun rsr/gptel-load-project-context ()
    "Load current project file into gptel context."
    (interactive)
    (require 'paths)
    (if (derived-mode-p 'org-mode)
        (progn
          (rsr/gptel-switch-to-chat)
          (when (buffer-file-name)
            (gptel-add-file (buffer-file-name)))
          (let ((buf (get-buffer-create "*gptel-project*")))
            (with-current-buffer buf
              (unless (eq major-mode 'org-mode)
                (org-mode))
              (gptel-mode 1))
            (pop-to-buffer buf)
            (message "Project loaded. Ask about your project...")))
      (message "Not in org-mode")))

  (defun rsr/gptel-load-area-context ()
    "Select an Area and load it into gptel context."
    (interactive)
    (require 'paths)
    (let* ((area-files (directory-files my/areas-dir nil "\\.org$"))
           (area-names (mapcar (lambda (f) (file-name-sans-extension f)) area-files))
           (selected (completing-read "Select Area: " area-names nil t))
           (area-file (expand-file-name (concat selected ".org") my/areas-dir)))
      (rsr/gptel-switch-to-chat)
      (gptel-add-file area-file)
      (let ((buf (get-buffer-create "*gptel-area*")))
        (with-current-buffer buf
          (unless (eq major-mode 'org-mode)
            (org-mode))
          (gptel-mode 1))
        (pop-to-buffer buf)
        (message "Area '%s' loaded into context" selected))))

  (defun rsr/gptel-plan-project ()
    "AI-assisted project planning workflow."
    (interactive)
    (let* ((project-name (read-string "Project name: "))
           (description (read-string "Brief description: "))
           (prompt (format "Help me plan this project:\n\nProject: %s\nDescription: %s\n\nProvide:\n1. Key phases/milestones\n2. Main tasks for each phase\n3. Potential risks\n4. Success criteria\n\nFormat as an org-mode outline."
                          project-name description)))
      (rsr/gptel-switch-to-chat)
      (let ((buf (get-buffer-create "*gptel-project-plan*")))
        (with-current-buffer buf
          (erase-buffer)
          (org-mode)
          (gptel-mode 1)
          (insert prompt)
          (goto-char (point-max)))
        (pop-to-buffer buf)
        (gptel-send)
        (message "Generating project plan..."))))

  (defun rsr/gptel-weekly-review-assistant ()
    "AI helps with weekly review by analyzing tasks."
    (interactive)
    (require 'paths)
    (rsr/gptel-switch-to-chat)
    ;; Add key GTD files to context
    (gptel-add-file my/inbox-file)
    (gptel-add-file my/next-file)
    (gptel-add-file my/gtd-projects-file)
    (let ((buf (get-buffer-create "*gptel-weekly-review*")))
      (with-current-buffer buf
        (erase-buffer)
        (org-mode)
        (gptel-mode 1)
        (insert "# Weekly Review Assistant\n\n")
        (insert "I've loaded your GTD files (inbox, next, projects). Ask me to:\n\n")
        (insert "- Analyze task distribution across projects\n")
        (insert "- Identify stalled projects (no recent activity)\n")
        (insert "- Suggest priorities for next week\n")
        (insert "- Find tasks that could be batched by context\n")
        (insert "- Review waiting items\n\n")
        (insert "What would you like to review?\n\n"))
      (pop-to-buffer buf)
      (message "Weekly review assistant ready (3 files loaded)")))

  (defun rsr/gptel-suggest-tags ()
    "AI suggests appropriate GTD tags for current task."
    (interactive)
    (if (derived-mode-p 'org-mode)
        (let* ((task (org-get-heading t t t t))
               (prompt (format "Suggest 2-3 appropriate GTD tags for this task:\n\n%s\n\nAvailable tags:\nLocations: @home, @office, @library, @errand\nMode: @deep (high focus), @shallow (low focus)\nActivities: dev, research, study, writing, admin\n\nFormat: Just list the tags, comma-separated." task)))
          (rsr/gptel-switch-to-chat)
          (let ((buf (get-buffer-create "*gptel-tags*")))
            (with-current-buffer buf
              (erase-buffer)
              (org-mode)
              (gptel-mode 1)
              (insert prompt)
              (goto-char (point-max)))
            (pop-to-buffer buf)
            (gptel-send)
            (message "Suggesting tags...")))
      (message "Not in org-mode")))

  ;; Default settings
  (setq gptel-default-mode 'org-mode
        gptel-backend rsr/gptel-chat-backend
        gptel-model rsr/gptel-chat-model))

(provide 'gptel-config)
