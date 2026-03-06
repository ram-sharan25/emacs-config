;;; gptel-config.el --- AI assistant configuration -*- lexical-binding: t; -*-

;;; Code:

(require 'my-secrets)

;;; --- GPTel ---

(use-package gptel
  :ensure t
  :bind (:map rsr/global-prefix-map
              ;; Core
              ("a a" . rsr/gptel-quick-chat)
              ("a A" . rsr/gptel-dedicated-chat)
              ("a s" . gptel-send)
              ("a c" . rsr/gptel-coding-session)
              ("a q" . gptel-abort)
              ("a m" . gptel-menu)

              ;; Context
              ("a f" . gptel-add-file)
              ("a F" . rsr/gptel-add-buffer)
              ("a +" . gptel-context-add)
              ("a -" . gptel-context-remove-all)
              ("a =" . rsr/gptel-show-context)

              ;; Model switching
              ("a t" . rsr/gptel-toggle-model)
              ("a T" . rsr/gptel-switch-to-chat)
              ("a C" . rsr/gptel-switch-to-coding)
              ("a G" . rsr/gptel-switch-to-gemini))
  :config
  (require 'gptel-context)

  ;;; Backends

  (defvar rsr/gptel-local-host "localhost:11434"
    "Host address for local Ollama instance.")

  (defvar rsr/gptel-chat-model 'qwen2.5:14b
    "Model to use for general chat.")

  (defvar rsr/gptel-coding-model 'qwen2.5-coder:32b
    "Model to use for coding tasks.")

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
    "Gemini backend.")

  ;;; Model switch functions

  (defun rsr/gptel-switch-to-chat ()
    "Switch to general chat model."
    (interactive)
    (setq gptel-backend rsr/gptel-chat-backend
          gptel-model rsr/gptel-chat-model)
    (message "Switched to chat: %s" rsr/gptel-chat-model))

  (defun rsr/gptel-switch-to-coding ()
    "Switch to coding model."
    (interactive)
    (setq gptel-backend rsr/gptel-coding-backend
          gptel-model rsr/gptel-coding-model)
    (message "Switched to coding: %s" rsr/gptel-coding-model))

  (defun rsr/gptel-switch-to-gemini ()
    "Switch to Gemini backend."
    (interactive)
    (setq gptel-backend rsr/gptel-gemini-backend
          gptel-model 'gemini-flash-latest)
    (message "Switched to Gemini"))

  (defun rsr/gptel-toggle-model ()
    "Toggle between chat and coding models."
    (interactive)
    (if (eq gptel-backend rsr/gptel-coding-backend)
        (rsr/gptel-switch-to-chat)
      (rsr/gptel-switch-to-coding)))

  ;; Auto-select coding model in prog-mode buffers
  (defun rsr/gptel-auto-select-backend ()
    "Select coding backend automatically in programming modes."
    (when (derived-mode-p 'prog-mode)
      (setq-local gptel-backend rsr/gptel-coding-backend
                  gptel-model rsr/gptel-coding-model)))

  (add-hook 'gptel-mode-hook #'rsr/gptel-auto-select-backend)

  ;;; Workflows

  (defun rsr/gptel-quick-chat ()
    "Start gptel in current buffer, auto-selecting model by mode."
    (interactive)
    (if (derived-mode-p 'prog-mode)
        (rsr/gptel-switch-to-coding)
      (rsr/gptel-switch-to-chat))
    (gptel-mode 1)
    (message "gptel active (%s)"
             (if (derived-mode-p 'prog-mode) "coding" "chat")))

  (defun rsr/gptel-dedicated-chat ()
    "Open a dedicated chat buffer with the chat model."
    (interactive)
    (rsr/gptel-switch-to-chat)
    (let ((buf (get-buffer-create "*gptel-chat*")))
      (with-current-buffer buf
        (unless (eq major-mode 'org-mode)
          (org-mode))
        (gptel-mode 1))
      (pop-to-buffer buf)))

  (defun rsr/gptel-coding-session ()
    "Start coding session with current file loaded into context."
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

  ;;; Context helpers

  (defun rsr/gptel-add-buffer ()
    "Add current buffer file to gptel context."
    (interactive)
    (if (buffer-file-name)
        (progn
          (gptel-add-file (buffer-file-name))
          (message "Added %s to context" (buffer-name)))
      (message "Buffer has no file")))

  (defun rsr/gptel-show-context ()
    "Show current gptel context in a temporary buffer."
    (interactive)
    (let ((context gptel-context--alist))
      (if context
          (with-output-to-temp-buffer "*gptel-context*"
            (princ "Current gptel context:\n\n")
            (dolist (item context)
              (princ (format "- %s\n" (car item)))))
        (message "No context items added"))))

  ;;; Mode-line indicator

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
    "Mode-line indicator showing active gptel model.")

  (add-to-list 'mode-line-misc-info rsr/gptel-mode-line-format t)

  ;;; Defaults

  (setq gptel-default-mode 'org-mode
        gptel-backend rsr/gptel-chat-backend
        gptel-model rsr/gptel-chat-model))

(provide 'gptel-config)
;;; gptel-config.el ends here
