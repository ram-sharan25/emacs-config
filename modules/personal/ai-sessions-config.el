;;; ai-sessions-config.el --- agent-sessions dashboard -*- lexical-binding: t -*-

;;; Commentary:
;; The dashboard for Claude/Codex/pi sessions, and the channel `emacs-share'
;; and agent hooks report through.  The package lives in the customize-agents
;; repo and is loaded from there.  Every agent runs in a tmux window
;; (agent-sessions-tmux.el); C-c o t is bound in keymaps.el, C-c i i /
;; C-c i p in terminal-config.el.  C-c i w is the planning workflow
;; (agent-workflow.el): lowercase opens a stage, uppercase its review, `c'
;; the same keys as a child of the current session, `y' bring in; C-u = Pi.  `rsr/ai-map' is defined in keymaps.el.
;; Hook wiring into ~/.claude, ~/.codex and Pi is one-time:
;;   M-x rsr/agent-sessions-install

;;; Code:

(add-to-list 'load-path
             (expand-file-name "~/Stillness/Development/customize-agents/emacs/agent-sessions"))
(require 'agent-sessions)
(require 'agent-workflow)

(setq rsr/agent-sessions-terminal 'tmux)
;; The database defaults to ~/.emacs.d/agent-sessions.db (gitignored).

(keymap-set rsr/ai-map "d" #'rsr/agent-sessions-list)          ; C-c i d
(keymap-set rsr/ai-map "r" #'rsr/agent-sessions-send-region)   ; C-c i r
(keymap-set rsr/ai-map "w" rsr/agent-workflow-map)              ; C-c i w

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c i w" "workflow" "C-c i w c" "child..."
    "M-m a w" "workflow" "M-m a w c" "child..."))

(with-eval-after-load 'ghostel
  (define-key ghostel-mode-map (kbd "C-c C-l") #'rsr/agent-session-insert-org-link))

(add-to-list 'project-switch-commands '(rsr/agent-session-switch-or-new "Agent Session"))

;; Advice on the terminal packages, focus hook, Org link type, C-x p a, server.
(rsr/agent-session-setup)

(provide 'ai-sessions-config)
;;; ai-sessions-config.el ends here
