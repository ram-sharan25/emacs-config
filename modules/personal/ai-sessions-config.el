;;; ai-sessions-config.el --- agent-sessions dashboard -*- lexical-binding: t -*-

;;; Commentary:
;; The dashboard for Claude/Codex/pi sessions, and the channel `emacs-share'
;; and agent hooks report through.  The package lives in the customize-agents
;; repo and is loaded from there.  Every agent runs in a tmux window
;; (agent-sessions-tmux.el); C-c t / C-c i i / C-c i p are bound in
;; terminal-config.el.
;;
;; Named to load after ai-keymap.el, which defines `rsr/ai-map'.
;; Hook wiring into ~/.claude, ~/.codex and Pi is one-time:
;;   M-x rsr/agent-sessions-install

;;; Code:

(add-to-list 'load-path
             (expand-file-name "~/Stillness/Development/customize-agents/emacs/agent-sessions"))
(require 'agent-sessions)

(setq rsr/agent-sessions-terminal 'tmux
      rsr/agent-sessions-show-all-worktrees t)
;; The database defaults to ~/.emacs.d/agent-sessions.db (gitignored).

(keymap-set rsr/ai-map "d" #'rsr/agent-sessions-list)          ; C-c i d
(keymap-set rsr/ai-map "r" #'rsr/agent-sessions-send-region)   ; C-c i r

(with-eval-after-load 'ghostel
  (define-key ghostel-mode-map (kbd "C-c C-l") #'rsr/agent-session-insert-org-link))

(add-to-list 'project-switch-commands '(rsr/agent-session-switch-or-new "Agent Session"))

;; Advice on the terminal packages, focus hook, Org link type, C-x p a, server.
(rsr/agent-session-setup)

(provide 'ai-sessions-config)
;;; ai-sessions-config.el ends here
