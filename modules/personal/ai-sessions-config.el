;;; ai-sessions-config.el --- agent-sessions dashboard -*- lexical-binding: t -*-

;;; Commentary:
;; The dashboard for Claude/Codex/pi sessions, and the channel `emacs-share'
;; and agent hooks report through.  The package lives in the customize-agents
;; repo and is loaded from there.  Every agent runs in a tmux window
;; (agent-sessions-tmux.el); C-c o t is bound in keymaps.el, C-c i i /
;; C-c i p in terminal-config.el.  C-c i w is the planning workflow menu
;; (agent-workflow.el): lowercase opens a stage, uppercase its review, `-a'
;; cycles the agent (kept until changed), `-c' opens a child of the current
;; session, `y' bring in.  `rsr/ai-map' is defined in keymaps.el.
;; C-c i l goes back to the last agent viewed (again to toggle).
;; Hook wiring into ~/.claude, ~/.codex and Pi is one-time:
;;   M-x rsr/agent-sessions-install

;;; Code:

(add-to-list 'load-path
             (expand-file-name "~/Stillness/Development/customize-agents/main/emacs/agent-sessions"))
(if (not (and (require 'agent-sessions nil t)
              (require 'agent-workflow nil t)))
    (display-warning 'ai-sessions-config
                     "agent-sessions not found on load-path; the agent dashboard (C-c i d/l/r/w) is unavailable.")
  (setq rsr/agent-sessions-terminal 'tmux)
  ;; The database defaults to ~/.emacs.d/agent-sessions.db (gitignored).

  (keymap-set rsr/ai-map "d" #'rsr/agent-sessions-list)          ; C-c i d
  (keymap-set rsr/ai-map "l" #'rsr/agent-sessions-visit-last-agent) ; C-c i l
  (keymap-set rsr/ai-map "r" #'rsr/agent-sessions-send-region)   ; C-c i r
  (keymap-set rsr/ai-map "w" #'rsr/agent-workflow-menu)           ; C-c i w

  (with-eval-after-load 'ghostel
    (define-key ghostel-mode-map (kbd "C-c C-l") #'rsr/agent-session-insert-org-link))

  (add-to-list 'project-switch-commands '(rsr/agent-session-switch-or-new "Agent Session"))

  ;; Advice on the terminal packages, focus hook, Org link type, C-x p a, server.
  (rsr/agent-session-setup))

(provide 'ai-sessions-config)
;;; ai-sessions-config.el ends here
