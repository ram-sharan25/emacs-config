;;; terminal-config.el --- ghostel as the one terminal emulator -*- lexical-binding: t -*-

;;; Commentary:
;; ghostel (libghostty-vt) replaces vterm and eat: faster on heavy output, and
;; the only one that passes mouse, OSC 9/777 notifications, Kitty images and
;; Claude Code's Shift+Enter / synchronized output through correctly.

;;; Code:

(use-package ghostel
  :ensure t
  :defer t
  :custom
  ;; Fetch a matching native module after an update without prompting.
  (ghostel-module-auto-install 'download)
  ;; ~50k lines of history at ~130 columns (the 5 MB default keeps ~4k).
  (ghostel-max-scrollback (* 60 1024 1024))
  :config
  ;; Ghostty's built-in colours (its config sets none), so a terminal looks the
  ;; same in Emacs as in Ghostty: `ghostty +show-config --default'.
  (set-face-attribute 'ghostel-default nil :foreground "#ffffff" :background "#282c34")
  ;; Ghostty's own font too: JetBrains Mono at regular weight, rather than the
  ;; Fira Code Medium of code buffers.  Ghostty bundles JetBrains Mono, Emacs
  ;; needs it installed (brew install --cask font-jetbrains-mono); until then,
  ;; Fira Code at regular weight.
  (set-face-attribute 'ghostel-default nil
                      :family (if (find-font (font-spec :family "JetBrains Mono"))
                                  "JetBrains Mono"
                                "Fira Code")
                      :weight 'regular)
  (cl-loop for (name color) on
           '(black "#1d1f21" red "#cc6666" green "#b5bd68" yellow "#f0c674"
             blue "#81a2be" magenta "#b294bb" cyan "#8abeb7" white "#c5c8c6"
             bright-black "#666666" bright-red "#d54e53" bright-green "#b9ca4a"
             bright-yellow "#e7c547" bright-blue "#7aa6da" bright-magenta "#c397d8"
             bright-cyan "#70c0b1" bright-white "#eaeaea")
           by #'cddr
           do (set-face-attribute (intern (format "ghostel-color-%s" name)) nil
                                  :foreground color :background color))
  ;; An Emacs launched from a shell inherits that shell's tmux, Ghostty and
  ;; Claude Code variables and hands them to every terminal: tmux then thinks
  ;; it's nested, yazi thinks it's in Ghostty proper, and Claude marks itself
  ;; a child session and stops saving transcripts.  A bare "KEY" unsets it.
  (setq ghostel-environment
        (seq-keep (lambda (kv)
                    (let ((k (car (split-string kv "="))))
                      (and (string-match-p
                            "\\`\\(TMUX\\|TERM_PROGRAM\\|GHOSTTY_\\|CLAUDE\\)" k)
                           k)))
                  process-environment)))

;;; tmux, and agent CLIs inside it
;;
;; Terminals and agents live in per-repository tmux sessions, attachable from
;; Ghostty or anywhere else; agents started this way are tracked by the
;; agent-sessions dashboard.  The implementation is agent-sessions-tmux.el in
;; ~/Stillness/Development/customize-agents (loaded by ai-sessions-config.el).
;;
;;   C-c o t   attach to the repository's session (C-u: plain ghostel shell),
;;             bound in keymaps.el
;;   C-c i i   new window running Claude Code in the session
;;   C-c i p   new window running pi in the session

(keymap-set rsr/ai-map "i" #'rsr/agent-tmux-claude)   ; C-c i i, see keymaps.el
(keymap-set rsr/ai-map "p" #'rsr/agent-tmux-pi)       ; C-c i p

(provide 'terminal-config)
;;; terminal-config.el ends here
