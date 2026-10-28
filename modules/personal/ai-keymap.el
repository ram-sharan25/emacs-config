;;; ai-keymap.el --- C-c i: AI and agents -*- lexical-binding: t -*-

;;; Commentary:
;; One prefix for every AI and agent command, following the Emacs convention
;; that C-c + letter belongs to the user.  Loaded before the modules that fill
;; it (directory order: "ai-keymap" sorts first), so they can bind into it.
;;
;;   C-c i a A s c q m f F + - = t T C   gptel   (gptel-config.el)
;;   C-c i n …                           NotebookLM (notebooklm.el)
;;   C-c i p                             pi in a tmux window (terminal-config.el)
;;   C-c i i                             Claude Code in a tmux window (terminal-config.el)
;;   C-c i d / C-c i r                   agent dashboard / send region (ai-sessions-config.el)
;;
;; See Plans/emacs-d/keybinding-redesign for the wider scheme.

;;; Code:

(defvar-keymap rsr/nlm-map
  :doc "NotebookLM commands, under `rsr/ai-map'."
  :prefix 'rsr/nlm-map)

(defvar-keymap rsr/ai-map
  :doc "AI and agent commands (C-c i)."
  :prefix 'rsr/ai-map
  "n" 'rsr/nlm-map)

(keymap-global-set "C-c i" 'rsr/ai-map)

;; Until the leader is retired (keybinding-redesign task), keep the old
;; M-m a / M-m n paths, and so evil SPC a / SPC n, pointing at the same maps.
(keymap-set rsr/global-prefix-map "a" 'rsr/ai-map)
(keymap-set rsr/global-prefix-map "n" 'rsr/nlm-map)

(provide 'ai-keymap)
;;; ai-keymap.el ends here
