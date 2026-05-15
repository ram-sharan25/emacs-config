;;; claude-code-ide-config.el --- Claude Code IDE integration via MCP -*- lexical-binding: t -*-

;;; Commentary:
;; Runs Claude Code CLI inside Emacs with bidirectional MCP integration.
;; Nothing loads until C-c C-' is explicitly pressed.

;;; Code:

(use-package eat
  :ensure t
  :defer t)

;; Load autoloads only — no package code, no MCP server, no hooks at startup
(load (expand-file-name "elpa/claude-code-ide/claude-code-ide-autoloads"
                        user-emacs-directory) nil t)

;; Set backend before the package body loads — avoids "vterm not installed" error
;; if claude-code-ide is triggered outside of rsr/claude-code-ide-menu
(setq claude-code-ide-terminal-backend 'eat)

(defun rsr/claude-code-ide-menu ()
  "Load claude-code-ide on demand then open the menu.
Defers all MCP server startup and hook setup until explicitly invoked."
  (interactive)
  (require 'claude-code-ide)
  (require 'claude-code-ide-mcp)
  ;; Configure MCP tools only on first load
  (unless (boundp 'rsr/claude-code-ide--initialized)
    (claude-code-ide-emacs-tools-setup)
    (defvar rsr/claude-code-ide--initialized t))
  (claude-code-ide-menu))

(global-set-key (kbd "C-c C-'") #'rsr/claude-code-ide-menu)

(provide 'claude-code-ide-config)
;;; claude-code-ide-config.el ends here
