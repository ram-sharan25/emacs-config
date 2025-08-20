(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c v")
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-project-root-patterns
	'("package.json" "tsconfig.json" "jsconfig.json" ".git" ".project" ".projectile"))

  ;; PERFORMANCE
  (setq lsp-completion-provider :capf
	lsp-idle-delay 0.1
	lsp-document-sync-method 'incremental
	lsp-response-timeout 10
	lsp-enable-file-watchers t
	lsp-file-watch-threshold 5000)

  ;; INTEGRATIONS
  (setq lsp-eslint-enable t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
	("C-c v d" . lsp-ui-doc-show))
  :config
  ;; DOCUMENTATION SETTINGS
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-frame-size '(0.3 . 0.3))

  ;; OTHER UI FEATURES
  (setq lsp-ui-peek-enable t
	lsp-ui-imenu-enable t
	lsp-ui-sideline-enable t
	lsp-ui-sideline-delay 0.1))

(provide 'lsp-config)
