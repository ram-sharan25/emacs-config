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
  (setq lsp-format-buffer-on-save nil)
  ;; This sets the default for all buffers to use spaces instead of tabs.
  (setq-default indent-tabs-mode nil)

  ;; This sets the visual width of a tab to 2 spaces. This is useful
  ;; for files that might still contain tab characters.
  (setq-default tab-width 2)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
  ("C-c v d" . lsp-ui-doc-show))
  :config
  ;; DOCUMENTATION SETTINGS
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-frame-size '(0.3 . 1))

  ;; OTHER UI FEATURES
  (setq lsp-ui-peek-enable t
  lsp-ui-imenu-enable t
  lsp-ui-sideline-enable t
  lsp-ui-sideline-delay 0.1))

(provide 'lsp-config)
