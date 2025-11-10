;; LSP MODE (Core)
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (;; JavaScript and TypeScript
	 (js-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred)
	 ;; JSON
	 (json-mode . lsp-deferred)
	 ;; HTML, CSS, SCSS, Less
	 (html-mode . lsp-deferred)
	 (css-mode . lsp-deferred)
	 (scss-mode . lsp-deferred)
	 (less-mode . lsp-deferred)
	 ;; YAML
	 (yaml-mode . lsp-deferred))
  :init
  ;; This sets `lsp-mode` to use `flycheck` for displaying errors
  (setq lsp-diagnostics-provider :flycheck)
  :config
  ;; This enables ESLint as a linter within lsp-mode
  (setq lsp-eslint-enable t)
  ;;
  :custom
  (lsp-language-id-configuration '((rjsx-mode . "javascriptreact")))
  (lsp-typescript-suggest-auto-imports t)
  (lsp-typescript-format-enable t)
  (lsp-typescript-preferences-quote-style "single"))


;; LSP UI (UI Enhancements)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :config
  ;; Customizations for the UI
  (setq lsp-ui-doc-position 'at-point) ; Show documentation popup at the cursor
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t))
