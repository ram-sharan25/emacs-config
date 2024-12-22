;;; Core LSP packages
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
	      ("M-/" . lsp-find-references)
	      ("M-." . lsp-find-definition))
  :hook
  ((python-mode . lsp-deferred)
   (c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (typescript-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config (setq lsp-disabled-clients '(deno-ls))
	  (setq lsp-diagnostics-provider :flymake))



(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package lsp-pyright
  :ensure t)

(use-package nvm
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  ;; Enable or disable lsp-ui features
  (setq lsp-ui-doc-enable t)           ;; Show documentation on hover
  (setq lsp-ui-doc-show-with-cursor t) ;; Only show documentation when the cursor is over the symbol
  (setq lsp-ui-doc-position 'top)      ;; Position of the documentation window (top, bottom, or at point)

  ;; Customize peek window behavior
  (setq lsp-ui-peek-enable t)          ;; Enable peek window (find definition, find references)
  (setq lsp-ui-peek-show-directory t)  ;; Show directory info in the peek window
  (setq lsp-ui-peek-list-width 60)     ;; Width of the peek window


  ;; Customize the flycheck diagnostics display
  (setq lsp-ui-sideline-enable t)      ;; Enable the inline diagnostic display
  (setq lsp-ui-sideline-show-diagnostics t) ;; Show diagnostics inline
  (setq lsp-ui-sideline-show-hover t) ;; Show hover information in the sideline
  (setq lsp-ui-sideline-show-symbol t) ;; Show symbol in the sideline

  ;; Optional: Customize the size of the documentation pop-up
  (setq lsp-ui-doc-max-width 80)       ;; Max width of the documentation pop-up
  (setq lsp-ui-doc-max-height 15)      ;; Max height of the documentation pop-up

  (global-set-key (kbd "M-?") 'lsp-ui-peek-find-definitions))  ;; Peek definition





;; Completion
(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (global-company-mode t))
