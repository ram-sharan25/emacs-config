;;; lsp-mode-config.el --- LSP general configuration -*- lexical-binding: t; -*-

;;; Code:

;;; Performance — increase process read buffer for LSP throughput
(setq read-process-output-max (* 1024 1024))

;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-headerline-breadcrumb-enable nil
        lsp-document-sync-method 2        ;; incremental — sends diffs only, not full buffer
        lsp-diagnostics-provider :flymake) ;; flymake handles large error counts better than flycheck
  (define-key lsp-mode-map (kbd "C-M-,") #'xref-go-forward))

;;; lsp-ui — inline diagnostics beside code (only renders visible lines, won't hang)
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t           ;; show errors/warnings beside the line
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil     ;; hover info in sideline is noisy — use C-h . instead
        lsp-ui-sideline-delay 0.5
        lsp-ui-doc-enable nil))            ;; disable floating doc popup — eldoc handles this

(provide 'lsp-mode-config)
;;; lsp-mode-config.el ends here
