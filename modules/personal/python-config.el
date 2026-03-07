;;; python-config.el --- Python and C/C++ LSP -*- lexical-binding: t; -*-

;;; Code:

;;; LSP performance — prevents stuttering on large files
(setq read-process-output-max (* 1024 1024))
(with-eval-after-load 'lsp-mode
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-headerline-breadcrumb-enable nil))

;;; Python — pylsp (~/.emacs.d/pyvenv/bin/pylsp)
(add-hook 'python-mode-hook #'lsp-deferred)

;;; C / C++ — ccls (brew install ccls)
(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode) . lsp-deferred))

;;; xref forward navigation (M-, is back, C-M-, is forward)
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-M-,") #'xref-go-forward))

(provide 'python-config)
;;; python-config.el ends here
