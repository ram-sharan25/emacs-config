;;; python-config.el --- Python and C/C++ LSP configuration -*- lexical-binding: t; -*-

(with-eval-after-load 'lsp-mode
  (setq lsp-pyright-executable "~/.emacs.d/pyvenv/bin/pylsp"))

;; --- Python LSP ---
;; Install server: pip install 'python-lsp-server[all]'
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

;; --- C/C++ LSP ---
;; Install server: brew install ccls
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode) . (lambda ()
                               (require 'ccls)
                               (lsp-deferred))))

(provide 'python-config)
