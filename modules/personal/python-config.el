;;; python-config.el --- Python and C/C++ language configuration -*- lexical-binding: t; -*-

;;; Code:

;;; Python — pylsp
(add-hook 'python-mode-hook #'lsp-deferred)

;;; C / C++ — ccls (brew install ccls)
(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode) . lsp-deferred))

(provide 'python-config)
;;; python-config.el ends here
