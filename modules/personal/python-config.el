;;; python-config.el --- Python and C/C++ language configuration -*- lexical-binding: t; -*-

;;; Code:

;;; Python — pylsp
;; python-base-mode-hook covers python-mode and python-ts-mode; files open
;; in the ts-mode (treesit-config.el), so a hook on python-mode never fires.
(add-hook 'python-base-mode-hook #'lsp-deferred)

;;; C / C++ — ccls (brew install ccls)
(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode) . lsp-deferred))

(provide 'python-config)
;;; python-config.el ends here
