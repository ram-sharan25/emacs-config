;;python3 configs
;; Load support for Python code blocks


(require 'org)
(require 'ob-python)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))


(setq org-src-fontify-natively t)  ;; Enable syntax highlighting for source code blocks

(global-font-lock-mode 1)

(set-face-attribute 'default nil :height 160)


(with-eval-after-load 'lsp-mode
  ;; Point to the LSP server executable inside your new virtual environment
  (setq lsp-pyright-executable "~/.emacs.d/pyvenv/bin/pylsp"))

;; --- Python Support ---
;; To install the server, run: pip install 'python-lsp-server[all]'
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			  (require 'lsp-pyright)
			  (lsp-deferred))))

;; --- C/C++ Support ---
;; To install the server (e.g., on macOS): brew install ccls
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode) . (lambda ()
			       (require 'ccls)
			       (lsp-deferred))))


(provide 'lsp-languages)
