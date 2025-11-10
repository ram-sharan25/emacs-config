;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WEB DEVELOPMENT CONFIGURATION (JS/TS/React)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core language modes
(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode) ("\\.jsx\\'" . rjsx-mode))
  :config
  (setq js2-basic-offset 2 sgml-basic-offset 2)
  (with-eval-after-load 'rjsx-mode
    (define-key rjsx-mode-map "<" nil)
    (define-key rjsx-mode-map ">" nil)))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2))

;; Development tools
(use-package add-node-modules-path
  :hook ((rjsx-mode . add-node-modules-path)
   (typescript-mode . add-node-modules-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML AND .ENV FILE SUPPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A. Install Major Modes for Syntax Highlighting
;; This ensures .yml and .env files get proper colors and indentation.
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package dotenv-mode
  :ensure t
  :mode "\\.env\\'")

;; React snippets and helpers
(use-package react-snippets :after yasnippet)


(defun my-dev-mode-setup ()
  "Unified setup for all web development modes."
  ;; Enable major tools
  (lsp-deferred)
  (emmet-mode)
  (add-node-modules-path)

  ;; Set up keybindings locally for this buffer
  (define-key (current-local-map) (kbd "M-.") #'lsp-find-definition)
  (define-key (current-local-map) (kbd "M-,") #'xref-pop-marker-stack)
  (define-key (current-local-map) (kbd "M-?") #'lsp-find-references)
  (define-key (current-local-map) (kbd "C-M-.") #'lsp-find-implementation)
  ;; Use LSP for formatting instead of a separate package
  (define-key (current-local-map) (kbd "M-F") #'lsp-format-buffer))



(add-hook 'rjsx-mode-hook #'my-dev-mode-setup-keys)
(add-hook 'typescript-mode-hook #'my-dev-mode-setup-keys)
(add-hook 'yaml-mode-hook #'my-dev-mode-setup-keys)

;; This line is crucial for modular configurations
(provide 'web-dev-config)
