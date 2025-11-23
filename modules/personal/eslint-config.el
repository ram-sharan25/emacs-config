;; (use-package flymake-eslint
;;   :ensure t
;;   :config
;;   (setq flymake-eslint-prefer-json-diagnostics t)

;;   (defun my/use-local-eslint ()
;;     "Use local project's `node_modules/.bin/eslint` if available."
;;     (interactive)
;;     (let* ((root (or (my/get-project-root) ;; Ensure project root is found
;;		     (locate-dominating-file (buffer-file-name) "node_modules")))
;;	   (eslint (and root (expand-file-name "node_modules/.bin/eslint" root))))
;;       (when (and eslint (file-executable-p eslint))
;;	(setq-local flymake-eslint-executable-name eslint)
;;	(message "Using local ESLint: %s" eslint)
;;	(flymake-eslint-enable))))

;;   (defun my/configure-eslint-with-flymake ()
;;     "Enable Flymake ESLint in relevant modes."
;;     (when (derived-mode-p 'js-ts-mode 'tsx-ts-mode 'typescript-ts-mode 'typescriptreact-mode)
;;       (my/use-local-eslint)))

;;   ;; Ensure it runs when entering JS/TS modes
;;   (add-hook 'js-ts-mode-hook #'my/use-local-eslint)
;;   (add-hook 'tsx-ts-mode-hook #'my/use-local-eslint)
;;   (add-hook 'typescript-ts-mode-hook #'my/use-local-eslint)
;;   (add-hook 'eglot-managed-mode-hook #'my/use-local-eslint))
