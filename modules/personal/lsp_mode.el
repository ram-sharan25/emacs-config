;; Ensure Node.js LTS is used before starting LSP
(defun rsr/setup-nvm ()
  "Dynamically load the correct Node.js version from NVM, respecting .nvmrc."
  (let* ((nvm-dir (or (getenv "NVM_DIR") (expand-file-name "~/.nvm")))
	 (nvm-sh (expand-file-name "nvm.sh" nvm-dir))
	 (project-root (locate-dominating-file default-directory ".nvmrc"))
	 (nvmrc-path (when project-root (expand-file-name ".nvmrc" project-root)))
	 (nvmrc-version (when (and nvmrc-path (file-exists-p nvmrc-path))
			 (string-trim
			  (shell-command-to-string
			   (concat "cat " (shell-quote-argument nvmrc-path))))))
	 (node-bin (when (file-exists-p nvm-sh)
		    (string-trim
		     (shell-command-to-string
		      (format "export NVM_DIR=\"%s\" && . \"%s\" && %s && echo $NVM_BIN"
			     nvm-dir
			     nvm-sh
			     (if nvmrc-version
				 (format "nvm use %s --silent" nvmrc-version)
			       "nvm use --silent")))))))
    (when (and node-bin (file-exists-p node-bin))
      (setenv "PATH" (concat node-bin path-separator (getenv "PATH")))
      (setq exec-path (cons node-bin exec-path))
      (message "✅ Using Node.js from: %s" node-bin))))

;; Run this function automatically when opening a JS/TS file
(add-hook 'js-mode-hook #'rsr/setup-nvm)
(add-hook 'typescript-mode-hook #'rsr/setup-nvm)
(add-hook 'tsx-ts-mode-hook #'rsr/setup-nvm)
(add-hook 'web-mode-hook #'rsr/setup-nvm)  ; If you use web-mode for JS/TS

(defun rsr/nvm-use ()
  "Manually trigger NVM setup for the current buffer."
  (interactive)
  (rsr/setup-nvm))

;; (setenv "PATH" (concat (getenv "PATH") ":/Users/rrimal/.nvm/versions/node/v22.8.0/bin"))
;; (setq exec-path (append exec-path '("/Users/rrimal/.nvm/versions/node/v22.8.0/bin")))

(defun rsr/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumbmode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . rsr/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)
