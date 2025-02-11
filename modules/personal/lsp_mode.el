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
(add-hook 'js-jsx-mode-hook #'rsr/setup-nvm)
(add-hook 'typescript-mode-hook #'rsr/setup-nvm)
(add-hook 'tsx-ts-mode-hook #'rsr/setup-nvm)
(add-hook 'web-mode-hook #'rsr/setup-nvm)  ; If you use web-mode for JS/TS

(defun rsr/nvm-use ()
  "Manually trigger NVM setup for the current buffer."
  (interactive)
  (rsr/setup-nvm))



(defun rsr/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . rsr/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :bind (:map lsp-mode-map
	 ;; Core navigation
	 ("M-." . lsp-find-definition)
	 ("M-," . xref-go-back)
	 ("M-?" . lsp-find-references)
	 ;; Peek functionality
	 ("C-c p d" . lsp-ui-peek-find-definitions)
	 ("C-c p r" . lsp-ui-peek-find-references)))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-doc-hide
  :bind (:map lsp-ui-mode-map
     ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
     ([remap xref-find-references] . lsp-ui-peek-find-references)
     ("C-c u" . lsp-ui-imenu)
     ("C-c d" . lsp-ui-doc-show)
     ("C-c h" . lsp-ui-doc-hide)
     ("C-c p d" . lsp-ui-peek-find-definitions)  ; Custom keybinding for Peek Definitions
     ("C-c p r" . lsp-ui-peek-find-references)   ; Custom keybinding for Peek References
     ("C-c p i" . lsp-ui-peek-find-implementation))
  :init (setq lsp-ui-doc-enable nil
   lsp-ui-doc-frame 'bottom
   lsp-ui-doc-position 'right
   lsp-ui-peek-enable t
   lsp-ui-peek-enable t     ; Enable lsp-ui-peek
   lsp-ui-peek-show-directory t  ; Show directory of files in peek window
   lsp-ui-peek-max-width 100  ; You can adjust the width of the peek window as needed
   lsp-ui-peek-max-height 20 )
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
  (add-hook 'lsp-ui-doc-frame-mode-hook
	    (lambda ()
	      (setq-local cursor-type 'box)  ; Make the cursor visible in the doc frame
	      (use-local-map lsp-ui-doc-mode-map))))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;;end of lsp_mode.el
