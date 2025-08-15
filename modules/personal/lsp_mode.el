;; ;; Ensure Node.js LTS is used before starting LSP
;; (defun rsr/setup-nvm ()
;;   "Dynamically load the correct Node.js version from NVM, respecting .nvmrc."
;;   (when (or (derived-mode-p 'js-mode) (derived-mode-p 'typescript-mode)
;;	    (derived-mode-p 'js-jsx-mode) (derived-mode-p 'tsx-ts-mode)
;;	    (derived-mode-p 'web-mode))  ;; Restrict to relevant JS/TS/Node files
;;     (let* ((nvm-dir (or (getenv "NVM_DIR") (expand-file-name "~/.nvm")))
;;	   (nvm-sh (expand-file-name "nvm.sh" nvm-dir))
;;	   (project-root (locate-dominating-file default-directory ".nvmrc"))
;;	   (nvmrc-path (when project-root (expand-file-name ".nvmrc" project-root)))
;;	   (nvmrc-version (when (and nvmrc-path (file-exists-p nvmrc-path))
;;			    (string-trim
;;			     (shell-command-to-string
;;			      (concat "cat " (shell-quote-argument nvmrc-path))))))
;;	   (node-bin (when (file-exists-p nvm-sh)
;;		      (string-trim
;;		       (shell-command-to-string
;;			(format "export NVM_DIR=\"%s\" && . \"%s\" && %s && echo $NVM_BIN"
;;				nvm-dir
;;				nvm-sh
;;				(if nvmrc-version
;;				    (format "nvm use %s --silent" nvmrc-version)
;;				  "nvm use --silent")))))))
;;       (when (and node-bin (file-exists-p node-bin))
;;	(setenv "PATH" (concat node-bin path-separator (getenv "PATH")))
;;	(setq exec-path (cons node-bin exec-path))
;;	(message "✅ Using Node.js from: %s" node-bin)))))

;; (defun rsr/nvm-use ()
;;   "Manually trigger NVM setup for the current buffer."
;;   (interactive)
;;   (rsr/setup-nvm))

;; (defun rsr/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

;; (defun rsr/disable-lsp-for-emacs-lisp ()
;;   "Prevent LSP from running in Emacs Lisp files."
;;   (when (derived-mode-p 'emacs-lisp-mode)
;;     (setq-local lsp-mode nil)
;;     (message "LSP disabled for Emacs Lisp mode")))

;; (defun rsr/restart-lsp-for-new-project ()
;;   "Restart LSP when opening a file from a different project, except for Emacs Lisp."
;;   (when (and (buffer-file-name)
;;	     (project-current)
;;	     (not (derived-mode-p 'emacs-lisp-mode)))  ;; Skip Emacs Lisp files
;;     (let* ((proj-root (project-root (project-current)))
;;	   (lsp-session (lsp-session)))
;;       (unless (lsp--find-workspace lsp-session 'eslint proj-root)
;;	(message "🆕 Restarting LSP for new project: %s" proj-root)
;;	(lsp-restart-workspace)))))

;; (defvar rsr/custom-eslint-config
;;   '({
;;      "extends" ["eslint:recommended"]
;;      "rules" {
;;        "no-unused-vars" "warn"
;;        "no-console" "warn"
;;        "semi" ["error" "always"]
;;        "quotes" ["error" "single"]
;;      }
;;      "parserOptions" {
;;        "ecmaVersion" "latest"
;;        "sourceType" "module"
;;      }
;;      "env" {
;;        "browser" t
;;        "es2021" t
;;        "node" t
;;      }})
;;   "Custom ESLint configuration to use when no project config is found.")


;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp lsp-deferred)
;;   :hook ((lsp-mode . rsr/lsp-mode-setup)
;;	 (python-mode . lsp-deferred)  ;; Python support
;;	 (c-mode . lsp-deferred)       ;; C support
;;	 (c++-mode . lsp-deferred)     ;; C++ support
;;	 (js-jsx-mode . rsr/setup-nvm) ;; JS/TS with Node setup
;;	 (typescript-mode . rsr/setup-nvm)
;;	 (tsx-ts-mode . rsr/setup-nvm)
;;	 (web-mode . rsr/setup-nvm)    ;; For web-mode (JS/TS)
;;	 (emacs-lisp-mode . rsr/disable-lsp-for-emacs-lisp))  ;; Skip LSP for Emacs Lisp files
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")

;;   :custom
;;   ;; ESLint configuration
;;   (lsp-prettier-enabled t)
;;   (lsp-eslint-enable t)
;;   (lsp-eslint-auto-fix-on-save t)
;;   (lsp-eslint-package-manager "npm")
;;   (lsp-eslint-working-directories [])



;;   :config
;;   (lsp-enable-which-key-integration t)
;;   (setq lsp-eslint-run "onType")
;;   (setq lsp-eslint-validate ["javascript" "javascriptreact" "typescript" "typescriptreact"])

;;     ;; Function to restart ESLint server
;;   (defun rsr/restart-lsp-eslint-server ()
;;     "Restart the ESLint language server."
;;     (interactive)
;;     (lsp-disconnect)
;;     (sleep-for 0.5)
;;     (lsp-restart-workspace))

;;   (dolist (hook '(js-jsx-mode-hook
;;		typescript-mode-hook
;;		tsx-ts-mode-hook
;;		web-mode-hook
;;		js-ts-mode-hook
;;		typescript-ts-mode-hook))
;;   (add-hook hook #'rsr/setup-nvm))

;;   :bind (:map lsp-mode-map
;;	 ;; Core navigation
;;	 ("M-." . lsp-find-definition)
;;	 ("M-," . xref-go-back)
;;	 ("M-?" . lsp-find-references)
;;	 ;; Peek functionality
;;	 ("C-c p d" . lsp-ui-peek-find-definitions)
;;	 ("C-c p r" . lsp-ui-peek-find-references)))

;; (add-hook 'find-file-hook #'rsr/restart-lsp-for-new-project)
;; (add-hook 'emacs-lisp-mode-hook #'rsr/disable-lsp-for-emacs-lisp)

;; (defun rsr/lsp-prettier-setup ()
;;   "Enable Prettier as a formatter for LSP."
;;   (add-hook 'before-save-hook 'lsp-format-buffer nil t)
;;   (rsr/lsp-prettier-config))  ;; Call this function to load the config

;; (add-hook 'js-jsx-mode-hook 'rsr/lsp-prettier-setup)
;; (add-hook 'typescript-mode-hook 'rsr/lsp-prettier-setup)
;; (add-hook 'tsx-ts-mode-hook 'rsr/lsp-prettier-setup)

;; (use-package lsp-ui
;;   :ensure t
;;   :after (lsp-mode)
;;   :commands lsp-ui-doc-hide
;;   :bind (:map lsp-ui-mode-map
;;      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;      ([remap xref-find-references] . lsp-ui-peek-find-references)
;;      ("C-c u" . lsp-ui-imenu)
;;      ("C-c d" . lsp-ui-doc-show)
;;      ("C-c h" . lsp-ui-doc-hide)
;;      ("C-c p d" . lsp-ui-peek-find-definitions)  ; Custom keybinding for Peek Definitions
;;      ("C-c p r" . lsp-ui-peek-find-references)   ; Custom keybinding for Peek References
;;      ("C-c p i" . lsp-ui-peek-find-implementation))
;;   :init (setq lsp-ui-doc-enable nil
;;    lsp-ui-doc-frame 'bottom
;;    lsp-ui-doc-position 'right
;;    lsp-ui-peek-enable t
;;    lsp-ui-peek-enable t     ; Enable lsp-ui-peek
;;    lsp-ui-peek-show-directory t  ; Show directory of files in peek window
;;    lsp-ui-peek-max-width 100  ; You can adjust the width of the peek window as needed
;;    lsp-ui-peek-max-height 20 )
;;   :config
;;   (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
;;   (add-hook 'lsp-ui-doc-frame-mode-hook
;;	    (lambda ()
;;	      (setq-local cursor-type 'box)  ; Make the cursor visible in the doc frame
;;	      (use-local-map lsp-ui-doc-mode-map))))

;; (use-package lsp-treemacs
;;   :after lsp)

;; (use-package lsp-ivy)


;; (use-package flycheck
;;   :ensure t
;;   :hook ((js-jsx-mode . flycheck-mode)
;;	 (typescript-mode . flycheck-mode)
;;	 (web-mode . flycheck-mode))  ;; Add web-mode if necessary
;;   :init
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change)))

;; ;; Highlight errors with a red underline
;; (custom-set-faces
;;  '(flycheck-error ((t (:underline "red" :weight bold :foreground "red")))))

;; ;; Highlight warnings with a yellow underline
;; (custom-set-faces
;;  '(flycheck-warning ((t (:underline "yellow" :weight bold :foreground "yellow")))))

;; ;; Optionally, you can also set the colors for info level
;; (custom-set-faces
;;  '(flycheck-info ((t (:underline "green" :weight bold :foreground "green")))))



;; (add-hook 'typescript-mode-hook 'prettier-rc-mode)
;; (add-hook 'js2-mode-hook 'prettier-rc-mode)
;; (add-hook 'web-mode-hook 'prettier-rc-mode)

;; ;; (global-set-key (kbd "C-p p r ") 'prettier-prettify)
;; ;; (global-set-key (kbd "M-p r") 'prettier-rc)

;; ;;lsp-mode ends here
