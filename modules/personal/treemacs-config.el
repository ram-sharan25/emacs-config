(use-package treemacs
  :after (treemacs perspective)
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  ;; --- Key Settings ---
  (setq treemacs-position 'right
	treemacs-width 35
	treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
	treemacs-show-hidden-files t
	treemacs-is-never-other-window t
	treemacs-file-event-delay 500) ; Lowered delay for faster updates

  ;; --- Enable Essential Modes ---
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)

  ;; --- Git Integration ---
  (pcase (executable-find "git")
    (`nil (message "Treemacs: Git not found, disabling git integration."))
    (_ (treemacs-git-mode 'deferred)))
  :bind
  ;; Correct syntax for global keybindings
  (("M-0"       . treemacs-select-window)
   ("C-x t t"   . treemacs)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t d"   . treemacs-select-directory)))

;; --- Treemacs Integrations ---

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-perspective
  :after (treemacs perspective)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))
