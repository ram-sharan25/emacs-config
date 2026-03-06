;;; treemacs-config.el --- File tree sidebar -*- lexical-binding: t; -*-

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :after perspective
  :config
  (setq treemacs-position              'right
        treemacs-width                 35
        treemacs-show-hidden-files     t
        treemacs-is-never-other-window t
        treemacs-file-event-delay      500
        treemacs-litter-directories    '("/node_modules" "/.venv" "/.cask"))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)

  ;; Git integration — deferred for performance
  (pcase (executable-find "git")
    (`nil (message "Treemacs: git not found, disabling git integration."))
    (_ (treemacs-git-mode 'deferred)))

  :bind
  (("M-0"     . treemacs-select-window)
   ("C-x t t" . treemacs)
   ("C-x t 1" . treemacs-delete-other-windows)
   ("C-x t d" . treemacs-select-directory)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :ensure t
  :after treemacs
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-perspective
  :ensure t
  :after (treemacs perspective)
  :config (treemacs-set-scope-type 'Perspectives))

(provide 'treemacs-config)
;;; treemacs-config.el ends here
