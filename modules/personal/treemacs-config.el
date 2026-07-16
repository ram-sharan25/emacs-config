;;; treemacs-config.el --- File tree sidebar -*- lexical-binding: t; -*-

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :after perspective
  :config
  (defun rsr/treemacs-toggle-current-project ()
    "Toggle Treemacs, preferring the current project.

When showing Treemacs, display the current project *exclusively* in the
current perspective's Treemacs workspace.  This prevents opening a stale
persisted project (e.g. Brain) when you're working in another project.

If the current buffer is not part of a project, fall back to plain Treemacs."
    (interactive)
    (require 'treemacs)
    (let ((origin (current-buffer)))
      (if (eq (treemacs-current-visibility) 'visible)
          (treemacs)
        ;; Open treemacs first so its window/buffer exists.
        (treemacs)
        (condition-case err
            (with-current-buffer origin
              (treemacs-add-and-display-current-project-exclusively))
          (error
           (message "Treemacs: %s" (error-message-string err))))

        ;; One-shot reveal of the current file without enabling follow-mode.
        ;; Use an idle timer so treemacs has time to finish rendering its DOM.
        (when (buffer-file-name origin)
          (run-with-idle-timer
           0.5 nil
           (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (ignore-errors (save-selected-window (treemacs-find-file))))))
           origin)))))

  (setq treemacs-position              'right
        treemacs-width                 35
        treemacs-show-hidden-files     t
        treemacs-is-never-other-window t
        treemacs-file-event-delay      500
        treemacs-litter-directories    '("/node_modules" "/.venv" "/.cask"))

  ;; Follow mode: highlight the current file in the sidebar as you switch
  ;; buffers.  Previously disabled due to timer errors before Treemacs' DOM
  ;; was ready; re-enabled to test whether newer Treemacs handles it cleanly.
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)           ;; live filesystem updates
  (treemacs-project-follow-mode nil)    ;; disabled — causes sentinel errors on non-project buffers
  (treemacs-fringe-indicator-mode 'always)

  ;; Avoid treemacs imenu index creation (currently errors with `arrayp, nil`).
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (setq-local imenu-create-index-function (lambda () nil))))

  ;; Git integration — deferred for performance
  (pcase (executable-find "git")
    (`nil (message "Treemacs: git not found, disabling git integration."))
    (_ (treemacs-git-mode 'deferred)))

  :bind
  (("M-0"     . treemacs-select-window)
   ("C-x t t" . rsr/treemacs-toggle-current-project)
   ("C-x t 1" . treemacs-delete-other-windows)
   ("C-x t f" . treemacs-find-file)
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
