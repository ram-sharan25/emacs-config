;;; magit_config.el --- Git interface via magit -*- lexical-binding: t; -*-

;;; Code:

(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-diff-hide-trailing-cr-characters t)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :custom
  (magit-diff-refine-hunk t)   ;; highlight changed words within a hunk
  :bind
  ("C-x g" . magit-status))

;; diff-hl — show git changes in fringe while editing (like VS Code git gutter)
(use-package diff-hl
  :ensure t
  :hook
  (prog-mode    . diff-hl-mode)
  (org-mode     . diff-hl-mode)
  (dired-mode   . diff-hl-dired-mode)
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'magit_config)
;;; magit_config.el ends here
