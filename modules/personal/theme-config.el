;;; theme-config.el --- Theme and visual appearance -*- lexical-binding: t; -*-

;;; Code:

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

(custom-set-faces
 ;; org scaffolding — small and dim
 '(org-drawer          ((t (:foreground "#b42cbe" :height 0.8))))
 '(org-meta-line       ((t (:inherit font-lock-comment-face :height 0.8))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :height 0.8))))
 '(org-property-value  ((t (:height 0.8))))

 ;; src/quote blocks — subtle dark background
 '(org-block            ((t (:background "#2B2B2B" :family "Fira Code" :extend t))))
 '(org-block-begin-line ((t (:inherit org-meta-line :background "#2B2B2B" :extend t))))
 '(org-block-end-line   ((t (:inherit org-meta-line :background "#2B2B2B" :extend t))))
 '(org-quote            ((t (:inherit org-block :slant italic
                             :foreground "#DEF7E0" :background "#4A5057"))))

 ;; modeline — distinct colors for git branch vs filename
 '(doom-modeline-buffer-file         ((t (:foreground "#f0dfaf" :weight bold)))) ;; yellow — filename
 '(doom-modeline-buffer-path         ((t (:foreground "#bfbfbf"))))               ;; dim — path prefix
 '(doom-modeline-vcs-info            ((t (:foreground "#7f9f7f" :weight bold)))) ;; green — clean branch
 '(doom-modeline-vcs-warning         ((t (:foreground "#dfaf8f" :weight bold)))) ;; orange — modified
 '(doom-modeline-vcs-error           ((t (:foreground "#cc9393" :weight bold)))) ;; red — error

 ;; org-agenda — scheduled, deadline, and date colors
 '(org-agenda-date-today      ((t (:foreground "#f0dfaf" :weight bold :underline t)))) ;; today header — yellow
 '(org-agenda-structure       ((t (:foreground "#f0dfaf" :weight bold))))               ;; group headers — yellow
 '(org-agenda-date            ((t (:foreground "#8cd0d3"))))                            ;; other dates — teal
 '(org-scheduled-today        ((t (:foreground "#7cb8bb"))))                            ;; 1x/2x repeat — cyan
 '(org-scheduled-previously   ((t (:foreground "#cc9393"))))                            ;; overdue scheduled — red
 '(org-upcoming-deadline      ((t (:foreground "#dfaf8f" :weight bold))))               ;; In 3d. — orange
 '(org-warning                ((t (:foreground "#cc9393" :weight bold))))               ;; 2d ago / past deadline — red

 ;; magit diff — clearer green/red against zenburn background
 '(magit-diff-added             ((t (:background "#2d4f2d" :foreground "#9fc99f"))))
 '(magit-diff-removed           ((t (:background "#4f2d2d" :foreground "#cc9393"))))
 '(magit-diff-added-highlight   ((t (:background "#3a6b3a" :foreground "#c3e8c3"))))
 '(magit-diff-removed-highlight ((t (:background "#6b3a3a" :foreground "#e8c3c3"))))
 '(magit-diff-hunk-heading           ((t (:background "#3f3f3f" :foreground "#afafaf"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#4f4f4f" :foreground "#dfdfdf")))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'theme-config)
;;; theme-config.el ends here
