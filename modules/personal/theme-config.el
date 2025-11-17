;; Add the DIRECTORY containing the theme file to the load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") ;; Assumes this is the correct path

;; Now, load the theme by its symbol name
(load-theme 'timu-macos t)
;;(load-theme ../../themes/mac-os-theme.el t)



(use-package org-bullets
  :ensure t
  :hook (org-mode-hook . org-bullets-mode))
;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-medium t)

;;   ;; Softer "white" for main text
;;   (set-face-attribute 'default nil :foreground "#dddddd")

;;   ;; Optional: keep comments dim, but readable
;;   (custom-set-faces
;;    '(font-lock-comment-face ((t (:foreground "#928374"))))
;;    '(font-lock-string-face  ((t (:foreground "#b8bb26")))))
;; )
