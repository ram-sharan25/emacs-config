;;(add-to-list 'custom-theme-load-path "~/./.emacs.d/theme/emacs-color-theme-solarized")


(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t)

  ;; Softer "white" for main text
  (set-face-attribute 'default nil :foreground "#dddddd")

  ;; Optional: keep comments dim, but readable
  (custom-set-faces
   '(font-lock-comment-face ((t (:foreground "#928374"))))
   '(font-lock-string-face  ((t (:foreground "#b8bb26")))))
)
