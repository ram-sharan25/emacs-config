;;for solarized theme

(add-to-list 'custom-theme-load-path "~/./.emacs.d/theme/emacs-color-theme-solarized")
(load-theme 'solarized t)
(solarized-update-background-mode 'dark)
;(add-hook 'after-make-frame-functions
         ; (lambda (frame)
          ;  (solarized-update-background-mode 'dark)
          ;   ))
