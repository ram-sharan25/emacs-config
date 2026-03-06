;;; projectile-config.el --- Project navigation -*- lexical-binding: t; -*-

;;; Code:

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
         ("s-p"   . projectile-command-map)
         ("C-c p" . projectile-command-map)))

(provide 'projectile-config)
;;; projectile-config.el ends here
