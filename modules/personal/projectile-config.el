(use-package projectile
  :ensure t
  :init
  ;; This line enables projectile globally when Emacs starts.
  (projectile-mode +1)
  :config
  ;; These keybindings will now be set correctly after projectile is loaded.
  ;; The `s-` prefix is for the Super key (Windows or Command keys).
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
