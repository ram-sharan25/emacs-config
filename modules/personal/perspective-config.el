;;; perspective-config.el --- Named workspace perspectives -*- lexical-binding: t; -*-

;;; Code:

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :init
  (persp-mode))

(use-package persp-projectile
  :ensure t
  :demand t
  :after (perspective projectile)
  :bind (:map projectile-mode-map
         ("C-c p p" . projectile-persp-switch-project)))

;;; --- Commands ---

(defun rsr/open-project-in-new-frame ()
  "Open a projectile project in a new frame with its own perspective."
  (interactive)
  (condition-case err
      (progn
        (select-frame (make-frame))
        ;; longer delay — lets treemacs/perspective hooks settle in new frame
        (run-with-idle-timer 0.5 nil #'call-interactively #'projectile-persp-switch-project))
    (error (message "rsr/open-project-in-new-frame: %s" (error-message-string err)))))

;;; Keybindings
(global-set-key (kbd "C-c C-p F") #'rsr/open-project-in-new-frame)

(provide 'perspective-config)
;;; perspective-config.el ends here
