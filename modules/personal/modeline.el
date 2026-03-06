;;; modeline.el --- Doom modeline configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init
  ;; custom layout: git branch before filename on left
  (doom-modeline-def-modeline 'rsr/modeline
    '(bar vcs buffer-info remote-host matches selection-info)
    '(misc-info persp-name major-mode process time))
  (add-hook 'doom-modeline-mode-hook
            (lambda () (doom-modeline-set-modeline 'rsr/modeline t)))

  (setq doom-modeline-buffer-encoding nil)         ;; hide UTF-8/LF
  (setq doom-modeline-percent-position nil)        ;; hide All/Top/Bot
  (setq doom-modeline-minor-modes nil)             ;; hide minor mode indicators
  (setq doom-modeline-vcs-max-length 20)           ;; truncate long branch names
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project) ;; project/file.el

  ;; suppress org-element warnings in org-agenda (Emacs 30 compatibility)
  (add-to-list 'warning-suppress-types '(org-element))

  (setq display-time-default-load-average nil)  ;; hide load average
  (setq display-time-format "%a %m/%d %I:%M %p")
  (display-time-mode 1)

  (doom-modeline-mode 1))

(provide 'modeline)
;;; modeline.el ends here
