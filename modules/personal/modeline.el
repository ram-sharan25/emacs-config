;;; modeline.el --- Doom modeline configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-format
        '((bar vcs-branch buffer-info remote-host matches selection parens)
          (misc-info persp-name major-mode process time)))

  (setq doom-modeline-buffer-encoding nil)         ;; hide UTF-8/LF
  (setq doom-modeline-display-buffer-size nil)     ;; hide buffer size
  (setq doom-modeline-display-buffer-position nil) ;; hide L:N
  (setq doom-modeline-percent-position nil)        ;; hide All/Top/Bot
  (setq doom-modeline-position-line-format nil)    ;; hide L31
  (setq doom-modeline-position-column-line-format nil) ;; hide col:line
  (setq doom-modeline-minor-modes nil)             ;; hide minor mode indicators
  (setq doom-modeline-vcs-max-length 20)           ;; truncate long branch names
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project) ;; project/file.el

  (setq display-time-default-load-average nil)  ;; hide load average
  (setq display-time-format "%a %m/%d %I:%M %p")
  (display-time-mode 1)

  (doom-modeline-mode 1))

(provide 'modeline)
;;; modeline.el ends here
