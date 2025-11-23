(use-package nerd-icons
  :ensure t
  :config
  ;; Initialize Nerd Icons
  (nerd-icons-install-fonts) ; Only needed once to install the font files
  )

(use-package doom-modeline
  :ensure t
  :init
  ;; This defines the segments shown on the LEFT and RIGHT of the modeline.
  (setq doom-modeline-format
        '(
          ;; LEFT side definition:
          (bar buffer-info remote-host vcs-branch matches selection parens)

          ;; RIGHT side definition:
          (misc-info persp-name major-mode process time)
          )
        )

  ;; Configuration for elements you want to remove:
  ;; Remove Line Number (The 'buffer-position' segment is responsible for L:100/All)
  (setq doom-modeline-buffer-encoding nil)     ; Removes UTF-8 and LF
  (setq doom-modeline-display-buffer-size nil) ; Removes 'All' (Buffer Size)
  (setq doom-modeline-display-buffer-position nil) ; Removes L:100

  ;; Remove 1.14 (System Load Average)
  (setq display-time-default-load-average nil)

  ;; Ensure clock is enabled and formatted (as per our previous chat)
  (display-time-mode 1)
  (setq display-time-format "%a %m/%d %I:%M %p")

  (doom-modeline-mode 1))
