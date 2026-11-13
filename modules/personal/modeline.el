;;; modeline.el --- Doom modeline configuration -*- lexical-binding: t; -*-

;;; Code:

;; suppress org-element warnings early — before any org or doom-modeline loads
(require 'warnings)
(add-to-list 'warning-suppress-types '(org-element))
(add-to-list 'warning-suppress-log-types '(org-element))

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-vcs-max-length 20)
  ;; Just the file name; the path lives in the window's own buffer.
  (setq doom-modeline-buffer-file-name-style 'file-name)
  ;; The live analogue clock regenerates an SVG every minute.  The macOS Emacs
  ;; port renders each SVG through WebKit and leaks a WebContent process, which
  ;; eventually exhausts memory and stalls WindowServer.  Keep the normal
  ;; font-based clock icon and textual time instead.
  (setq doom-modeline-time-analogue-clock nil)
  ;; disable org-clock injecting full task into global-mode-string
  (setq org-clock-clocked-in-display nil)
  (setq display-time-default-load-average nil)
  (setq display-time-format "%a %m/%d %I:%M %p")
  (display-time-mode 1)
  (doom-modeline-mode 1)
  :config
  ;; doom-modeline looks every vcs icon up in `devicon', but the worktree icon
  ;; is `nf-cod-worktree'; the lookup errors and the branch never shows in a
  ;; git worktree.  Pick the set from the icon's prefix.
  (define-advice doom-modeline-vcs-icon (:override (icon &optional unicode text face) rsr/icon-set)
    (doom-modeline-icon (if (and icon (string-prefix-p "nf-cod-" icon)) 'codicon 'devicon)
                        (and doom-modeline-vcs-icon icon)
                        unicode text :face face))

  (defun rsr/modeline-org-clock ()
    "Return a short org-clock string: truncated task name + elapsed time."
    (when (and (fboundp 'org-clocking-p) (org-clocking-p))
      (let* ((task  (substring-no-properties org-clock-heading))
             (short (if (> (length task) 22)
                        (concat (substring task 0 22) "…")
                      task))
             (time  (org-duration-from-minutes
                     (floor (org-time-convert-to-integer
                             (time-since org-clock-start-time))
                            60))))
        (propertize (format " ⏱ %s %s " short time)
                    'face '(:foreground "#8ab4c9")))))

  ;; Global, like time and persp-name: shown once, in the selected window,
  ;; rather than repeated in every window's mode line.
  (doom-modeline-def-segment rsr/clock
    "Org clock: truncated task + elapsed time."
    (or (and (doom-modeline--segment-visible 'rsr/clock)
             (rsr/modeline-org-clock))
        ""))

  ;; doom's `major-mode' segment without the text-scale amount it appends.
  (doom-modeline-def-segment rsr/major-mode
    "The major mode name."
    (concat
     (doom-modeline-spc)
     (propertize (format-mode-line mode-name)
                 'face (doom-modeline-face 'doom-modeline-buffer-major-mode)
                 'mouse-face 'doom-modeline-highlight
                 'local-map mode-line-major-mode-keymap)
     (doom-modeline-spc)))

  ;; Branch, file name; clocked task, workspace, file type, time.
  (doom-modeline-def-modeline 'rsr/default
    '(vcs buffer-info-simple)
    '(rsr/clock persp-name rsr/major-mode time))

  (doom-modeline-set-modeline 'rsr/default t)

  ;; `doom-modeline-mode' (in :init) resets every open buffer to its `main'
  ;; layout; put the open ones back on ours.  Modes in
  ;; `doom-modeline-mode-alist' (magit, info, pdf...) keep their own.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and mode-line-format (not (doom-modeline-auto-set-modeline)))
        (doom-modeline-set-modeline 'rsr/default)))))

(provide 'modeline)
;;; modeline.el ends here
