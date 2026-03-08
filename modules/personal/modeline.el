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
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  ;; disable org-clock injecting full task into global-mode-string
  (setq org-clock-clocked-in-display nil)
  (setq display-time-default-load-average nil)
  (setq display-time-format "%a %m/%d %I:%M %p")
  (setq doom-modeline-battery t)
  (display-time-mode 1)
  (display-battery-mode 1)
  (doom-modeline-mode 1)
  :config
  ;; --- custom segments (must be inside :config — doom-modeline must be loaded first) ---

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

  (doom-modeline-def-segment rsr/clock
    "Org clock: truncated task + elapsed time."
    (or (rsr/modeline-org-clock) ""))

  (defun rsr/buffer-path-last-n (n)
    "Return buffer file path showing only last N directory components + filename."
    (if-let ((file (buffer-file-name)))
        (let* ((parts (seq-filter (lambda (s) (not (string-empty-p s)))
                                  (split-string (abbreviate-file-name file) "/")))
               (tail  (last parts (1+ n)))
               (dirs  (butlast tail))
               (fname (car (last tail))))
          (concat (propertize (if dirs (concat (mapconcat #'identity dirs "/") "/") "")
                              'face 'doom-modeline-buffer-path)
                  (propertize fname 'face 'doom-modeline-buffer-file)))
      (propertize (buffer-name) 'face 'doom-modeline-buffer-file)))

  (doom-modeline-def-segment rsr/buffer-info-prog
    "Buffer path showing last 3 folder components + filename."
    (concat " " (rsr/buffer-path-last-n 3)))

  ;; --- modeline layouts ---

  ;; default
  (doom-modeline-def-modeline 'rsr/default
    '(vcs buffer-info matches selection-info)
    '(rsr/clock persp-name major-mode time battery))

  ;; prog — filename only + flymake + lsp
  (doom-modeline-def-modeline 'rsr/prog
    '(vcs buffer-info-simple matches selection-info check)
    '(rsr/clock persp-name lsp major-mode time battery))

  ;; org — word count
  (doom-modeline-def-modeline 'rsr/org
    '(vcs buffer-info matches selection-info)
    '(rsr/clock word-count persp-name major-mode battery time))

  ;; magit / dired / agenda — minimal
  (doom-modeline-def-modeline 'rsr/minimal
    '(vcs buffer-info-simple)
    '(rsr/clock major-mode time battery))

  ;; set default for all buffers
  (doom-modeline-set-modeline 'rsr/default t)

  ;; per-mode hooks
  (add-hook 'prog-mode-hook
            (lambda () (doom-modeline-set-modeline 'rsr/prog)))
  (add-hook 'org-mode-hook
            (lambda () (doom-modeline-set-modeline 'rsr/org)))
  (add-hook 'magit-mode-hook
            (lambda () (doom-modeline-set-modeline 'rsr/minimal)))
  (add-hook 'dired-mode-hook
            (lambda () (doom-modeline-set-modeline 'rsr/minimal)))
  (add-hook 'org-agenda-mode-hook
            (lambda () (doom-modeline-set-modeline 'rsr/minimal))))

(provide 'modeline)
;;; modeline.el ends here
