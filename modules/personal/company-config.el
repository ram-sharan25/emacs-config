;;; company-config.el --- In-buffer code completion via corfu -*- lexical-binding: t; -*-

;;; Code:

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)              ;; show popup automatically
  (corfu-auto-delay 0.2)      ;; seconds before popup appears
  (corfu-auto-prefix 2)       ;; min chars before triggering
  (corfu-cycle t)             ;; cycle through candidates
  (corfu-quit-no-match 'separator) ;; stay open for async backends (LSP/ECA)
  :init
  (global-corfu-mode)
  :config
  ;; disable in non-editing modes — prevents org-element-at-point warnings
  (add-hook 'org-agenda-mode-hook (lambda () (corfu-mode -1)))
  (add-hook 'vterm-mode-hook      (lambda () (corfu-mode -1))))

;; cape — extra completion sources
(use-package cape
  :ensure t
  :init
  (setq cape-dabbrev-check-other-buffers nil) ;; scan current buffer only — prevents freeze
  (add-to-list 'completion-at-point-functions #'cape-file) ;; file paths everywhere
  :config
  ;; dabbrev only in text/notes — LSP handles completions in code buffers
  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda ()
                     (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)))))

(provide 'company-config)
;;; company-config.el ends here
