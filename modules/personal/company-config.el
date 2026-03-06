;;; company-config.el --- In-buffer code completion via corfu -*- lexical-binding: t; -*-

;;; Code:

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)              ;; show popup automatically
  (corfu-auto-delay 0.2)      ;; seconds before popup appears
  (corfu-auto-prefix 2)       ;; min chars before triggering
  (corfu-cycle t)             ;; cycle through candidates
  (corfu-quit-no-match t)     ;; auto-dismiss if no match
  :init
  (global-corfu-mode)
  :config
  ;; disable in non-editing modes — prevents org-element-at-point warnings
  (add-hook 'org-agenda-mode-hook (lambda () (corfu-mode -1)))
  (add-hook 'vterm-mode-hook      (lambda () (corfu-mode -1))))

;; cape — extra completion sources (files, dabbrev, keywords)
(use-package cape
  :ensure t
  :init
  (setq cape-dabbrev-check-other-buffers nil) ;; scan current buffer only — prevents freeze
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(provide 'company-config)
;;; company-config.el ends here
