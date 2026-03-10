;;; company-config.el --- In-buffer completion via corfu + cape -*- lexical-binding: t; -*-

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
  :config
  ;; cape-file only in text/org — LSP handles path completions in prog-mode
  ;; (was global before; caused backup file paths to be silently inserted in code)
  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda ()
                     (add-hook 'completion-at-point-functions #'cape-file    nil t)
                     (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)))))

;; Fix: lsp-passthrough + orderless race condition with corfu auto-complete
;; lsp-capf gets orderless directly, bypassing the broken lsp-passthrough style
(with-eval-after-load 'lsp-mode
  (add-to-list 'completion-category-overrides
               '(lsp-capf (styles orderless basic))))

(provide 'company-config)
;;; company-config.el ends here
