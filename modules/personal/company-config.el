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
  (add-hook 'vterm-mode-hook      (lambda () (corfu-mode -1)))

  ;; macOS NS: make-frame-visible calls makeKeyAndOrderFront: which steals the
  ;; macOS key window. When the frame is later hidden, focus goes to the last
  ;; active app (kitty). Fix: replace hide/show with alpha 0/100 so the frame
  ;; stays "visible" at the NS level — makeKeyAndOrderFront: is never called.
  (when (eq window-system 'ns)
    (advice-add #'make-frame-invisible :around
                (lambda (orig frame &rest args)
                  (if (and (boundp 'corfu--frame) (eq frame corfu--frame))
                      (set-frame-parameter frame 'alpha '(0 . 0))
                    (apply orig frame args))))
    (advice-add #'make-frame-visible :around
                (lambda (orig frame &rest args)
                  (if (and (boundp 'corfu--frame) (eq frame corfu--frame))
                      (set-frame-parameter frame 'alpha '(100 . 100))
                    (apply orig frame args))))))

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
                     ;; org-mode adds ispell-completion-at-point by default — remove it.
                     ;; It spawns a 'look' process on every corfu trigger, causing noise.
                     (remove-hook 'completion-at-point-functions #'ispell-completion-at-point t)
                     (add-hook 'completion-at-point-functions #'cape-file    nil t)
                     (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)))))

;; Fix: lsp-passthrough + orderless race condition with corfu auto-complete
;; lsp-capf gets orderless directly, bypassing the broken lsp-passthrough style
(with-eval-after-load 'lsp-mode
  (add-to-list 'completion-category-overrides
               '(lsp-capf (styles orderless basic))))

(provide 'company-config)
;;; company-config.el ends here
