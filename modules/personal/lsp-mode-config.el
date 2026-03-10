;;; lsp-mode-config.el --- LSP general configuration -*- lexical-binding: t; -*-

;;; Code:

;;; Performance — increase process read buffer for LSP throughput
(setq read-process-output-max (* 1024 1024))

;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-headerline-breadcrumb-enable nil
        lsp-document-sync-method 2        ;; incremental — sends diffs only, not full buffer
        lsp-diagnostics-provider :flymake) ;; flymake handles large error counts better than flycheck
  (define-key lsp-mode-map (kbd "C-M-,") #'xref-go-forward))

;;; lsp-ui — sideline disabled; peek enabled for references
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil   ;; manual only — no auto-show on move
        lsp-ui-doc-show-with-mouse nil    ;; manual only — no auto-show on hover
        lsp-ui-doc-position 'top
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 30
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t))

;;; Fringe — red X for errors, yellow ! for warnings
(define-fringe-bitmap 'my/flymake-error-x
  [#b10000001
   #b01000010
   #b00100100
   #b00011000
   #b00011000
   #b00100100
   #b01000010
   #b10000001])

(setq flymake-error-bitmap   '(my/flymake-error-x compilation-error)
      flymake-warning-bitmap '(exclamation-mark    compilation-warning)
      flymake-note-bitmap    '(right-triangle      compilation-info))

;;; posframe — child frame primitives used by my/show-error-frame
(use-package posframe :ensure t :defer t)

;;; On-demand diagnostics

(defconst my/error-frame-buf " *flymake-posframe*"
  "Buffer name for the flymake error posframe.")

(defun my/posframe-point-top-right (info)
  "Position posframe above and to the right of point."
  (let* ((px (plist-get info :position-pixel-x))
         (py (plist-get info :position-pixel-y))
         (fw (plist-get info :posframe-width))
         (fh (plist-get info :posframe-height))
         (pw (plist-get info :parent-frame-width))
         (x  (min (+ px 16) (- pw fw 10)))
         (y  (max 0 (- py fh 6))))
    (cons x y)))

(defun my/error-frame--format (diags)
  "Format DIAGS with colors: errors red, warnings yellow, notes green."
  (let* ((header (propertize " Diagnostics:\n" 'face '(:weight bold)))
         (n 0)
         (lines (mapconcat
                 (lambda (d)
                   (setq n (1+ n))
                   (let* ((type (flymake-diagnostic-type d))
                          (text (flymake-diagnostic-text d))
                          (face (pcase type
                                  (:error   '(:foreground "#ff5555" :weight bold))
                                  (:warning '(:foreground "#ffaa00"))
                                  (_        '(:foreground "#50fa7b"))))
                          (icon (pcase type
                                  (:error   "✖")
                                  (:warning "⚠")
                                  (_        "●"))))
                     (propertize (format " %d. %s %s" n icon text) 'face face)))
                 diags "\n")))
    (concat header lines "\n")))

(defun my/show-error-frame ()
  "Show flymake diagnostics in a child frame below the current line.
Press f to focus into frame and copy. Any other key closes.
When focused: q or C-g closes and returns focus."
  (interactive)
  (require 'posframe)
  (let ((diags (flymake-diagnostics (line-beginning-position) (line-end-position))))
    (if (not diags)
        (message "No diagnostic on this line")
      (let* ((parent (selected-frame))
             (child  (posframe-show my/error-frame-buf
                                    :string (my/error-frame--format diags)
                                    :position (point)
                                    :poshandler #'posframe-poshandler-point-bottom-left-corner
                                    :internal-border-width 1
                                    :internal-border-color "#555555"
                                    :foreground-color "#cccccc"
                                    :background-color (face-attribute 'default :background)
                                    :left-fringe 8
                                    :right-fringe 8
                                    :accept-focus t)))
        (with-current-buffer my/error-frame-buf
          (dolist (k '("q" "C-g"))
            (local-set-key (kbd k)
                           (lambda ()
                             (interactive)
                             (posframe-hide my/error-frame-buf)
                             (select-frame-set-input-focus parent)))))
        (let ((key (read-key "[f] focus · [any] close")))
          (if (eq key ?f)
              (select-frame-set-input-focus child)
            (posframe-hide my/error-frame-buf)))))))

(defun my/lsp-doc-show-and-focus ()
  "Show lsp-ui-doc frame and focus into it for scrolling and copying."
  (interactive)
  (lsp-ui-doc-show)
  (lsp-ui-doc-focus-frame))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l d") #'my/lsp-doc-show-and-focus)   ;; doc frame — show + focus
  (define-key lsp-mode-map (kbd "C-c l D") #'lsp-ui-doc-show)             ;; quick hover (no focus)
  (define-key lsp-mode-map (kbd "C-c l e") #'my/show-error-frame)    ;; error child frame
  (define-key lsp-mode-map (kbd "C-c l E") #'consult-flymake)        ;; all errors + live code preview
  (define-key lsp-mode-map (kbd "C-c l p") #'lsp-ui-peek-find-references)) ;; references peek

(provide 'lsp-mode-config)
;;; lsp-mode-config.el ends here
