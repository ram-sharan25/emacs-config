;;; editor-config.el --- General editing settings -*- lexical-binding: t; -*-

;;; Code:

;;; --- Smartparens ---

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)))

;;; --- Multiple Cursors ---

(use-package smartrep
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :defer t
  :commands (mc/mark-previous-like-this mc/mark-next-like-this)
  :init
  (smartrep-define-key rsr/global-prefix-map "m"
    '(("p" . mc/mark-previous-like-this)
      ("n" . mc/mark-next-like-this)
      ("0" . mc/insert-numbers)
      ("a" . mc/insert-letters))))

;;; --- Line Display ---

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook  #'display-line-numbers-mode)
(setq-default truncate-lines t)

;;; --- Code Folding (hs-minor-mode) ---

(add-hook 'prog-mode-hook #'hs-minor-mode)

(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-c h t") #'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-c h h") #'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c h s") #'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c h a") #'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c h A") #'hs-show-all))

;;; --- Whitespace Cleanup on Save ---

(defun rsr/prog-mode-hook ()
  "Hooks for programming modes."
  (add-hook 'before-save-hook #'whitespace-cleanup nil t))

(add-hook 'prog-mode-hook #'rsr/prog-mode-hook)

;;; --- Electric Indent ---

(electric-indent-mode 1)

;; org manages its own RET behavior — disable electric-indent there
(defun rsr/org-disable-electric-indent ()
  "Disable electric indent in org-mode and use org-return with indent."
  (electric-indent-local-mode -1)
  (local-set-key (kbd "RET") (lambda () (interactive) (org-return t))))

(add-hook 'org-mode-hook #'rsr/org-disable-electric-indent)

;;; --- Pulsar (flash line on jump) ---

(use-package pulsar
  :ensure t
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.055
        pulsar-face 'pulsar-magenta)
  (pulsar-global-mode 1)
  (dolist (fn '(xref-find-definitions
                xref-go-back
                xref-go-forward
                org-agenda-goto
                recenter-top-bottom
                scroll-up-command
                scroll-down-command))
    (add-to-list 'pulsar-pulse-functions fn)))

;;; --- Custom Commands ---

(defun rsr/insert-indented-todo-item ()
  "Insert a new '- [ ] ' todo item on the next line, matching current indentation."
  (interactive)
  (end-of-line)
  (let* ((current-indent (current-indentation))
         (prev-indent (save-excursion (forward-line -1) (current-indentation)))
         (final-indent (cond ((> current-indent 0) current-indent)
                             ((> prev-indent 0) prev-indent)
                             (t 2))))
    (insert "\n" (make-string final-indent ?\ ) "- [ ] ")))

;;; Keybindings
(global-set-key (kbd "C-c 0")   #'rsr/insert-indented-todo-item)
(global-set-key (kbd "C-c C-0") #'rsr/insert-indented-todo-item)

(provide 'editor-config)
;;; editor-config.el ends here
