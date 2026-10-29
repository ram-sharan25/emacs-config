;;; editor-config.el --- General editing settings -*- lexical-binding: t; -*-

;;; Code:

;;; --- Smartparens ---

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)))

;;; --- Multiple Cursors ---

;; smartrep stays for mpv-notes.el; mc and goto-chg repeat via `repeat-mode'.
(use-package smartrep
  :ensure t)

;; C-c m p / n / 0 / a (keymaps.el), then tap p / n to repeat.
(use-package multiple-cursors
  :ensure t
  :defer t)

;;; --- Enable Narrowing Commands ---

(put 'narrow-to-region   'disabled nil)
(put 'narrow-to-page     'disabled nil)
(put 'narrow-to-defun    'disabled nil)

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

;;; --- Autosave & Backup — redirect clutter to central dirs ---

(let ((auto-saves-dir (expand-file-name "auto-saves/" user-emacs-directory))
      (backups-dir    (expand-file-name "backups/"    user-emacs-directory)))
  (make-directory auto-saves-dir t)
  (make-directory backups-dir t)
  ;; redirect #file# crash-recovery files
  (setq auto-save-file-name-transforms `((".*" ,auto-saves-dir t)))
  ;; redirect file~ backup files
  (setq backup-directory-alist `((".*" . ,backups-dir))))

;; also save the real file automatically (VS Code style)
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 2)  ;; seconds of idle before saving
(setq backup-by-copying t)           ;; safer: copy instead of rename (no rename races)

;; exclude backup files (file~, file.tsx~, etc.) from all completions
(add-to-list 'completion-ignored-extensions "~")  ;; covers file.tsx~ file.el~ etc.
(with-eval-after-load 'ivy
  (add-to-list 'ivy-ignore-buffers "~$"))          ;; ivy buffer list
(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-file-suffixes "~")
  (add-to-list 'projectile-globally-ignored-directories
               (expand-file-name "backups/" user-emacs-directory))
  (add-to-list 'projectile-globally-ignored-directories
               (expand-file-name "auto-saves/" user-emacs-directory)))

;;; --- Auto Revert ---

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil               ;; suppress "Reverting buffer..." messages
      global-auto-revert-non-file-buffers t) ;; also revert dired when dir changes

;;; --- Repeat Mode ---

(repeat-mode 1)

;;; --- Electric Pair ---

;; smartparens handles prog-mode — electric-pair covers everything else (text, org, etc.)
(electric-pair-mode 1)
(add-hook 'prog-mode-hook (lambda () (electric-pair-local-mode -1)))

;;; --- Avy — jump to any visible text in 2-3 keypresses ---

;; C-: (keymaps.el): type chars, avy highlights matches, press overlay letter.
;; Not M-j: skhd owns Option+J.
(use-package avy
  :ensure t
  :defer t
  :config
  (setq avy-timeout-seconds 0.3))        ;; how long to wait for more chars before showing overlays

;;; --- Electric Indent ---

(electric-indent-mode 1)

;; org manages its own RET behavior — disable electric-indent there
(defun rsr/org-disable-electric-indent ()
  "Disable electric indent in org-mode; RET indents via `org-return' below."
  (electric-indent-local-mode -1))

(add-hook 'org-mode-hook #'rsr/org-disable-electric-indent)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "RET") (lambda () (interactive) (org-return t))))

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

;;; --- Persistent Cursor Position (save-place) ---

;; Remember point in every file and restore it when the file is reopened or
;; Emacs restarts.  Built-in; negligible cost and no startup-time impact.
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;;; --- Navigation History (dogears) ---

;; Automatically remembers ("dogears") the places you visit, so you can retrace
;; your trail: a list (`dogears-list'), a quick picker (`dogears-go'), and
;; back/forward stepping.  Loading is deferred and recording is switched on via
;; an idle timer, so it adds nothing to startup (mirrors the config's pattern).
(use-package dogears
  :ensure t
  :commands (dogears-mode)
  ;; dogears README keys, under the built-in goto prefix.  Bound here rather
  ;; than in keymaps.el: dogears-back / -forward are not autoloaded.
  :bind (("M-g d"   . dogears-go)
         ("M-g M-b" . dogears-back)
         ("M-g M-f" . dogears-forward)
         ("M-g M-d" . dogears-list))
  :init
  (run-with-idle-timer 1 nil (lambda () (dogears-mode 1))))

;;; --- Jump to Last Edit (goto-chg) ---

;; Jump to where you last edited, and again to step further back through your
;; edits (Vim's g; / g,).  Complements dogears: dogears tracks places you
;; *visited*, goto-chg tracks places you *changed*.  Deferred via autoloads.
;; `M-g .' then tap . / , to keep walking (keymaps.el, via `repeat-mode').
(use-package goto-chg
  :ensure t
  :commands (goto-last-change goto-last-change-reverse))

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
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c 0") #'rsr/insert-indented-todo-item))

(provide 'editor-config)
;;; editor-config.el ends here
