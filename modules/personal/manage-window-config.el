;;; manage-window-config.el --- Window management and completion -*- lexical-binding: t; -*-

;;; Code:

;;; --- Window Splits ---

(defun rsr/split-vertical ()
  "Split window right and open scratch buffer in new window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (scratch-buffer))

(defun rsr/split-horizontal ()
  "Split window below and open scratch buffer in new window."
  (interactive)
  (split-window-below)
  (other-window 1)
  (scratch-buffer))

;;; --- Vertico ---

(use-package vertico
  :ensure t
  :custom
  (vertico-count 12)   ;; reasonable height
  (vertico-cycle t)
  :init
  (vertico-mode))

;;; --- Orderless (fuzzy matching) ---

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; --- Marginalia (annotations in minibuffer) ---

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;;; Emacs 29 compatibility — consult 3.x references this Emacs 30 variable
(unless (boundp 'minibuffer-visible-completions)
  (defvar minibuffer-visible-completions nil))

;;; --- Consult (enhanced commands) ---

(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
         ("C-x M-f" . consult-recent-file)
         ("C-s"     . consult-line)
         ("M-s"     . consult-imenu)
         ("M-y"     . consult-yank-pop)))

;;; --- Embark (context-sensitive actions) ---

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; savehist — M-x remembers command history (replaces smex)
(savehist-mode 1)

;;; --- Window Navigation ---

;; ace-window — jump, swap, close windows with letter overlays
(use-package ace-window
  :ensure t
  :bind ("C-c w w" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always t))

(defun rsr/toggle-window-split ()
  "Toggle between horizontal and vertical split for two windows."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Only works with exactly 2 windows"))
  (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (vertical-p (= (car this-win-edges)
                        (car (window-edges (next-window))))))
    (delete-other-windows)
    (if vertical-p
        (split-window-horizontally)
      (split-window-vertically))
    (set-window-buffer (selected-window) this-win-buffer)
    (set-window-buffer (next-window) next-win-buffer)))

;;; Keybindings
(global-set-key (kbd "C-x 3") #'rsr/split-vertical)
(global-set-key (kbd "C-x 2") #'rsr/split-horizontal)
(global-set-key (kbd "C-c w t") #'rsr/toggle-window-split)

;; Unbind windmove shift+arrow keys — conflict with org scheduling
(global-unset-key [S-left])
(global-unset-key [S-right])
(global-unset-key [S-up])
(global-unset-key [S-down])

(provide 'manage-window-config)
;;; manage-window-config.el ends here
