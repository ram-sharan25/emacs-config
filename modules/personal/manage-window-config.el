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

;;; Keybindings
(global-set-key (kbd "C-x 3") #'rsr/split-vertical)
(global-set-key (kbd "C-x 2") #'rsr/split-horizontal)

(provide 'manage-window-config)
;;; manage-window-config.el ends here
