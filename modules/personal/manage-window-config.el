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
  (vertico-count 20)
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

;;; --- Consult (enhanced commands) ---

(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
         ("C-x M-f" . consult-recent-file)
         ("C-s"     . consult-line)
         ("M-s"     . consult-imenu)
         ("M-y"     . consult-yank-pop)))

;;; savehist — M-x remembers command history (replaces smex)
(savehist-mode 1)

;;; Keybindings
(global-set-key (kbd "C-x 3") #'rsr/split-vertical)
(global-set-key (kbd "C-x 2") #'rsr/split-horizontal)

(provide 'manage-window-config)
;;; manage-window-config.el ends here
