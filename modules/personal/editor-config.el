(use-package smartparens
  :ensure t
  :bind (("C-M-f" . sp-forward-sexp)
	 ("C-M-b" . sp-backward-sexp)))

(use-package smartrep
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :defer t
  :commands (mc/mark-previous-like-this mc/mark-next-like-this)
  :init
  (smartrep-define-key bp/global-prefix-map
      "m"
    '(("p" . mc/mark-previous-like-this)
      ("n" . mc/mark-next-like-this)
      ("0" . mc/insert-numbers)
      ("a" . mc/insert-letters))))

(defun rsr/prog-mode-hook ()
  (add-hook 'before-save-hook #'whitespace-cleanup))

(add-hook 'prog-mode-hook #'rsr/prog-mode-hook)
