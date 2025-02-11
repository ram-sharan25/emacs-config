(use-package smartparens
  :ensure t
  :bind (("C-M-f" . sp-forward-sexp)
	 ("C-M-b" . sp-backward-sexp)))

(global-set-key (kbd "s-/") 'comment-dwim)
(global-set-key (kbd "s-l") 'mark-whole-line)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)

(setq org-latex-toc-command "\\tableofcontents \\clearpage")

(use-package smartrep
  :ensure t)

(use-Package multiple-cursors
  :ensure t
  :defer t
  :commands (mc/mark-previous-like-this mc/mark-next-like-this)
  :init
  (smartrep-define-key rsr/global-prefix-map
      "m"
    '(("p" . mc/mark-previous-like-this)
      ("n" . mc/mark-next-like-this)
      ("0" . mc/insert-numbers)
      ("a" . mc/insert-letters))))

(defun rsr/prog-mode-hook ()
  (add-hook 'before-save-hook #'whitespace-cleanup))

(add-hook 'prog-mode-hook #'rsr/prog-mode-hook)

;; Enable org-indent-mode by default for all org files
(add-hook 'org-mode-hook 'org-indent-mode)
(setq-default toggle-truncate-lines t)


;; Enable hs-minor-mode globally
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'text-mode-hook #'hs-minor-mode)

;; Keybinding to toggle hide/show of the current block in hs-minor-mode
(global-set-key (kbd "C-c C-t") 'hs-toggle-hiding)
;; Keybinding to toggle hide/show of the current block in hs-minor-mode
(global-set-key (kbd "C-c C-h") 'hs-hide-block)
(global-set-key (kbd "C-c C-s") 'hs-show-block)
(global-set-key (kbd "C-c C-c") 'hs-hide-all)
(global-set-key (kbd "C-c C-a") 'hs-show-all)
