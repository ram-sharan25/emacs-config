(use-package smartparens
  :ensure t
  :bind (("C-M-f" . sp-forward-sexp)
	 ("C-M-b" . sp-backward-sexp)))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)

(setq org-latex-toc-command "\\tableofcontents \\clearpage")

(use-package smartrep
  :ensure t)

(use-package multiple-cursors
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

(setq-default toggle-truncate-lines t)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(defun my-insert-indented-todo-item ()
  "Insert a new to-do item ('- [ ] ') on the next line
with indentation based on the current or previous line."
  (interactive)
  (end-of-line)

  (let* (;; Get indentation of the current line.
	 (current-indent (current-indentation))
	 ;; Get indentation of the previous line.
	 (prev-indent (save-excursion (forward-line -1) (current-indentation)))
	 ;; Determine the final indentation value based on your rules.
	 (final-indent
	  (cond
	   ((> current-indent 0) current-indent) ; Rule 1: Use current line's indent.
	   ((> prev-indent 0) prev-indent)     ; Rule 2: Use previous line's indent.
	   (t 2))))                          ; Rule 3: Fallback to 2 spaces.

    ;; Insert the content with the calculated indentation.
    (insert "\n" (make-string final-indent ?\ ) "- [ ] ")))

(global-set-key (kbd "C-c 0") 'my-insert-indented-todo-item)
(global-set-key (kbd "C-c C-0") 'my-insert-indented-todo-item)

;; Keybinding to toggle hide/show of the current block in hs-minor-mode
(global-set-key (kbd "C-c C-t") 'hs-toggle-hiding)
;; Keybinding to toggle hide/show of the current block in hs-minor-mode
(global-set-key (kbd "C-c C-h") 'hs-hide-block)
(global-set-key (kbd "C-c C-s") 'hs-show-block)
(global-set-key (kbd "C-c C-c") 'hs-hide-all)
(global-set-key (kbd "C-c C-a") 'hs-show-all)

;; Auto-indent on RET — built-in, fires only on newline/trigger chars.
;; Zero idle or timer overhead. electric-indent-mode is already the
;; Emacs default but we enable it explicitly for clarity.
(electric-indent-mode 1)

;; Org-mode manages its own RET behavior; electric-indent conflicts with it.
;; Use org-return-indent instead: same as org-return but also indents the new line.
;; Fires only on keypress — zero idle or timer overhead.
(add-hook 'org-mode-hook
          (lambda ()
            (electric-indent-local-mode -1)
            (local-set-key (kbd "RET") (lambda () (interactive) (org-return t)))))
