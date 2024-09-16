;;; config for Ivy in emacs
(use-package ivy
  :ensure t
  :bind (("C-x b" . ido-switch-buffer))
  :config
  (require 'smex)
  (ivy-mode t)
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)
  :init
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "
	ivy-re-builders-alist '( (t . ivy--regex-ignore-order))))

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'full)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :demand t
  :bind (("C-x C-f" . counsel-find-file)
		 ("C-x M-f" . counsel-recentf)
		 ("M-x" . counsel-M-x)
		 ("C-c u" . counsel-unicode-char)
		 ("C-c s" . counsel-rg)
		 ("M-s" . counsel-imenu))
  :config
  ;;(setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) " ")
  ;; enable opening file as sudo
  (defadvice counsel-find-file (after find-file-sudo activate)
    "Find file as root if necessary."
    (when (and buffer-file-name
			   (file-exists-p buffer-file-name)
			   (not (file-writable-p buffer-file-name)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (ivy-configure 'counsel-M-x
    :initial-input ""))
