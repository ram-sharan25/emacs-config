;; Helm Dash
(use-package helm-dash
  :ensure t
  :defer t
  :config
  (setq helm-dash-docsets-path
	(cond
	 ((eq system-type 'gnu/linux) "/mnt/Data/Files/Documentation/docsets/")
	 ((eq system-type 'darwin) "~/Library/Application Support/Dash/DocSets/"))
	dash-docs-enable-debugging nil
	helm-dash-browser-func 'browse-web)

  ;; Define a sub-keymap for "d"
  (define-prefix-command 'rsr/doc-map)
  (define-key rsr/global-prefix-map (kbd "d") 'rsr/doc-map)

  (define-key rsr/doc-map (kbd "h") 'helm-dash)
  (define-key rsr/doc-map (kbd ".") 'helm-dash-at-point))

;; Helm Recoll
(use-package helm-recoll
  :ensure t
  :defer t
  :config
  (setq helm-recoll-options '("/Applications/recoll.app/Contents/MacOS/recoll" "-t"))
  (helm-recoll-create-source "main" "~/.recoll"))
