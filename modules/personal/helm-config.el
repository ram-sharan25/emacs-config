;;helm package for emacs

(use-package helm-dash
  :ensure t
  :defer t
  :config
  (setq helm-dash-docsets-path
	(case system-type
	  (linux "/mnt/Data/Files/Documentation/docsets/")
	  (darwin "~/Library/Application Support/Dash/DocSets/"))
	dash-docs-enable-debugging nil
	helm-dash-browser-func 'browse-web)
  (bind-keys :map bp/global-prefix-map
	     ("d h" . helm-dash)
	     ("d ." . helm-dash-at-point)))

(use-package helm-recoll
  :ensure t
  :defer t
  :init
  (setf helm-recoll-options '("/Applications/recoll.app/Contents/MacOS/recoll" "-t"))
  (helm-recoll-create-source "main" "~/.recoll"))
