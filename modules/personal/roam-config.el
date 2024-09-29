(use-package org-roam
  :ensure t
  :bind (:map global-map
	      (("M-m r f" . org-roam-node-find)
	       ("M-m r i" . org-roam-node-insert)
	       ("M-m r F" . org-roam-ref-find)
	       ("M-m r c" . org-roam-capture)
	       ("M-m r g" . org-roam-graph)))
  :bind (:map org-mode-map
	      (("M-m o r" . org-roam-node-insert)
	       ("M-m r i" . org-roam-node-insert)
	       ("M-m r r" . org-roam-buffer-toggle)
	       ("M-m r R" . org-roam-ref-add)
	       ("M-m r a" . org-roam-alias-add)
	       ("M-m r t" . org-roam-tag-add)))
  :config
  (setq org-roam-directory "/Users/rrimal/Stillness/Notes")

  (org-roam-db-autosync-mode t)

  (setq org-roam-capture-templates
	'(("d" "default" plain
	   "%?"
	   :if-new (file+head "${slug}.org"
			      "#+title: ${title}\n")
	   :unnarrowed t)))

  (setf org-id-link-to-org-use-id t))

(use-package org-download
  :ensure t
  :bind (:map org-mode-map
	      (("M-v" . org-download-clipboard)))
  :custom
  (org-download-image-dir "./data/")
  (org-download-heading-lvl nil)
  (org-download-image-org-width 100)
  (org-download-image-html-width 800)
  (org-download-image-latex-width 600)
  :config
  (defun org-download--dir-2 ()
    (if org-download-heading-lvl
	(org-download-get-heading
	 org-download-heading-lvl)
      (file-name-base (buffer-file-name)))))
