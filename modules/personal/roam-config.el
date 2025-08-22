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
  (setq org-roam-directory "~/Stillness/Personal/")

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
        (("s-r" . org-download-clipboard)))
  :custom
  (org-download-heading-lvl nil) ; Don't organize by heading
  (org-download-image-dir "./data/") ; Default, overridden by our function
  (org-download-image-org-width 100)
  (org-download-image-html-width 800)
  (org-download-image-latex-width 600)
  :config
  ;; Ensure ./data/ exists relative to the current buffer file
  (defun org-download--dir-2 ()
    "Return ./data directory relative to buffer file. Create if needed."
    (let* ((base-dir (file-name-directory (or (buffer-file-name) default-directory)))
     (data-dir (expand-file-name "data" base-dir)))
      (unless (file-directory-p data-dir)
  (make-directory data-dir t))
      data-dir))

  ;; Prompt user for image filename and save it to ./data
  (defun org-download--fullname (filename link)
    "Prompt for a custom filename and save it in ./data/."
    (let* ((ext (file-name-extension filename))
     (base-name (read-string "Image name (without extension): "))
     (final-name (concat base-name "." ext))
     (dir (org-download--dir-2)))
      (expand-file-name final-name dir))))
(setq org-startup-with-inline-images t)
