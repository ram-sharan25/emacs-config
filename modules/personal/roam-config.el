(require 'paths)
(require 'gtd-config)
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
  (setq org-roam-directory my/brain-dir)
  (setq org-roam-directory-exclude-patterns '("cleanup_2026_02_27/*"))
  (setq org-roam-file-extensions '("org" "org_archive"))
  (setq org-roam-list-files-commands '(fd find))

  (add-hook 'after-init-hook #'org-roam-db-autosync-mode)

  ;; Link Abbreviation for portable data links
  ;; Usage: [[data:image.png]] -> expands to my/data-dir/image.png

  (setq org-roam-capture-templates
  '(("z" "Zettel" plain
     "%?"
     :if-new (file+head "Public/${slug}.org"
            "#+title: ${title}\n#+filetags: :ZETTEL:\n#+AREA: %(progn (setq my/zettel-area (my/select-area-default-misc)) my/zettel-area)\n#+PROJECT: %(let* ((proj-cons (my/org-select-project-allow-empty my/zettel-area)) (proj-name (car proj-cons)) (proj-id (cdr proj-cons))) (if proj-id (format \"[[id:%s][%s]]\" proj-id proj-name) proj-name))\n\n* Context\n- Area: [[id:%(my/get-area-id-by-name my/zettel-area)][%(identity my/zettel-area)]]\n")
     :unnarrowed t)))

  (setf org-id-link-to-org-use-id t))

(use-package org-download
  :ensure t
  :bind (:map org-mode-map
        (("s-r" . org-download-clipboard)))
  :custom
  (org-download-heading-lvl nil) ; Don't organize by heading
  (org-download-image-dir my/data-dir) ; Centralized data directory
  (org-download-image-org-width 800)
  (org-download-image-html-width 800)
  (org-download-image-latex-width 10)
  :config
  ;; Prompt user for image filename and save it to my/data-dir
  (defun org-download--fullname (filename link)
    "Prompt for a custom filename and save it in my/data-dir."
    (let* ((ext (file-name-extension filename))
           (base-name (read-string "Image name (without extension): "))
           (final-name (concat base-name "." ext)))
      (expand-file-name final-name my/data-dir))))
(setq org-startup-with-inline-images t)

(use-package websocket
  :after org-roam
  :defer t)

(use-package org-roam-ui
  :after org-roam
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)) ;; open on demand: M-x org-roam-ui-mode


(use-package org-roam-bibtex
  :after org-roam
  :ensure t
  :config
  (require 'org-ref) ; Optional: if you use org-ref for citations
  (org-roam-bibtex-mode)

  ;; Configuration to prepopulate the note with bib data
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))

 (add-to-list 'org-roam-capture-templates
             `("r" "bibliography reference" plain
               "%?"
               :if-new
               (file+head ,(concat (file-name-as-directory my/resources-dir) "${citekey}.org")
                          "#+title: ${title}\n#+filetags: :reading:\n\n* Source\nCitekey:\nAuthor: ${author-or-editor}\n\n* Notes\n")
               :unnarrowed t)))

;; Use 'concat' to safely join the variable with the filename
;; Result: /Users/rrimal/Stillness/Brain/Resources/Zotero/zotero_ref.bib
(setq bibtex-completion-bibliography (list (concat my/resources-dir "Zotero/zotero_ref.bib"))
      ;; Point this to where your PDFs live
      bibtex-completion-library-path (list my/zotero-storage)
      bibtex-completion-pdf-field "file")
(provide 'roam-config)
