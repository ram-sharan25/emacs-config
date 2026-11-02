;;; roam-config.el --- Org-roam, bibliography, and research workflow -*- lexical-binding: t; -*-

;;; Code:

(require 'paths)
(require 'gtd-config)
;; Roam node policy, whole-repo org-id coverage, duplicate-ID guard (D16).
;; See Plans/Brain/.../2026-11-01-brain-roam-id-links/findings.md.
(require 'brain-roam-ids)

;; Prefer ID links over file+headline links in `org-store-link' (C-c l).
;; Set at top level, not inside the org-roam :config block below — org-roam
;; is :bind-deferred, so C-c l would ignore heading IDs until org-roam had
;; already been lazy-loaded once in the session.
(setq org-id-link-to-org-use-id t)

;;; --- Org-roam ---

(use-package org-roam
  :ensure t
  :bind (:map rsr/notes-map              ; C-c n, org-roam's own prefix
        (("f" . org-roam-node-find)
         ("i" . org-roam-node-insert)
         ("F" . org-roam-ref-find)
         ("c" . org-roam-capture)
         ("g" . org-roam-graph)
         ("r" . org-roam-buffer-toggle)
         ("e" . org-roam-ref-add)
         ("a" . org-roam-alias-add)
         ("t" . org-roam-tag-add)))
  :config
  (setq org-roam-directory my/brain-dir)
  ;; Target layout (D3/D5/D11): nodes live in Public/, Resources/ and Areas/.
  ;; gtd/ mechanics, Archives/, Trackers/ (tables) and Private/ (gpg) are
  ;; not indexed at all; `data/' and `output/' hold no notes.
  (setq org-roam-file-exclude-regexp
        '("^gtd/" "^Archives/" "^Trackers/" "^Private/" "^data/" "^output/"
          "\\.bak\\'" "~\\'"))
  ;; Which headings become nodes.  File-level nodes always.  Under Areas/
  ;; (project files, area hubs) no heading is a node: tasks, Log entries and
  ;; scaffolding keep their :ID: for linking, and a link written inside a
  ;; task is credited to the project/hub file node (org-roam walks up to the
  ;; nearest included node).  Elsewhere a heading is a node unless it, or an
  ;; ancestor, carries a TODO keyword.  REQUIRES every Areas/ file to have a
  ;; file-level :ID:, or roam drops the links inside its tasks; check with
  ;; M-x my/org-roam-files-without-file-id after a sync.
  (setq org-roam-db-node-include-function #'my/org-roam-node-include-p)
  (setq org-roam-file-extensions '("org"))
  (setq org-roam-list-files-commands '(fd find))

  ;; org-roam loads lazily, after `after-init-hook' has run, so enable here.
  (org-roam-db-autosync-mode 1)

  (setq org-roam-capture-templates
        '(("z" "Zettel" plain
           "%?"
           :if-new (file+head "Public/${slug}.org"
                              "#+title: ${title}\n#+filetags: :zettel:\n\n* Source\n- %(my/zettel-source-link)\n")
           :unnarrowed t))))

;;; --- Org-download ---

(use-package org-download
  :ensure t
  :bind (:map org-mode-map
        (("s-r" . org-download-clipboard)))
  :custom
  (org-download-heading-lvl nil)
  (org-download-image-dir my/data-dir)
  (org-download-image-org-width 800)
  (org-download-image-html-width 800)
  (org-download-image-latex-width 10)
  :config
  (defun org-download--fullname (filename link)
    "Prompt for a custom filename and save it in my/data-dir."
    (let* ((ext (file-name-extension filename))
           (base-name (read-string "Image name (without extension): "))
           (final-name (concat base-name "." ext)))
      (expand-file-name final-name my/data-dir)))

  ;; Compress every pasted image to the data/ standard (D12) before
  ;; org-download inserts its link.  One implementation of the standard:
  ;; .githooks/compress-images.py in the Brain repo (the pre-commit hook runs
  ;; the same script).  Synchronous, ~0.2-1 s per paste.  The script turns
  ;; HEIC into JPEG and removes the .heic, so the link must name the .jpg.
  (defun my/org-download-compress (args)
    "Compress the file in ARGS (LINK FILENAME) before its link is inserted.
Return ARGS with FILENAME renamed when compression changed its extension."
    (let ((link (nth 0 args))
          (filename (nth 1 args)))
      (when (and (file-exists-p filename)
                 (string-match-p "\\.\\(png\\|jpe?g\\|heic\\)\\'" filename))
        (call-process (expand-file-name "../.githooks/compress-images.py" my/data-dir)
                      nil nil nil filename)
        (let ((jpg (concat (file-name-sans-extension filename) ".jpg")))
          (when (and (string-match-p "\\.heic\\'" filename)
                     (not (file-exists-p filename))
                     (file-exists-p jpg))
            (setq filename jpg))))
      (list link filename)))
  (advice-add 'org-download-insert-link :filter-args #'my/org-download-compress))

(defun my/zettel-source-link ()
  "Get source link for Zettel.
If current heading has a '- src:' line, use that link so Zettels
created from fleeting notes point to the original resource, not the
fleeting note itself. Falls back to current position otherwise."
  (save-excursion
    (when (derived-mode-p 'org-mode)
      (condition-case nil (org-back-to-heading t) (error nil))
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (if (re-search-forward "^- src: \\(.+\\)$" end t)
            (match-string 1)
          (or (org-capture-get :annotation) ""))))))

;;; --- Org-roam-ui ---

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
        org-roam-ui-open-on-start nil))

;;; --- Research workflow: org-ref + org-roam-bibtex ---

(use-package org-ref
  :ensure t
  :defer t)

(use-package bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-bibliography (list (concat my/resources-dir "Zotero/zotero_ref.bib"))
        bibtex-completion-library-path (list my/zotero-storage)
        bibtex-completion-notes-path my/resources-dir
        bibtex-completion-pdf-field "file"))

(use-package org-roam-bibtex
  :ensure t
  :after (org-roam org-ref)
  :config
  (org-roam-bibtex-mode)
  (setq orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))

  (add-to-list 'org-roam-capture-templates
               `("r" "Research paper" plain
                 "%?"
                 :if-new
                 (file+head ,(concat (file-name-as-directory my/resources-dir) "${citekey}.org")
                            "#+title: ${title}\n#+filetags: :source:\n#+AUTHOR: ${author-or-editor}\n#+CREATED_FROM: %a\n\n* Summary\n\n* Key Concepts\n\n* Quotes\n#+BEGIN_QUOTE\n\n#+END_QUOTE\n")
                 :unnarrowed t)))

;;; --- LaTeX export with BibTeX citations ---

(with-eval-after-load 'ox-latex
  ;; pdflatex → bibtex → pdflatex x2 for citation resolution
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f")))

;;; --- Brain search and paper helpers ---

(defun rsr/edit-paper-note ()
  "Select a BibTeX entry and create or open its org-roam note."
  (interactive)
  (require 'bibtex-completion)
  (let* ((candidates (bibtex-completion-candidates))
         (selected (completing-read "Paper note: " candidates nil t))
         (entry (cdr (assoc selected candidates)))
         (key (bibtex-completion-get-value "=key=" entry)))
    (orb-edit-note key)))

(defun rsr/search-brain ()
  "Full-text search across all org files in brain dir."
  (interactive)
  (consult-ripgrep my/brain-dir))

(defun rsr/open-paper ()
  "Browse BibTeX entries and open the associated PDF."
  (interactive)
  (require 'bibtex-completion)
  (let* ((candidates (bibtex-completion-candidates))
         (selected (completing-read "Open paper: " candidates nil t))
         (entry (cdr (assoc selected candidates)))
         (key (bibtex-completion-get-value "=key=" entry)))
    (bibtex-completion-open-pdf (list key))))

(defun rsr/org-insert-citation ()
  "Pick a BibTeX entry via completing-read and insert [cite:@key] at point.

Org-mode counterpart of `rsr/latex-insert-citation' (latex-config.el):
same `bibtex-completion' candidates over the Zotero library, but emits
native org-cite syntax instead of \\cite{key}."
  (interactive)
  (require 'bibtex-completion)
  (let* ((candidates (bibtex-completion-candidates))
         (choice     (completing-read "Cite: " candidates nil t))
         (entry      (cdr (assoc choice candidates)))
         (key        (bibtex-completion-get-value "=key=" entry)))
    (insert (format "[cite:@%s]" key))))

;;; Keybindings

;; M-s b search brain, C-c r p / n / c papers and org-ref link: keymaps.el.

;; In org buffers, C-c ] is the completing-read citation picker (mirrors the
;; LaTeX C-c ] in latex-config.el); org-ref-insert-link is C-c r c.
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c ]") #'rsr/org-insert-citation))

(provide 'roam-config)
;;; roam-config.el ends here
