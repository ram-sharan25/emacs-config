;; *** Exports to 'Brain/output' directory

(require 'paths)
(require 'org)



(unless (file-directory-p my/export-output-dir)
  (make-directory my/export-output-dir t))

(defun my/get-export-filename (original-name subtreep)
  "Determine the export filename based on context.
   If SUBTREEP is true, use the Heading Title.
   Otherwise, use the File Title (or fallback to filename)."
  (let ((base-name nil))
    (if subtreep
        ;; Subtree Export: Use Heading Title
        (let ((heading-str (org-get-heading t t t t)))
          (setq base-name heading-str))
      ;; File Export: Use #+TITLE or File Name (fallback to buffer name)
      (setq base-name (or (cadar (org-collect-keywords '("TITLE")))
                          (file-name-base (or (buffer-file-name) (buffer-name))))))

    ;; Clean: Remove TODO keywords and Tags manually (extra safety)
    (let ((todo-re (concat "^\\(" (mapconcat 'identity org-todo-keywords-1 "\\|") "\\) ")))
      (setq base-name (replace-regexp-in-string todo-re "" (or base-name ""))))
    (setq base-name (replace-regexp-in-string ":[[:alnum:]_@#%]+:$" "" base-name))

    ;; Sanitize: Remove illegal characters, replace spaces with underscores
    (setq base-name (replace-regexp-in-string "[^a-zA-Z0-9-_ ]" "" base-name))
    (setq base-name (replace-regexp-in-string " " "_" base-name))
    base-name))

(defadvice org-export-output-file-name (around my/centralized-export-output activate)
  "Force all exports to `my/export-output-dir` and use Title-based filenames."
  (let* ((extension (ad-get-arg 0))
         (subtreep (ad-get-arg 1))
         (pub-dir (ad-get-arg 2))
         ;; Determine new filename base
         (new-base (my/get-export-filename (buffer-name) subtreep))
         ;; Force output directory
         (final-dir my/export-output-dir))

    (unless (file-directory-p final-dir)
      (make-directory final-dir t))

    (setq ad-return-value (expand-file-name (concat new-base extension) final-dir))))

;; Image handling for HTML export (Relative to the new output dir)
(defun bp/org-html--format-image-relative (original-function source attribute info)
  "Modify the <img src=... /> link to point to path relative to html file."
  (let ((org-file (buffer-file-name)))
    (cond ((and org-file
                (not (file-name-absolute-p source)))
           (let* ((source-absolute (file-truename source))
                  (relative-path (file-relative-name source-absolute my/export-output-dir)))
             (funcall original-function relative-path attribute info)))
          (t
           (funcall original-function source attribute info)))))

(advice-add 'org-html--format-image :around #'bp/org-html--format-image-relative)

(setq backup-directory-alist '(("." . "/Users/rrimal/.emacs.d/backupfiles/")))


;; Disable export of drawers (like :THOUGHTS:, :PROPERTIES:, :LOGBOOK:)
(setq org-export-with-drawers nil)

;; -------------------------------------------------------------------------
;; ODT / DOCX Export via Pandoc
;; -------------------------------------------------------------------------
(use-package ox-pandoc
  :ensure t
  :after org
  :config
  ;; Define standard options for docx export if needed, e.g. reference doc
  ;; (setq org-pandoc-options-for-docx '((reference-doc . "~/.emacs.d/data/custom-reference.docx")))

  ;; Add 'pandoc' as a valid export dispatch option
  (add-to-list 'org-export-backends 'pandoc))

;; Open PDF exports in a split window using pdf-tools
(defun my/open-pdf-in-split (file)
  "Open FILE in a split window to the right."
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (find-file file))

(add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file _link) (my/open-pdf-in-split file))))

;; -------------------------------------------------------------------------
;; Auto mermaid → LaTeX figure via org-native attrs (global, all files)
;;
;; On C-c C-c of any mermaid block:
;;   1. Rewrites [[file:X]] → [[data:X]] in results (Emacs inline display)
;;   2. Removes #+RESULTS: line (so the image link exports directly)
;;   3. Inserts #+CAPTION / #+NAME / #+ATTR_LATEX above the image (idempotent)
;; -------------------------------------------------------------------------
(defun my/mermaid-latex-attrs ()
  "After a mermaid src block executes, add org-native figure attributes.
Replaces all result content with attrs + data: image link."
  (let* ((info (ignore-errors
                 (save-excursion
                   (org-babel-goto-src-block-head)
                   (org-babel-get-src-block-info 'light))))
         (lang (and info (car info)))
         (file (and info (cdr (assq :file (nth 2 info))))))
    (when (and (equal lang "mermaid") file)
      (let* ((fig-name (file-name-nondirectory file))
             (base (file-name-sans-extension fig-name))
             (label (replace-regexp-in-string "_" "-" base))
             (caption (capitalize (replace-regexp-in-string "_" " " base))))
        (save-excursion
          ;; 1. Ensure :exports results (not none) so image link exports
          (org-babel-goto-src-block-head)
          (when (re-search-forward ":exports +none" (line-end-position) t)
            (replace-match ":exports results"))

          ;; 2. Replace all result content with attrs + data: link
          (org-babel-goto-src-block-head)
          (when-let ((rp (org-babel-where-is-src-block-result)))
            (goto-char rp)
            (forward-line) ;; past #+RESULTS:
            (let ((content-start (point))
                  (content-end (org-babel-result-end)))
              (delete-region content-start content-end)
              (insert (format "#+CAPTION: %s\n#+NAME: fig:%s\n#+ATTR_LATEX: :width 0.8\\textwidth :height 1.0\\textheight :options keepaspectratio :float t\n[[data:%s]]\n"
                              caption label fig-name)))))))))

(add-hook 'org-babel-after-execute-hook #'my/mermaid-latex-attrs)

(provide 'export-config)
