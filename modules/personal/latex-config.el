;;; latex-config.el --- LaTeX writing: cite, compile, viewer -*- lexical-binding: t; -*-
;;; Commentary:
;; Citation picker for LaTeX buffers. Reuses bibtex-completion (already
;; configured against ~/Stillness/Brain/resources/Zotero/zotero_ref.bib in
;; roam-config.el), so the same Zotero library powers org-roam notes and
;; tex citations.
;;
;;; Code:

(require 'paths)

(defun rsr/latex-insert-citation ()
  "Pick a BibTeX entry via completing-read and insert \\cite{key} at point.

Mirrors the picker pattern used in roam-config.el and gtd-config.el:
bibtex-completion is loaded as a library, and we build the interactive
front-end ourselves rather than relying on helm-bibtex/ivy-bibtex."
  (interactive)
  (require 'bibtex-completion)
  (let* ((candidates (bibtex-completion-candidates))
         (choice     (completing-read "Cite: " candidates nil t))
         (entry      (cdr (assoc choice candidates)))
         (key        (bibtex-completion-get-value "=key=" entry)))
    (insert (format "\\cite{%s}" key))))

;; Bind in builtin tex-mode parent map so latex-mode + plain-tex-mode + tex-mode
;; all inherit it. Without this, chapter fragments (no \documentclass) land in
;; plain-tex-mode where C-c ] is `latex-close-block', which errors with
;; "Couldn't find unended \\begin" on fragment files.
(with-eval-after-load 'tex-mode
  (define-key tex-mode-map      (kbd "C-c ]") #'rsr/latex-insert-citation)
  (define-key latex-mode-map    (kbd "C-c ]") #'rsr/latex-insert-citation)
  (define-key plain-tex-mode-map (kbd "C-c ]") #'rsr/latex-insert-citation))

;; Same binding for AUCTeX if/when it is installed.
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c ]") #'rsr/latex-insert-citation))

(provide 'latex-config)
;;; latex-config.el ends here
