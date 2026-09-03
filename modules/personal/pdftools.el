;; PDF Tools configuration
(defconst my/pdf-tools-epdfinfo-program "/opt/homebrew/bin/epdfinfo"
  "Canonical epdfinfo executable used by PDF Tools.")

(defconst my/pdf-tools-auto-mode-alist-entry
  '("\\.[pP][dD][fF]\\'" . my/pdf-tools-load)
  "Lazy PDF Tools entry for `auto-mode-alist'.")

(defconst my/pdf-tools-magic-mode-alist-entry
  '("%PDF" . my/pdf-tools-load)
  "Lazy PDF Tools entry for `magic-mode-alist'.")

(defvar my/pdf-tools-validated-p nil
  "Non-nil after the canonical epdfinfo executable validates successfully.")

(defvar my/pdf-tools-org-integrations-enabled-p nil
  "Non-nil after the Org PDF integrations have been enabled.")

(define-error 'my/pdf-tools-error "PDF Tools error")
(define-error 'my/pdf-tools-helper-missing
  "PDF Tools epdfinfo helper is missing" 'my/pdf-tools-error)
(define-error 'my/pdf-tools-helper-not-executable
  "PDF Tools epdfinfo helper is not executable" 'my/pdf-tools-error)
(define-error 'my/pdf-tools-helper-incompatible
  "PDF Tools epdfinfo helper is incompatible" 'my/pdf-tools-error)

(defconst my/pdf-tools--features-response-regexp
  (concat "\\`OK\n"
          "\\(?:no-\\)?case-sensitive-search:"
          "\\(?:no-\\)?writable-annotations:"
          "\\(?:no-\\)?markup-annotations\n"
          "\\.\nOK\n\\.\n\\'")
  "Expected response to epdfinfo `features' followed by `quit'.")

(defun my/pdf-tools--add-loader ()
  "Arrange for PDF Tools to be validated when a PDF is opened."
  (add-to-list 'auto-mode-alist my/pdf-tools-auto-mode-alist-entry)
  (add-to-list 'magic-mode-alist my/pdf-tools-magic-mode-alist-entry))

(defun my/pdf-tools--remove-loader ()
  "Remove the lazy PDF Tools mode-selection entries."
  (setq-default auto-mode-alist
                (remove my/pdf-tools-auto-mode-alist-entry auto-mode-alist)
                magic-mode-alist
                (remove my/pdf-tools-magic-mode-alist-entry magic-mode-alist)))

(defun my/pdf-tools--check-helper-protocol ()
  "Verify that the configured helper implements the epdfinfo protocol."
  (let ((output-buffer (generate-new-buffer " *epdfinfo-validation*"))
        status output)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "features\nquit\n")
            ;; A failure to execute at this process boundary means that the
            ;; configured file cannot serve as a compatible helper.  Preserve
            ;; the original `file-error' as structured error data.
            (condition-case err
                (setq status
                      (call-process-region
                       (point-min) (point-max)
                       my/pdf-tools-epdfinfo-program nil output-buffer nil))
              (file-error
               (signal 'my/pdf-tools-helper-incompatible
                       (list my/pdf-tools-epdfinfo-program err)))))
          (setq output (with-current-buffer output-buffer (buffer-string)))
          (unless (and (integerp status)
                       (zerop status)
                       (string-match-p my/pdf-tools--features-response-regexp
                                       output))
            (signal 'my/pdf-tools-helper-incompatible
                    (list my/pdf-tools-epdfinfo-program
                          (list :exit-status status :response output)))))
      (kill-buffer output-buffer))))

(defun my/pdf-tools--validate-helper ()
  "Validate the canonical epdfinfo helper and its PDF rendering support."
  (unless (file-exists-p my/pdf-tools-epdfinfo-program)
    (signal 'my/pdf-tools-helper-missing
            (list my/pdf-tools-epdfinfo-program)))
  (unless (file-executable-p my/pdf-tools-epdfinfo-program)
    (signal 'my/pdf-tools-helper-not-executable
            (list my/pdf-tools-epdfinfo-program)))
  (my/pdf-tools--check-helper-protocol)
  ;; Rendering the package's test PDF is the final compatibility boundary.
  ;; Preserve any lower-level failure as structured error data.
  (condition-case err
      (pdf-info-check-epdfinfo)
    (error
     (signal 'my/pdf-tools-helper-incompatible
             (list my/pdf-tools-epdfinfo-program err)))))

(defun my/pdf-tools--enable-org-integrations ()
  "Enable Org PDF integrations after epdfinfo has validated."
  (unless my/pdf-tools-org-integrations-enabled-p
    (setq org-noter-always-create-frame nil
          org-noter-auto-save-last-location t
          org-noter-kill-frame-at-session-end nil
          org-noter-hide-other nil
          org-noter-notes-window-location 'vertical-split)
    (require 'org-pdftools)
    (require 'org-noter)
    (org-pdftools-setup-link)
    (setq my/pdf-tools-org-integrations-enabled-p t)))

(defun my/pdf-tools-install (&rest _ignored)
  "Validate and activate PDF Tools without building or repairing epdfinfo."
  (unless my/pdf-tools-validated-p
    (require 'pdf-tools)
    (setq pdf-info-epdfinfo-program my/pdf-tools-epdfinfo-program)
    (my/pdf-tools--validate-helper)
    ;; Activation is outside the validation boundary, so its errors reach the
    ;; caller unchanged.
    (pdf-tools-install-noverify)
    (my/pdf-tools--remove-loader)
    (setq my/pdf-tools-validated-p t))
  (my/pdf-tools--enable-org-integrations))

(defun my/pdf-tools-load ()
  "Validate and load PDF Tools for the current PDF buffer."
  (my/pdf-tools-install))

(use-package org-pdftools
  :ensure t
  :after org
  :config
  ;; Link registration must not depend on opening or validating a PDF first.
  (org-pdftools-setup-link))

;; don't prompt for large PDF files
(setq large-file-warning-threshold nil)

;; org-noter calls pdf-view-current-overlay as a function but it's a macro — wrap it
(defun pdf-view-current-overlay ()
  (image-mode-window-get 'overlay))

(use-package nov
  :ensure t
  :defer t)

(use-package org-noter
  :ensure t
  :defer t)

(defun my/org-noter ()
  "Load the NOTER_DOCUMENT PDF into a background buffer without changing windows.
Call this from a notes heading with a NOTER_DOCUMENT property set.
Then manually split and display the PDF buffer where you want it."
  (interactive)
  (let ((doc (org-entry-get nil "NOTER_DOCUMENT" t)))
    (unless doc
      (user-error "No NOTER_DOCUMENT property found on this heading"))
    (my/pdf-tools-install)
    (find-file-noselect (expand-file-name doc))
    (message "PDF loaded: %s — use C-x 4 b or split to view it" doc)))

(use-package pdf-tools
  :pin manual
  :defer t
  :init
  (setq pdf-info-epdfinfo-program my/pdf-tools-epdfinfo-program
        pdf-view-use-scaling t)
  (setq-default pdf-view-display-size 'fit-width)
  (unless (advice-member-p #'my/pdf-tools-install 'pdf-tools-install)
    (advice-add 'pdf-tools-install :override #'my/pdf-tools-install))
  (my/pdf-tools--add-loader)

  :config

  ;; Annotation list format
  (setf pdf-annot-list-format
	'((page . 3)
	  (label . 12)
	  (contents . 114)))

  ;; Use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

  ;; Fine-grained scrolling functions
  (defun rsr/pdf-slight-up ()
    "Scroll PDF up slightly."
    (interactive)
    (pdf-view-scroll-up-or-next-page 12))

  (defun rsr/pdf-slight-down ()
    "Scroll PDF down slightly."
    (interactive)
    (pdf-view-scroll-down-or-previous-page 12))

(defun rsr/pdf-highlight-and-take-note ()
  (interactive)
  (let* ((pdf-annot-activate-created-annotations nil)
	 (result (pdf-view-active-region-text))
	 (final-string (if (listp result)
			   (apply #'concat result)
			 result)))
    (kill-new final-string)
    (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region nil)))
  (org-noter-insert-precise-note))
  ;; Generate org-pdftools link
(defun rsr/pdf-annot-get-org-pdftools-link (file-name annot)
  "Generate org-pdftools link for annotation."
  (let* ((path (funcall org-pdftools-path-generator file-name))
	 ;; ✅ Get page number directly from the annotation, not the current view.
	 (page (pdf-annot-get annot 'page))
	 (annot-id (pdf-annot-get-id annot))
	 ;; This is the simplified and more robust way to get the vertical position.
	 (height (if annot-id
		     ;; If we have an annotation, get its precise vertical edge.
		     (nth 1 (pdf-annot-get annot 'edges))
		   ;; If not, get the top of the visible part of the page.
		   (pdf-view-get-slice-region page))))
    ;; Use `format` for cleaner string building
    (format "pdf:%s::%d++%.2f%s"
	    path
	    page
	    height
	    (if annot-id
		(concat ";;" (symbol-name annot-id))
	      ""))))
  ;; Convert annotation edges to region
  (defun pdf-tools-org-edges-to-region (edges)
    "Convert annotation EDGES to region format."
    (let ((left0 (nth 0 (car edges)))
	  (top0 (nth 1 (car edges)))
	  (bottom0 (nth 3 (car edges)))
	  (top1 (nth 1 (car (last edges))))
	  (right1 (nth 2 (car (last edges))))
	  (bottom1 (nth 3 (car (last edges)))))
      (list left0
	    (+ top0 (/ (- bottom0 top0) 3))
	    right1
	    (- bottom1 (/ (- bottom1 top1) 3)))))

  ;; Export annotations to org
  (defun rsr/pdf-annot-export-as-org (compact)
    "Export PDF annotations to Org buffer. With prefix arg, use compact format."
    (interactive "P")
    (let* ((annots (sort (pdf-annot-getannots) 'pdf-annot-compare-annotations))
	   (source-buffer (current-buffer))
	   (source-buffer-name (file-name-sans-extension (buffer-name)))
	   (source-file-name (buffer-file-name source-buffer))
	   (target-buffer-name (format "*Notes for %s*" source-buffer-name))
	   (target-buffer (get-buffer-create target-buffer-name)))

      (with-current-buffer target-buffer
	(org-mode)
	(erase-buffer)

	(insert (format "#+title: Notes for %s\n" source-buffer-name))
	(insert "#+startup: indent\n\n")
	(insert (format "source: [[%s][%s]]\n\n" source-file-name source-buffer-name))

	(mapc (lambda (annot)
		(let ((page (cdr (assoc 'page annot)))
		      (highlighted-text
		       (if (pdf-annot-get annot 'markup-edges)
			   (let ((text (with-current-buffer source-buffer
					 (pdf-info-gettext
					  (pdf-annot-get annot 'page)
					  (pdf-tools-org-edges-to-region
					   (pdf-annot-get annot 'markup-edges))))))
			     (replace-regexp-in-string "\n" " " text))
			 nil))
		      (note (pdf-annot-get annot 'contents)))

		  (when (or highlighted-text (> (length note) 0))
		    (insert (if compact "- " "* "))
		    (insert (format "[[%s][pg. %s]]"
				    (rsr/pdf-annot-get-org-pdftools-link
				     source-file-name
				     annot)
				    page))

		    (when highlighted-text
		      (insert (if compact
				  (format ": "%s" " highlighted-text)
				(concat "\n\n#+begin_quote\n"
					highlighted-text
					"\n#+end_quote"))))

		    (if (> (length note) 0)
			(insert (if compact
				    (format " %s\n" note)
				  (format "\n\n%s\n\n" note)))
		      (insert (if compact "\n" "\n\n"))))))

	      (cl-remove-if
	       (lambda (annot) (member (pdf-annot-get-type annot) '(link)))
	       annots)))

      (pop-to-buffer target-buffer '(display-buffer-pop-up-window))))

  (defun rsr/pdf-copy-text-and-link ()
    "Highlight selection and copy text, link, and annotation note to clipboard."
    (interactive)
    (let* ((text (apply #'concat (pdf-view-active-region-text)))
           (annot (pdf-annot-add-highlight-markup-annotation
                   (pdf-view-active-region nil)))
           (link (rsr/pdf-annot-get-org-pdftools-link (buffer-file-name) annot))
           (note (pdf-annot-get annot 'contents))
           (clip (concat (format "[[%s][pg. %d]]\n" link (pdf-view-current-page))
                         (format "#+begin_quote\n%s\n#+end_quote" text)
                         (when (and note (> (length note) 0))
                           (format "\n\n%s" note)))))
      (kill-new clip)
      (message "Copied text + link to clipboard")))

  (defun rsr/pdf-copy-link-only ()
    "Highlight selection, create annotation, and copy only the org link.
No quote, no note — just `[[pdf:...][pg. N]]' to the kill ring."
    (interactive)
    (let* ((pdf-annot-activate-created-annotations nil)
           (annot (pdf-annot-add-highlight-markup-annotation
                   (pdf-view-active-region nil)))
           (link (rsr/pdf-annot-get-org-pdftools-link (buffer-file-name) annot))
           (clip (format "[[%s][pg. %d]]" link (pdf-view-current-page))))
      (kill-new clip)
      (message "Copied link only: %s" clip)))

  ;; Key bindings for PDF mode
  (bind-keys :map pdf-view-mode-map
	     ("x" . rsr/pdf-slight-up)
	     ("z" . rsr/pdf-slight-down)
	     ("C-c a" . rsr/pdf-highlight-and-take-note)
	     ("C-c e" . rsr/pdf-annot-export-as-org)
             ("C-c Y" . rsr/pdf-copy-text-and-link)
             ("C-c y" . rsr/pdf-copy-link-only)
             ("C-c x" . pdf-annot-delete)))
