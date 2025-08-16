;; PDF Tools configuration
(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link))

(use-package pdf-tools
  :pin manual
  :defer t
  :init
  (setq pdf-view-use-scaling t)
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install)

  :config
  (require 'org-pdftools)

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

  ;; Key bindings for PDF mode
  (bind-keys :map pdf-view-mode-map
	     ("x" . rsr/pdf-slight-up)
	     ("z" . rsr/pdf-slight-down)
	     ("C-c a" . rsr/pdf-highlight-and-take-note)
	     ("C-c e" . rsr/pdf-annot-export-as-org)))
