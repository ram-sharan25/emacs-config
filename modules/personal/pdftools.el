;; pdf tools packages
(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link))

(use-package pdf-tools
  :defer t
  :init
  ;; NOTE: line numbering mode may hang pdf-view mode

  ;; http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools/
  ;; initialise
  ;;(pdf-tools-install)
  (setq pdf-view-use-scaling t) ;; from https://github.com/vedang/pdf-tools

  ;; open pdfs scaled to fit width
  (setq-default pdf-view-display-size 'fit-width)
  ;; automatically annotate highlights (Highlight using {C-c C-a h} )
  ;; (setq pdf-annot-activate-created-annotations t)
  (pdf-loader-install)

  :config
  (setf pdf-annot-list-format
	'((page . 3)
	  (label . 12)
	  (contents . 114)))

  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

  ;;; For Exporting all highlights from a pdf to org-mode
  (defun bp/pdf-annot-get-org-pdftools-link (file-name annot-id)
    (let* ((path (funcall org-pdftools-path-generator file-name))
	   (height (cond ((bound-and-true-p annot-id)
			  (nth 1 (pdf-annot-get
				  (pdf-info-getannot
				   annot-id
				   path)
				  'edges)))
			 (t
			  (/
			   (*
			    (or (image-mode-window-get
				 'vscroll)
				0)
			    (frame-char-height))
			   (float
			    (cdr (pdf-view-image-size)))))))
	   ;; pdf://path::page++height_percent;;annot_id\\|??search-string
	   (search-string "")
	   (link (concat

		  org-pdftools-link-prefix ":"
		  path
		  "::"
		  (number-to-string page)
		  "++"
		  (format "%.2f" height)
		  (if annot-id
		      (concat
		       ";;"
		       (symbol-name annot-id))
		    (if (not (string-empty-p search-string))
			(concat
			 org-pdftools-search-string-separator
			 (replace-regexp-in-string
			  " "
			  "%20"
			  search-string))
		      (message
		       "   Reminder: You haven't performed a isearch!") "")))))
      link))

  (defun pdf-tools-org-edges-to-region (edges)
    "Attempt to get 4-entry region \(LEFT TOP RIGHT BOTTOM\) from several EDGES.
We need this to import annotations and to get marked-up text, because
annotations are referenced by its edges, but functions for these tasks
need region."
    (let ((left0 (nth 0 (car edges)))
	  (top0 (nth 1 (car edges)))
	  (bottom0 (nth 3 (car edges)))
	  (top1 (nth 1 (car (last edges))))
	  (right1 (nth 2 (car (last edges))))
	  (bottom1 (nth 3 (car (last edges))))
	  (n (safe-length edges)))
      ;; we try to guess the line height to move
      ;; the region away from the boundary and
      ;; avoid double lines
      (list left0
	    (+ top0 (/ (- bottom0 top0) 3))
	    right1
	    (- bottom1 (/ (- bottom1 top1) 3)))))

    ;; From https://github.com/malb/emacs.d/blob/master/malb.org#pdf-viewer
  (defun malb/pdf-annot-export-as-org (compact)
    "Export annotations to Org Buffer."
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
	(insert (format "#+startup: indent\n\n"))
	(insert (format "source: [[%s][%s]]\n\n" source-file-name source-buffer))

	(mapc (lambda (annot) ;; traverse all annotations
		(progn
		  (let ((page (cdr (assoc 'page annot)))

			(highlighted-text
			 (if (pdf-annot-get annot 'markup-edges)
			     (let ((highlighted-text
				    (with-current-buffer source-buffer
				      (pdf-info-gettext (pdf-annot-get annot 'page)
							(pdf-tools-org-edges-to-region
							 (pdf-annot-get annot 'markup-edges))))))
			       (replace-regexp-in-string "\n" " " highlighted-text))
			   nil))
			(note (pdf-annot-get annot 'contents)))

		    (when (or highlighted-text (> (length note) 0))
		      (insert (if compact "- " "* "))
		      (insert (format "[[%s][pg. %s]]"
				      (bp/pdf-annot-get-org-pdftools-link
				       source-file-name
				       (pdf-annot-get-id annot))
				      page))

		      (when highlighted-text
			(insert (if compact (format ": “%s” " highlighted-text)
				  (concat "\n\n#+begin_quote\n"
					  highlighted-text
					  "\n#+end_quote"))))
		      (if (> (length note) 0)
			  (insert (if compact (format " %s\n" note)
				    (format "\n\n%s\n\n" note)))
			(insert (if compact "\n" "\n\n")))))))
	      (cl-remove-if
	       (lambda (annot) (member (pdf-annot-get-type annot) (list 'link)))
	       annots))
	)
      (pop-to-buffer target-buffer '(display-buffer-pop-up-window)))))
