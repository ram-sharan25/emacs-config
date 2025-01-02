(setq org-html-head-include-default-style nil)

(defun bp/org-publish--add-setupfile (&rest args)
  (goto-char (point-min))
  (search-forward "#+title")
  (beginning-of-line)
  (insert "#+setupfile: /Users/rrimal/.emacs.d/modules/git-modules/src/comfy_inline/comfy_inline.theme\n"))

(use-package ox
  :defer t
  :config
  (add-hook 'org-export-before-processing-functions #'bp/org-publish--add-setupfile))


(use-package org
  :config
  (setq org-preview-latex-image-directory "/tmp/ltximg/")
  (setq org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :background "Transparent")

  (defun bp/adjust-latex-previews-scale ()
    "Adjust the size of latex preview fragments when changing the
buffer's text scale."
    (pcase major-mode
      ('latex-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
	 (if (eq (overlay-get ov 'category)
		 'preview-overlay)
	     (bp/latex-preview--resize-fragment ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
	 (if (eq (overlay-get ov 'org-overlay-type)
		 'org-latex-overlay)
	     (bp/latex-preview--resize-fragment ov))))))

  (defun bp/latex-preview--resize-fragment (ov)
    (overlay-put
     ov 'display
     (cons 'image
	   (plist-put
	    (cdr (overlay-get ov 'display))
	    :scale (* 2 (/ (frame-char-height) 12) (expt text-scale-mode-step text-scale-mode-amount))))))

  (add-hook 'text-scale-mode-hook #'bp/adjust-latex-previews-scale)
  (defadvice org-latex-preview (after bp/org-latex-preview--adjust-scale activate)
    (bp/adjust-latex-previews-scale)))
