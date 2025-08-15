;; *** Exports to './output' directory

(defvar org-export-output-directory-prefix "output" "prefix of directory used for org-mode export")
(defadvice org-export-output-file-name (before org-add-export-dir activate )
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir org-export-output-directory-prefix)
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))


(defvar bp/org-publishing-file nil)
(defun org-publish-file--publishing-flag-around (f &rest args)
  (let ((bp/org-publishing-file t))
    (apply f args)))


(advice-add 'org-publish-file :around #'org-publish-file--publishing-flag-around)

(defun bp/org-html--format-image-relative (original-function source attribute info)
  "Modify the <img src=... /> link to point to path relative to html file instead of orgfile"
  (let ((org-file (buffer-file-name)))
    (cond ((and org-file
		org-export-output-directory-prefix
		(not (file-name-absolute-p source))
		(not bp/org-publishing-file))
	   (let* ((output-dir (format "%s/%s/"
				      (file-name-directory org-file)
				      org-export-output-directory-prefix))
		  (source (file-relative-name (file-truename source)
					      output-dir)))
	     (funcall original-function source attribute info)))
	  (t
	   (funcall original-function source attribute info)))))

(advice-add 'org-html--format-image :around #'bp/org-html--format-image-relative)


(setq backup-directory-alist '(("." . "/Users/rrimal/.emacs.d/backupfiles/")))
