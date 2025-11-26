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

(provide 'export-config)
