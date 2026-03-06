;;; publish.el --- Publish org files to Astro collections -*- lexical-binding: t; -*-

;;; Code:

(require 'ox-html)
(require 'htmlize)

(setq org-html-htmlize-output-type 'css)
(setq htmlize-output-type 'css)
(setq htmlize-css-name-prefix "org-")

(defvar rsr/portfolio-root
  (expand-file-name "~/Stillness/Development/learning-logs/portfolio")
  "Root directory of Astro portfolio.")

(defun rsr/publish-org-to-astro (org-file collection)
  "Publish ORG-FILE to COLLECTION (learnings/blog/thoughts) as Astro markdown."
  (interactive
   (list
    (read-file-name "Org file: " nil (buffer-file-name) t nil
                    (lambda (name) (string-match "\\.org\\'" name)))
    (completing-read "Collection: " '("learnings" "blog" "thoughts") nil t)))

  (unless (file-exists-p org-file)
    (user-error "File does not exist: %s" org-file))

  (with-current-buffer (find-file-noselect org-file)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not an org file: %s" org-file))

    (let* ((title (or (car (plist-get (org-export-get-environment) :title))
                      (file-name-base org-file)))
           (slug (downcase (replace-regexp-in-string "[^a-z0-9-]+" "-" title)))
           (output-dir (expand-file-name
                        (format "src/content/%s" collection)
                        rsr/portfolio-root))
           (output-file (expand-file-name (concat slug ".md") output-dir))
           (metadata (rsr/--extract-metadata))
           (frontmatter (rsr/--generate-frontmatter metadata collection)))

      (make-directory output-dir t)

      (let ((images (rsr/--copy-images org-file slug collection)))
        (let* ((org-html-head-include-default-style nil)
               (org-html-head-include-scripts nil)
               (org-export-with-toc nil)
               (org-export-with-section-numbers nil)
               (html-body (org-export-as 'html nil nil t)))

          (dolist (img images)
            (setq html-body
                  (replace-regexp-in-string
                   (regexp-quote (car img))
                   (cdr img)
                   html-body)))

          (setq html-body
                (replace-regexp-in-string
                 "\\.\\./\\.\\./\\.\\./public/images/"
                 "/images/"
                 html-body))

          (with-temp-file output-file
            (insert frontmatter)
            (insert "<div class=\"org-content\">\n")
            (insert html-body)
            (insert "\n</div>\n"))

          (message "Published: %s → %s" title output-file)
          (when (y-or-n-p "Open published file? ")
            (find-file output-file)))))))

(defun rsr/--extract-metadata ()
  "Extract metadata from current org buffer."
  (let ((date (or (org-entry-get nil "Created" t)
                  (format-time-string "%Y-%m-%d")))
        (tags '())
        (difficulty nil)
        (link nil))

    (when (string-match "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" date)
      (setq date (match-string 1 date)))

    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Information" nil t)
        (let ((end (or (save-excursion (re-search-forward "^\\* [^I]" nil t))
                       (point-max))))
          (when (re-search-forward "Difficulty:\\s-*\\[\\([^]]+\\)\\]" end t)
            (setq difficulty (downcase (string-trim (match-string 1) nil "/"))))
          (goto-char (match-beginning 0))
          (when (re-search-forward "Link:\\s-*\\[\\([^]]+\\)\\]" end t)
            (setq link (string-trim (match-string 1))))
          (goto-char (match-beginning 0))
          (when (re-search-forward "\\(?:Problem Type\\|Tags\\):\\s-*\\(.+\\)$" end t)
            (setq tags (mapcar (lambda (s)
                                 (downcase (replace-regexp-in-string
                                            "\\s-+" "-" (string-trim s))))
                               (split-string (match-string 1) ",")))))))

    (when difficulty (push difficulty tags))

    (list :date date
          :tags (delete-dups tags)
          :difficulty difficulty
          :link link)))

(defun rsr/--copy-images (org-file slug collection)
  "Copy images from ORG-FILE directory to public/images/COLLECTION/SLUG/.
Returns alist of (old-path . new-path) for path updates."
  (let ((source-dir (file-name-directory org-file))
        (dest-dir (expand-file-name
                   (format "public/images/%s/%s" collection slug)
                   rsr/portfolio-root))
        (images-copied '()))

    (make-directory dest-dir t)

    (with-current-buffer (find-file-noselect org-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\[\\[file:\\([^]]+\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)\\)\\]\\]" nil t)
          (let* ((rel-path (match-string-no-properties 1))
                 (img-name (file-name-nondirectory rel-path))
                 (source-img (expand-file-name rel-path source-dir))
                 (dest-img (expand-file-name img-name dest-dir))
                 (new-path (format "/images/%s/%s/%s" collection slug img-name)))
            (when (file-exists-p source-img)
              (copy-file source-img dest-img t)
              (push (cons rel-path new-path) images-copied)
              (message "Copied: %s" img-name))))))

    images-copied))

(defun rsr/--generate-frontmatter (metadata collection)
  "Generate YAML frontmatter from METADATA for COLLECTION."
  (let ((date (plist-get metadata :date))
        (tags (plist-get metadata :tags))
        (difficulty (plist-get metadata :difficulty))
        (link (plist-get metadata :link)))
    (concat
     "---\n"
     (format "title: \"%s\"\n" (or (car (plist-get (org-export-get-environment) :title))
                                   (file-name-base (buffer-file-name))))
     (when (string= collection "learnings")
       "topic: \"LeetCode\"\n")
     (format "date: %s\n" date)
     (format "tags: [%s]\n"
             (mapconcat (lambda (tag) (format "\"%s\"" tag)) tags ", "))
     (when difficulty (format "difficulty: \"%s\"\n" difficulty))
     (when link (format "leetcode_link: \"%s\"\n" link))
     "draft: false\n"
     "---\n\n")))

(provide 'publish)
;;; publish.el ends here
