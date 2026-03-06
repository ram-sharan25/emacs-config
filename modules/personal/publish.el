;;; publish.el --- Publish org files to Astro collections -*- lexical-binding: t; -*-

;;; Code:

(require 'ox-html)
(require 'htmlize)

(setq org-html-htmlize-output-type 'css)
(setq htmlize-output-type 'css)
(setq htmlize-css-name-prefix "org-")

(defvar rsr/portfolio-root
  (expand-file-name "~/Stillness/Development/learning-logs")
  "Root directory of Astro portfolio.")

(defconst rsr/--collections '("learnings" "blog" "thoughts")
  "Available Astro content collections.")

(defconst rsr/--internal-sections '("Resources" "Scratch" "Draft" "Information")
  "Org headings that are internal scaffolding and should not be exported.")

;;; --- Helpers ---

(defun rsr/--slug (title)
  "Generate a URL slug from TITLE."
  (downcase (replace-regexp-in-string "[^a-z0-9-]+" "-" title)))

(defun rsr/--find-published-file (slug)
  "Find the published .md file for SLUG across all collections.
Returns (collection . filepath) or nil."
  (cl-loop for collection in rsr/--collections
           for path = (expand-file-name
                       (format "src/content/%s/%s.md" collection slug)
                       rsr/portfolio-root)
           when (file-exists-p path)
           return (cons collection path)))

(defun rsr/--set-draft-in-file (md-file is-draft)
  "Set draft status in MD-FILE to IS-DRAFT (t = draft, nil = live)."
  (let* ((content (with-temp-buffer
                    (insert-file-contents md-file)
                    (buffer-string)))
         (new-content (replace-regexp-in-string
                       "^draft: \\(true\\|false\\)$"
                       (if is-draft "draft: true" "draft: false")
                       content)))
    (with-temp-file md-file
      (insert new-content))))

;;; --- HTML export ---

(defun rsr/--export-html (org-content)
  "Export ORG-CONTENT string to HTML body.
Tags internal sections with :noexport: in a temp buffer —
the original file is never modified."
  (with-temp-buffer
    (org-mode)
    (insert org-content)
    (goto-char (point-min))
    (while (re-search-forward
            (concat "^\\(\\*+\\) \\("
                    (mapconcat #'regexp-quote rsr/--internal-sections "\\|")
                    "\\):?\\s-*$")
            nil t)
      (end-of-line)
      (insert " :noexport:"))
    (let ((org-export-exclude-tags       '("noexport"))
          (org-html-head-include-default-style nil)
          (org-html-head-include-scripts  nil)
          (org-export-with-toc            nil)
          (org-export-with-section-numbers nil))
      (org-export-as 'html nil nil t))))

;;; --- Core publish ---

(defun rsr/publish-org-to-astro (org-file collection &optional draft)
  "Publish ORG-FILE to COLLECTION as Astro markdown.
With prefix arg, publish as draft (hidden on site)."
  (interactive
   (list
    (read-file-name "Org file: " nil (buffer-file-name) t nil
                    (lambda (name) (or (file-directory-p name)
                                       (string-match "\\.org\\'" name))))
    (completing-read "Collection: " rsr/--collections nil t)
    current-prefix-arg))

  (unless (file-exists-p org-file)
    (user-error "File does not exist: %s" org-file))

  (with-current-buffer (find-file-noselect org-file)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not an org file: %s" org-file))

    (let* ((title       (or (car (plist-get (org-export-get-environment) :title))
                            (file-name-base org-file)))
           (slug        (rsr/--slug title))
           (output-dir  (expand-file-name
                         (format "src/content/%s" collection)
                         rsr/portfolio-root))
           (output-file (expand-file-name (concat slug ".md") output-dir))
           (metadata    (rsr/--extract-metadata))
           (frontmatter (rsr/--generate-frontmatter metadata collection title draft)))

      (make-directory output-dir t)

      (let ((images    (rsr/--copy-images (current-buffer) org-file slug collection))
            (html-body (rsr/--export-html (buffer-string))))

        (dolist (img images)
          (setq html-body
                (replace-regexp-in-string
                 (regexp-quote (car img))
                 (cdr img)
                 html-body)))

        ;; Fix image paths — match any depth of ../
        (setq html-body
              (replace-regexp-in-string
               "\\(?:\\.\\./\\)+public/images/"
               "/images/"
               html-body))

        ;; Strip internal org id links — keep display text only
        (setq html-body
              (replace-regexp-in-string
               "<a href=\"[^\"]*#ID-[^\"]*\"[^>]*>\\([^<]+\\)</a>"
               "\\1"
               html-body))

        (with-temp-file output-file
          (insert frontmatter)
          (insert "<div class=\"org-content\">\n")
          (insert html-body)
          (insert "\n</div>\n"))

        (message "Published%s: %s → %s"
                 (if draft " (draft)" "") title output-file)
        (when (y-or-n-p "Open published file? ")
          (find-file output-file))))))

(defun rsr/publish-current-buffer (collection &optional draft)
  "Publish the current org buffer to COLLECTION.
With prefix arg, publish as draft."
  (interactive
   (list (completing-read "Collection: " rsr/--collections nil t)
         current-prefix-arg))
  (unless (buffer-file-name)
    (user-error "Buffer has no file"))
  (rsr/publish-org-to-astro (buffer-file-name) collection draft))

;;; --- Draft toggle ---

(defun rsr/publish-toggle-draft ()
  "Toggle draft status of the published file for the current org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((title  (or (car (plist-get (org-export-get-environment) :title))
                     (file-name-base (buffer-file-name))))
         (slug   (rsr/--slug title))
         (result (rsr/--find-published-file slug)))
    (unless result
      (user-error "No published file found for \"%s\"" title))
    (let* ((md-file  (cdr result))
           (content  (with-temp-buffer
                       (insert-file-contents md-file)
                       (buffer-string)))
           (is-draft (string-match "^draft: true$" content)))
      (rsr/--set-draft-in-file md-file (not is-draft))
      (message "Draft → %s (%s)"
               (if is-draft "false (live)" "true (hidden)")
               (file-name-nondirectory md-file)))))

;;; --- List published ---

(defvar rsr/list-published-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d")   #'rsr/list-published-toggle-draft)
    (define-key map (kbd "RET") #'rsr/list-published-open)
    (define-key map (kbd "g")   #'rsr/list-published)
    map)
  "Keymap for `rsr/list-published-mode'.")

(define-derived-mode rsr/list-published-mode tabulated-list-mode "Published"
  "Major mode for listing published Astro posts."
  (setq tabulated-list-format
        [("Collection" 12 t)
         ("Title"      48 t)
         ("Date"       12 t)
         ("Draft"       6 t)])
  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header))

(defun rsr/--parse-frontmatter (file)
  "Parse YAML frontmatter from FILE, return alist."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((result '()))
      (goto-char (point-min))
      (when (looking-at "---")
        (forward-line 1)
        (while (not (or (eobp) (looking-at "---")))
          (when (looking-at "^\\([a-z_]+\\): *\"?\\([^\"\\n]*\\)\"?$")
            (push (cons (match-string 1) (match-string 2)) result))
          (forward-line 1)))
      result)))

(defun rsr/list-published ()
  "Show all published Astro posts in a tabulated buffer."
  (interactive)
  (let ((buf     (get-buffer-create "*Published Posts*"))
        (entries '()))
    (dolist (collection rsr/--collections)
      (let ((dir (expand-file-name
                  (format "src/content/%s" collection)
                  rsr/portfolio-root)))
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t "\\.md\\'"))
            (let* ((fm    (rsr/--parse-frontmatter file))
                   (title (or (cdr (assoc "title" fm)) (file-name-base file)))
                   (date  (or (cdr (assoc "date"  fm)) ""))
                   (draft (or (cdr (assoc "draft" fm)) "false")))
              (push (list file (vector collection title date draft)) entries))))))
    (with-current-buffer buf
      (rsr/list-published-mode)
      (setq tabulated-list-entries entries)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun rsr/list-published-open ()
  "Open the published .md file at point."
  (interactive)
  (when-let ((file (tabulated-list-get-id)))
    (find-file file)))

(defun rsr/list-published-toggle-draft ()
  "Toggle draft status of the post at point and refresh the list."
  (interactive)
  (let* ((file    (tabulated-list-get-id))
         (content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
         (is-draft (string-match "^draft: true$" content)))
    (rsr/--set-draft-in-file file (not is-draft))
    (message "Draft → %s" (if is-draft "false (live)" "true (hidden)"))
    (rsr/list-published)))

;;; --- Metadata & frontmatter ---

(defun rsr/--extract-metadata ()
  "Extract metadata from current org buffer.
Reads #+DATE and #+TAGS keywords (blog/thoughts), then also
parses the LeetCode Information section (learnings)."
  (let ((date (or (org-entry-get nil "Created" t)
                  (format-time-string "%Y-%m-%d")))
        (tags '())
        (difficulty nil)
        (link nil))

    (when (string-match "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" date)
      (setq date (match-string 1 date)))

    ;; Org keywords (blog/thoughts)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+DATE:[ \t]+\\(.+\\)$" nil t)
        (let ((kw-date (string-trim (match-string 1))))
          (when (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" kw-date)
            (setq date (match-string 0 kw-date))))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+TAGS:[ \t]+\\(.+\\)$" nil t)
        (setq tags (mapcar (lambda (s)
                             (downcase (replace-regexp-in-string
                                        "\\s-+" "-" (string-trim s))))
                           (split-string (match-string 1) "[,:]")))))

    ;; LeetCode Information section (learnings)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Information" nil t)
        (let ((section-start (point))
              (section-end   (or (save-excursion
                                   (re-search-forward "^\\* [^I]" nil t))
                                 (point-max))))
          (save-excursion
            (goto-char section-start)
            (when (re-search-forward "Difficulty:\\s-*\\[\\([^]]+\\)\\]" section-end t)
              (setq difficulty (downcase (string-trim (match-string 1) nil "/")))))
          (save-excursion
            (goto-char section-start)
            (when (re-search-forward "Link:\\s-*\\[\\([^]]+\\)\\]" section-end t)
              (setq link (string-trim (match-string 1)))))
          (save-excursion
            (goto-char section-start)
            (when (re-search-forward "\\(?:Problem Type\\|Tags\\):\\s-*\\(.+\\)$" section-end t)
              (setq tags (append tags
                                 (mapcar (lambda (s)
                                           (downcase (replace-regexp-in-string
                                                      "\\s-+" "-" (string-trim s))))
                                         (split-string (match-string 1) ",")))))))))

    (when difficulty (push difficulty tags))

    (list :date date
          :tags (delete-dups tags)
          :difficulty difficulty
          :link link)))

(defun rsr/--copy-images (buf org-file slug collection)
  "Copy images referenced in BUF (ORG-FILE) to public/images/COLLECTION/SLUG/.
Returns alist of (old-path . new-path) for path updates."
  (let ((source-dir (file-name-directory org-file))
        (dest-dir   (expand-file-name
                     (format "public/images/%s/%s" collection slug)
                     rsr/portfolio-root))
        (images-copied '()))

    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "\\[\\[file:\\([^]]+\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)\\)\\]\\]"
                nil t)
          (let* ((rel-path   (match-string-no-properties 1))
                 (img-name   (file-name-nondirectory rel-path))
                 (source-img (expand-file-name rel-path source-dir))
                 (dest-img   (expand-file-name img-name dest-dir))
                 (new-path   (format "/images/%s/%s/%s" collection slug img-name)))
            (when (file-exists-p source-img)
              (make-directory dest-dir t)
              (copy-file source-img dest-img t)
              (push (cons rel-path new-path) images-copied)
              (message "Copied: %s" img-name))))))

    images-copied))

(defun rsr/--generate-frontmatter (metadata collection title &optional draft)
  "Generate YAML frontmatter from METADATA for COLLECTION using TITLE.
If DRAFT is non-nil, sets draft: true."
  (let ((date       (plist-get metadata :date))
        (tags       (plist-get metadata :tags))
        (difficulty (plist-get metadata :difficulty))
        (link       (plist-get metadata :link)))
    (concat
     "---\n"
     (format "title: \"%s\"\n" title)
     (when (string= collection "learnings") "topic: \"LeetCode\"\n")
     (format "date: %s\n" date)
     (format "tags: [%s]\n"
             (mapconcat (lambda (tag) (format "\"%s\"" tag)) tags ", "))
     (when difficulty (format "difficulty: \"%s\"\n" difficulty))
     (when link       (format "leetcode_link: \"%s\"\n" link))
     (format "draft: %s\n" (if draft "true" "false"))
     "---\n\n")))

(provide 'publish)
;;; publish.el ends here
