(require 'paths)
(require 'org-id)

(defun my-sanitize-filename (filename)
  "Replace invalid filename characters with underscores, strip leading/trailing dots and spaces."
  (let ((sanitized (replace-regexp-in-string "[\\/:*?\"<>| \[\]]" "_" filename)))
    (string-trim sanitized "[. ]+" "[. ]+"  )))

(defun my-create-new-leetcode-note ()
  "Create a new LeetCode problem note and save it to the leetcode directory."
  (interactive)
  (let* ((title-input (read-string "Problem title: "))
         (full-path (expand-file-name
                     (format "%s.org" (my-sanitize-filename title-input))
                     my/leetcode-dir)))
    (switch-to-buffer (generate-new-buffer (format "*LeetCode: %s*" title-input)))
    (org-mode)
    (insert (format "#+TITLE: %s\n" title-input))
    (insert (format ":PROPERTIES:\n:ID:       %s\n:Title:    %s\n:Created:  <%s>\n:END:\n\n"
                    (org-id-new) title-input (format-time-string "%Y-%m-%d")))
    (insert "* Information\n")
    (insert (format "  - Name: [%s]\n" title-input))
    (insert "  - Link: \n")
    (insert "  - Difficulty: [Easy/Medium/Hard]\n")
    (insert (format "  - Date: <%s>\n" (format-time-string "%Y-%m-%d")))
    (insert "  - Problem Type:\n\n")
    (insert "* Problem Description\n- \n\n")
    (insert "* Approach & Code\n\n")
    (insert "** Approach 1\n\n- \n\n")
    (insert "#+BEGIN_SRC python :results output :exports both\n\n#+END_SRC\n\n")
    (insert "#+RESULTS:\n\n")
    (insert "*** Complexity\n")
    (insert "   - Time: O(...)\n")
    (insert "   - Space: O(...)\n\n")
    (insert "*** Key Takeaway\n\n")
    (write-file full-path)
    (goto-char (point-min))
    (search-forward "* Problem Description")
    (forward-line 1)))

(defun my-org-insert-src-block-with-extras ()
  "Insert an org src block with :results output and leave point inside it."
  (interactive)
  (let ((language (read-string "Language: " nil nil "python")))
    (unless (string-empty-p language)
      (insert (format "#+BEGIN_SRC %s :results output :exports both\n" language))
      (let ((p (point)))
        (insert "\n#+END_SRC\n\n#+RESULTS:\n")
        (goto-char p)))))

(defun my-open-leetcode-folder ()
  "Open the LeetCode notes directory in dired."
  (interactive)
  (dired my/leetcode-dir))

(define-key global-map (kbd "C-c m") #'my-create-new-leetcode-note)
(define-key global-map (kbd "C-c o l") #'my-open-leetcode-folder)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c k") #'my-org-insert-src-block-with-extras))
