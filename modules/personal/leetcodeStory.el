(require 'paths)
(require 'org-id)


(defvar my-leetcode-notes-default-directory my/leetcode-dir
  "Default directory to save LeetCode notes, using standardized path.")
(defvar-local my-is-leetcode-note-buffer nil
  "Non-nil if the current buffer is a new LeetCode note buffer.")

(defun my-sanitize-filename (filename)
  "Sanitize FILENAME by replacing potentially invalid characters with underscores."
  (replace-regexp-in-string "[\\/:*?\"<>| \[\]]" "_" filename))

;; Define the path for your index file
(defvar leetcode-index-file my/leetcode-index-file
  "The full path to the LeetCode index file, using standardized path.")

(defun my-create-new-leetcode-note ()
  "Prompt for a LeetCode problem title, create an Org note, and save it with that title."

  (interactive)
  (setq-local my-is-leetcode-note-buffer t)
  (let* ((title-input (read-string "Enter LeetCode Problem Title: "))
	 (sanitized-title (my-sanitize-filename title-input))
	 (filename (format "%s.org" sanitized-title))
	 (full-path (expand-file-name filename my-leetcode-notes-default-directory))
	 (buffer-name (format "*LeetCode: %s*" title-input)))
    (switch-to-buffer (generate-new-buffer buffer-name))
    (org-mode)
    (setq my-is-leetcode-note-buffer nil) ;; Mark as saved manually

    ;; Insert metadata
    (insert (format "#+TITLE: %s\n" title-input))
    (insert (format ":PROPERTIES:\n:ID:       %s\n:Title:    %s\n:Created:  <%    s>\n:END:\n\n"
		(org-id-new) ; Call the function to generate an ID
		title-input
		(format-time-string "%Y-%m-%d")))
    ;; Insert template
    (insert "* Information \n" )
    (insert (format "  - Name: [%s] \n" title-input))
    (insert "  - Link: [Problem Link Here] \n")
    (insert "  - Difficulty: [Easy/Medium/Hard]\n")
    (insert (format "  - Date: <%s>\n" (format-time-string "%Y-%m-%d")))
    (insert "  - Problem Type:     \n\n")
    (insert "* Problem Decription\n")
    (insert "- (Briefly describe your problem here ) \n\n")  (insert "- \n\n")
    (insert "* Approach & Code\n\n")
    (insert "** Approach 1 \n\n")
    (insert "- (Briefly describe your main idea or approach here) \n\n")
    (insert "#+BEGIN_SRC python :results output :exports both\n")
    (insert "# Your Python code here\n")
    (insert "# print(\"Solution output...\")\n")
    (insert "# result = your_function_call()\n")
    (insert "# print(f\"Result: {result}\")\n")
    (insert "#+END_SRC\n\n")
    (insert "#+RESULTS:\n\n")
    (insert "*** Problem Complexity\n")
    (insert "   - Time Complexity: O(...)\n")
    (insert "   - Space Complexity: O(...)\n\n")
    (insert "*** Key Takeaway / Learning\n\n")

    ;; Save to file immediately
    (write-file full-path)
    (set-visited-file-name full-path t)
    (message "✅ LeetCode note created and saved to: %s" full-path)))





;; --- 5. Advice to Modify 'save-buffer' Behavior ---
(defun my-save-buffer-advice (original-save-function &rest args)
  "Advice for 'save-buffer'.
If it's a new LeetCode note buffer, use custom save logic. Otherwise, normal save."
  ;; Check if it's a new buffer (no file associated) AND our special LeetCode buffer
  (if (and (not (buffer-file-name (current-buffer)))
	   (buffer-local-value 'my-is-leetcode-note-buffer (current-buffer)))
      (my-save-leetcode-note-buffer-as-title) ; Use our custom save
    (apply original-save-function args))) ; Proceed with normal save behavior

;; Add the advice around the 'save-buffer' command
(advice-add 'save-buffer :around #'my-save-buffer-advice)

;; --- 6. Global Keybinding for Creating a New Note ---
;; Bind to "Super-n" (Cmd-n on macOS, WindowsKey-n on Windows/Linux)
;; You can change "s-n" to your preferred global shortcut.
;; For example, if you have a personal prefix map like "C-c p", you could use "C-c p n".
;;(global-set-key (kbd "s-n") #'my-create-new-leetcode-note) ; "s" stands for Super/Cmd/Windows key
;; Alternatively, for a "C-c" based binding (less "global" but common for user commands):
(define-key global-map (kbd "C-c m") #'my-create-new-leetcode-note)
;; Note: C-c followed by a letter is often reserved for major modes.
;; Using Super key or a personal prefix map is safer for truly global bindings.



(defun my-org-insert-src-block-with-extras ()
  "Insert an Org mode source block, prompting for language,
and including :results output, :exports both, and a #+RESULTS: block.
Leaves point on the line inside the source block."
  (interactive)
  (let ((language (read-string "Source code language (e.g., python): " nil nil "python")))
    (if (string-empty-p language)
	(message "No language specified. Source block not inserted.")
      (progn
	;; Insert the #+BEGIN_SRC line with language and default headers
	(insert (format "#+BEGIN_SRC %s :results output :exports both\n" language))
	;; Save point for cursor positioning, then insert the rest
	(let ((code-line-point (point)))
	  (insert "\n") ; Empty line for code
	  (insert "#+END_SRC\n\n")  ; #+END_SRC followed by two newlines
	  (insert "#+RESULTS:\n")   ; The #+RESULTS: block
	  (goto-char code-line-point) ; Move cursor to the empty line inside the SRC block
	  )))))

;; (Re)Define the keybinding specifically for Org mode
;; This ensures C-c k is only active in Org buffers.
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c k") #'my-org-insert-src-block-with-extras))

(defun leetcode-rebuild-index ()
  "Create an Org index of all files in `my-leetcode-notes-default-directory`."
  (interactive)
  (let* ((directory my-leetcode-notes-default-directory)
	 (index-file (expand-file-name "Index.org" directory))
	 ;; Match all files except the index itself
	 (files (seq-filter
		 (lambda (f)
		   (and (file-regular-p f)
			(not (string-equal (file-truename f) (file-truename index-file)))))
		 (directory-files directory t "\\.\\(org\\|py\\)$"))))
    (with-temp-file index-file
      (insert "#+TITLE: File Index\n\n* Indexed Files\n")
      (dolist (file (sort files #'string<))
	(let ((rel-path (file-relative-name file directory))
	      (display-name (file-name-nondirectory file)))
	  (insert (format "- [[file:%s][%s]]\n" rel-path display-name)))))
    (message "✅ Index created at: %s" index-file)))


(defun leetcode-open-index ()
  "Open the LeetCode index file."
  (interactive)
  (find-file my/leetcode-index-file))

;; Define a keybinding to open the index
(define-key global-map (kbd "C-c o l") #'leetcode-open-index)


;; Define a keybinding to run the indexer
(define-key global-map (kbd "C-c b l") #'leetcode-rebuild-index)
