(defvar my-leetcode-notes-default-directory
  "~/Stillness/Personal/Software/Projects/NeetCode/"
  "Default directory to save LeetCode notes.")


(defun my-sanitize-filename (filename)
  "Sanitize FILENAME by replacing potentially invalid characters with underscores."
  (replace-regexp-in-string "[\\/:*?\"<>| \[\]]" "_" filename))


(defun my-create-new-leetcode-note ()
  "Prompt for a LeetCode problem title, create an Org note, and save it with that title."
  (interactive)
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
    (insert (format ":PROPERTIES:\n:Title: %s\n:Created: <%s>\n:END:\n\n"
		    title-input (format-time-string "%Y-%m-%d")))

    ;; Insert template
    (insert (format "* [%s] - LeetCode [Problem Number]\n" title-input))
    (insert "  - Link: [Problem Link Here]\n")
    (insert "  - Difficulty: [Easy/Medium/Hard]\n")
    (insert (format "  - Date: <%s>\n\n" (format-time-string "%Y-%m-%d")))
    (insert "** Approach & Code\n\n")
    (insert "*** Approach 1 \n\n")
    (insert "   (Briefly describe your main idea or approach here)\n\n")
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
    (insert "** Key Takeaway / Learning\n\n")

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
(define-key global-map (kbd "C-c n") #'my-create-new-leetcode-note)
;; Note: C-c followed by a letter is often reserved for major modes.
;; Using Super key or a personal prefix map is safer for truly global bindings.
