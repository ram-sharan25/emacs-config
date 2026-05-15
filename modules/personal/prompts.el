;;; prompts.el --- Load LLM system prompts from text files -*- lexical-binding: t; -*-

;;; Code:

(defvar my/prompts-dir
  (expand-file-name "prompts" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing prompt text files.")

(defun my/load-prompt (name)
  "Load prompt text from NAME.org in `my/prompts-dir'."
  (let ((file (expand-file-name (concat name ".org") my/prompts-dir)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (string-trim (buffer-string)))
      (user-error "Prompt file not found: %s" file))))

(defvar my/compile-system-prompt (my/load-prompt "compile-notes")
  "System prompt for resource note compilation.")

(provide 'prompts)
;;; prompts.el ends here
