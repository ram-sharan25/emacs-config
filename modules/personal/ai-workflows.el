;;; ai-workflows.el --- AI-assisted workflows using gptel -*- lexical-binding: t; -*-

;;; Code:

(require 'paths)
(require 'prompts)

;;; --- Resource Note Compilation ---

(defun my/resource--split-raw-sections (raw-start raw-end)
  "Split raw notes between RAW-START and RAW-END into sections by ** headings.
Returns a list of strings. If no ** headings, returns the whole content as one section.
Sub-headings are flattened to --- markers so the LLM doesn't confuse them with output."
  (let ((sections '())
        (current-start raw-start))
    (save-excursion
      (goto-char raw-start)
      ;; Skip past any properties drawer at the start
      (when (looking-at "^:PROPERTIES:")
        (re-search-forward "^:END:" raw-end t)
        (forward-line 1)
        (setq current-start (point)))
      ;; Find each ** heading boundary
      (while (re-search-forward "^\\*\\* " raw-end t)
        (let ((heading-start (line-beginning-position)))
          ;; Save content before this heading (if any real content exists)
          (when (> heading-start current-start)
            (let ((text (string-trim
                         (buffer-substring-no-properties current-start heading-start))))
              (unless (string-empty-p text)
                (push text sections))))
          (setq current-start heading-start)))
      ;; Collect final section
      (let ((text (string-trim
                   (buffer-substring-no-properties current-start raw-end))))
        (unless (string-empty-p text)
          (push text sections))))
    ;; Flatten any remaining sub-headings (*** etc.) within each section
    (mapcar (lambda (s)
              (replace-regexp-in-string "^\\*+ \\(.*\\)" "--- \\1 ---" s))
            (nreverse sections))))

(defun my/resource--compile-sections (sections target-buf insert-marker remaining-count)
  "Process SECTIONS sequentially via gptel, inserting results into TARGET-BUF.
INSERT-MARKER tracks where to insert next. REMAINING-COUNT tracks progress."
  (if (null sections)
      ;; All sections done
      (with-current-buffer target-buf
        (save-excursion
          (goto-char (point-min))
          (org-entry-put nil "COMPILE_STATE" "compiled"))
        (save-buffer)
        (message "Done. Review and edit compiled notes."))
    ;; Process next section
    (let ((section (car sections))
          (rest (cdr sections)))
      (message "Compiling section %d of %d..."
               (- remaining-count (length rest)) remaining-count)
      (gptel-request section
        :system my/compile-system-prompt
        :callback
        (lambda (response info)
          (if (not response)
              (progn
                (message "Section failed: %s" (plist-get info :status))
                ;; Continue with remaining sections despite failure
                (my/resource--compile-sections
                 rest target-buf insert-marker remaining-count))
            (with-current-buffer target-buf
              (save-excursion
                (goto-char insert-marker)
                ;; Fix malformed headings: **Title → ** Title
                (let ((fixed (replace-regexp-in-string
                              "^\\(\\*+\\)\\([^ *\n]\\)" "\\1 \\2" response)))
                  (insert fixed "\n\n"))
                (set-marker insert-marker (point))))
            ;; Process next section
            (my/resource--compile-sections
             rest target-buf insert-marker remaining-count)))))))

(defun my/resource-compile-notes ()
  "Use gptel to compile * Raw Notes into atomic headings under * Compiled Notes.
Splits raw notes by ** sub-headings and processes each section separately.
Uses the currently active gptel backend/model. Switch model first if needed
\(M-m a G for Gemini, M-m a O for GitHub Copilot)."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in an org-mode buffer"))
  ;; Clear any existing compiled notes before re-running
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Compiled Notes:?\n" nil t)
      (let ((start (point))
            (end (if (re-search-forward "^\\* " nil t)
                     (line-beginning-position)
                   (point-max))))
        (delete-region start end))))
  ;; Extract and split raw notes
  (let* ((raw-bounds
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "^\\* Raw Notes" nil t)
                (let* ((end (save-excursion (org-end-of-subtree t) (point)))
                       (start (progn (forward-line 1) (point))))
                  (cons start end))
              (user-error "No '* Raw Notes' heading found"))))
         (sections (my/resource--split-raw-sections
                    (car raw-bounds) (cdr raw-bounds))))
    (when (null sections)
      (user-error "* Raw Notes is empty — add your session notes first"))
    (save-excursion
      (goto-char (point-min))
      (org-entry-put nil "COMPILE_STATE" "compiling"))
    (let ((insert-marker
           (save-excursion
             (goto-char (point-min))
             (if (re-search-forward "^\\* Compiled Notes" nil t)
                 (progn (forward-line 1)
                        (copy-marker (point)))
               (user-error "No '* Compiled Notes' heading found")))))
      (message "Compiling %d section(s) with %s/%s..."
               (length sections)
               (gptel-backend-name gptel-backend) gptel-model)
      ;; Kick off sequential processing
      (my/resource--compile-sections
       sections (current-buffer) insert-marker (length sections)))))

(global-set-key (kbd "M-m r C") #'my/resource-compile-notes)

(provide 'ai-workflows)
;;; ai-workflows.el ends here
