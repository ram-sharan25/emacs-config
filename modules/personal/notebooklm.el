;;; notebooklm.el --- NotebookLM chat interface -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs chat interface for the notebooklm CLI, modelled on gptel.
;;
;; The *NotebookLM* buffer is an org-mode buffer.  Type your question
;; anywhere after the last answer, then press C-c RET to send.  Answers
;; stream in under a "** NLM [timestamp]" heading.  Markdown from the CLI
;; is converted to org syntax so everything renders natively.
;;
;; All keybindings live under M-m n (rsr/global-prefix-map):
;;
;;   n o  — open / switch to chat buffer
;;   n l  — list notebooks + select active
;;   n c  — create notebook
;;   n s  — status (auth + active notebook)
;;   n +  — add source URL
;;   n f  — add source (local file chooser)
;;   n .  — add current buffer as source (eww/pdf/org auto-detect)
;;   n S  — list sources
;;   n A  — pre-fill Ask with region (C-u: region as context + prompt)
;;   n y  — capture last NLM answer at point in current buffer
;;   n g  — generate artifact
;;   n G  — list artifacts + status
;;   n d  — download artifact
;;   n x  — clear chat buffer
;;
;; Inside the *NotebookLM* buffer:
;;   C-c RET  — send question to NotebookLM
;;   C-c C-c  — same

;;; Code:

;;; ─── Customisation ───────────────────────────────────────────────────────────

(defgroup rsr/notebooklm nil
  "NotebookLM CLI integration."
  :group 'tools
  :prefix "rsr/nlm-")

(defcustom rsr/nlm-cli "notebooklm"
  "Path or name of the notebooklm CLI executable."
  :type 'string
  :group 'rsr/notebooklm)

(defcustom rsr/nlm-download-dir (expand-file-name "~/Downloads")
  "Default directory for downloaded NotebookLM artifacts."
  :type 'directory
  :group 'rsr/notebooklm)

(defcustom rsr/nlm-buffer-name "*NotebookLM*"
  "Name of the NotebookLM chat buffer."
  :type 'string
  :group 'rsr/notebooklm)

;;; ─── Markdown → Org conversion ───────────────────────────────────────────────

(defun rsr/nlm--md-to-org (text)
  "Convert markdown TEXT (as output by the notebooklm CLI) to org syntax.

Strips CLI metadata, converts bold/italic/code, and normalises all list
styles (asterisk, en-dash, em-dash, numbered) to org-compatible forms."
  (with-temp-buffer
    (insert text)
    ;; Strip CLI answer prefix: "Answer:\n"
    (goto-char (point-min))
    (when (looking-at "Answer:\n")
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Strip CLI conversation footer: "Conversation: <uuid> (turn N)"
    (goto-char (point-min))
    (while (re-search-forward "\nConversation: [^\n]+\n?" nil t)
      (replace-match "\n"))
    ;; Bold: **text** → *text*  (run before list-item pass)
    (goto-char (point-min))
    (while (re-search-forward "\\*\\*\\([^*\n]+\\)\\*\\*" nil t)
      (replace-match "*\\1*"))
    ;; Italic: _text_ → /text/
    (goto-char (point-min))
    (while (re-search-forward "\\b_\\([^_\n]+\\)_\\b" nil t)
      (replace-match "/\\1/"))
    ;; Inline code: `text` → ~text~
    (goto-char (point-min))
    (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
      (replace-match "~\\1~"))
    ;; En-dash / em-dash list items: "–   text" or "—   text" → "- text"
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*[–—][[:space:]]+" nil t)
      (replace-match "- "))
    ;; Asterisk list items: "* text" → "- text"
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\* " nil t)
      (replace-match "- "))
    ;; ATX headings: "## text" → "** text"
    (goto-char (point-min))
    (while (re-search-forward "^\\(#+\\)[[:space:]]" nil t)
      (replace-match (concat (make-string (length (match-string 1)) ?*) " ")))
    (buffer-string)))

;;; ─── Minor mode ──────────────────────────────────────────────────────────────

(defvar nlm-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'rsr/nlm-send)
    (define-key map (kbd "C-c C-c") #'rsr/nlm-send)
    map)
  "Keymap for `nlm-chat-mode'.")

(define-minor-mode nlm-chat-mode
  "Minor mode for the NotebookLM chat buffer.

Adds C-c RET / C-c C-c to send the current question."
  :lighter " NLM"
  :keymap nlm-chat-mode-map)

;;; ─── Chat buffer ─────────────────────────────────────────────────────────────

(defun rsr/nlm--chat-buffer ()
  "Return the NotebookLM chat buffer, creating and initialising it if needed."
  (let ((buf (get-buffer-create rsr/nlm-buffer-name)))
    (with-current-buffer buf
      ;; Always ensure the buffer is writable regardless of previous state.
      (setq buffer-read-only nil)
      (unless (derived-mode-p 'org-mode)
        (org-mode)
        (nlm-chat-mode 1)
        (insert "* NotebookLM\n\n")
        (message "NotebookLM chat ready. Type your question and press C-c RET."))
      ;; Re-enable the minor mode if it was lost (e.g. after mode change).
      (unless (bound-and-true-p nlm-chat-mode)
        (nlm-chat-mode 1)))
    buf))

(defun rsr/nlm--question-start ()
  "Return the buffer position where the current question begins.

That is: the end of the last NLM answer subtree, or just after the
title heading when no answers exist yet."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "^\\*\\* NLM " nil t)
        ;; Jump past the subtree of the last answer
        (progn
          (org-end-of-subtree t t)
          (skip-chars-forward "\n[:space:]")
          (point))
      ;; No answers yet: skip past the "* NotebookLM" title heading
      (goto-char (point-min))
      (forward-line 2)           ;; past heading + blank line
      (point))))

(defun rsr/nlm--extract-question ()
  "Return the question text typed after the last NLM answer."
  (string-trim
   (buffer-substring-no-properties
    (rsr/nlm--question-start)
    (point-max))))

;;; ─── Async process ───────────────────────────────────────────────────────────

(defun rsr/nlm--run-sync (args)
  "Run notebooklm with ARGS synchronously; return stdout string.
Signals an error on non-zero exit."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process rsr/nlm-cli nil t nil args)))
      (if (zerop exit-code)
          (buffer-string)
        (error "notebooklm %s failed (exit %d):\n%s"
               (car args) exit-code (buffer-string))))))

(defun rsr/nlm--parse-json (str)
  "Parse JSON string STR into an alist/list structure."
  (condition-case err
      (json-parse-string str :object-type 'alist :array-type 'list)
    (json-parse-error
     (error "JSON parse error: %s\nInput: %s" (cadr err) str))))

(defun rsr/nlm--run-chat (args chat-buf insert-marker label)
  "Run notebooklm with ARGS, streaming output into CHAT-BUF at INSERT-MARKER.
LABEL is shown in the answer heading."
  (ignore label)
  (make-process
   :name    "notebooklm-chat"
   :buffer  nil
   :command (cons rsr/nlm-cli args)
   :filter
   (lambda (_proc string)
     (when (buffer-live-p chat-buf)
       (with-current-buffer chat-buf
         (let ((inhibit-read-only t))
           (save-excursion
             (goto-char insert-marker)
             (insert (rsr/nlm--md-to-org string))
             (set-marker insert-marker (point)))))))
   :sentinel
   (lambda (_proc event)
     (when (and (string-prefix-p "finished" event)
                (buffer-live-p chat-buf))
       (with-current-buffer chat-buf
         (let ((inhibit-read-only t))
           (save-excursion
             (goto-char insert-marker)
             (unless (bolp) (insert "\n"))
             (insert "\n")))
         (goto-char (point-max)))))))

(defun rsr/nlm--run-mgmt (args heading)
  "Run a management command with ARGS, inserting output in the chat buffer.
HEADING is used as the org heading for this management block."
  (let* ((chat-buf (rsr/nlm--chat-buffer))
         marker)
    (with-current-buffer chat-buf
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "\n** → %s  /%s/\n\n#+begin_example\n"
                      heading
                      (format-time-string "%H:%M")))
      (setq marker (point-marker)))
    (make-process
     :name    "notebooklm-mgmt"
     :buffer  nil
     :command (cons rsr/nlm-cli args)
     :filter
     (lambda (_proc string)
       (when (buffer-live-p chat-buf)
         (with-current-buffer chat-buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char marker)
               (insert string)
               (set-marker marker (point)))))))
     :sentinel
     (lambda (_proc event)
       (when (and (string-prefix-p "finished" event)
                  (buffer-live-p chat-buf))
         (with-current-buffer chat-buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char marker)
               (insert "#+end_example\n")))
           (goto-char (point-max))))))
    (pop-to-buffer chat-buf)))

;;; ─── Notebook helpers ────────────────────────────────────────────────────────

(defun rsr/nlm--get-notebooks ()
  "Fetch notebooks; return list of (TITLE . ID) pairs."
  (let* ((raw       (rsr/nlm--run-sync '("list" "--json")))
         (data      (rsr/nlm--parse-json raw))
         (notebooks (alist-get 'notebooks data)))
    (mapcar (lambda (nb)
              (cons (alist-get 'title nb) (alist-get 'id nb)))
            notebooks)))

(defun rsr/nlm--select-notebook (prompt)
  "PROMPT user to choose a notebook; return its ID."
  (let* ((nbs    (rsr/nlm--get-notebooks))
         (choice (completing-read prompt (mapcar #'car nbs) nil t)))
    (or (cdr (assoc choice nbs))
        (error "No notebook selected"))))

;;; ─── Artifact helpers ────────────────────────────────────────────────────────

(defun rsr/nlm--get-artifacts ()
  "Fetch artifacts; return list of (LABEL . ID) pairs."
  (let* ((raw       (rsr/nlm--run-sync '("artifact" "list" "--json")))
         (data      (rsr/nlm--parse-json raw))
         (artifacts (alist-get 'artifacts data)))
    (mapcar (lambda (a)
              (cons (format "%s  [%s]  %s"
                            (alist-get 'title  a "")
                            (alist-get 'type   a "")
                            (alist-get 'status a ""))
                    (alist-get 'id a)))
            artifacts)))

;;; ─── Chat commands ───────────────────────────────────────────────────────────

;;;###autoload
(defun rsr/nlm-open ()
  "Open the NotebookLM chat buffer."
  (interactive)
  (pop-to-buffer (rsr/nlm--chat-buffer)))

;;;###autoload
(defun rsr/nlm-send ()
  "Send the current question to NotebookLM.

Works from anywhere (mirrors M-m a s / gptel-send):
- Called outside the chat buffer → switches to it, ready to type.
- Called inside the chat buffer → extracts and sends the question.
Response streams in under a new '** NLM [timestamp]' org heading."
  (interactive)
  (let ((chat-buf (rsr/nlm--chat-buffer)))
    (if (not (eq (current-buffer) chat-buf))
        ;; Outside the chat buffer: navigate there so user can type/send.
        (progn
          (pop-to-buffer chat-buf)
          (goto-char (point-max))
          (message "Type your question, then press C-c RET to send."))
      ;; Inside the chat buffer: extract question and send.
      (let ((question (rsr/nlm--extract-question)))
        (when (string-empty-p question)
          (user-error "Nothing to send — type a question first"))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let* ((ts      (format-time-string "%Y-%m-%d %H:%M"))
               (heading (format "\n** NLM [%s]\n\n" ts))
               marker)
          (insert heading)
          (setq marker (point-marker))
          (rsr/nlm--run-chat (list "ask" question)
                             (current-buffer)
                             marker
                             question))))))

;;;###autoload
(defun rsr/nlm-prefill-region (beg end &optional context-p)
  "Pre-fill the chat buffer input with the selected region.

Without prefix arg: the region IS the question — placed at end of buffer
ready to send with C-c RET.
With prefix arg CONTEXT-P: wraps the region as context and prompts for
a question inline."
  (interactive "r\nP")
  (unless (use-region-p)
    (user-error "Select a region first"))
  (let ((region-text (buffer-substring-no-properties beg end))
        (chat-buf    (rsr/nlm--chat-buffer)))
    (pop-to-buffer chat-buf)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (if context-p
        (let ((q (read-string "Ask about this: ")))
          (insert (format "Context:\n#+begin_quote\n%s\n#+end_quote\n\n%s"
                          region-text q)))
      (insert region-text))
    (message "Press C-c RET to send.")))

;;; ─── Notebook management ─────────────────────────────────────────────────────

;;;###autoload
(defun rsr/nlm-status ()
  "Show active notebook context and auth status."
  (interactive)
  (rsr/nlm--run-mgmt '("status") "status"))

;;;###autoload
(defun rsr/nlm-list ()
  "List notebooks and switch the active context."
  (interactive)
  (let ((id (rsr/nlm--select-notebook "Switch to notebook: ")))
    (rsr/nlm--run-mgmt (list "use" id) (format "use %s" id))))

;;;###autoload
(defun rsr/nlm-create (title)
  "Create a new notebook with TITLE."
  (interactive "sNew notebook title: ")
  (rsr/nlm--run-mgmt (list "create" title "--json")
                     (format "create \"%s\"" title)))

;;; ─── Source management ───────────────────────────────────────────────────────

;;;###autoload
(defun rsr/nlm-source-add (url-or-path)
  "Add URL-OR-PATH as a source to the active notebook."
  (interactive "sSource URL or path: ")
  (rsr/nlm--run-mgmt (list "source" "add" url-or-path)
                     (format "source add %s" url-or-path)))

;;;###autoload
(defun rsr/nlm-source-add-file ()
  "Choose a local file and add it as a source."
  (interactive)
  (let ((path (expand-file-name (read-file-name "Add file as source: "))))
    (rsr/nlm--run-mgmt (list "source" "add" path)
                       (format "source add %s" (file-name-nondirectory path)))))

;;;###autoload
(defun rsr/nlm-source-add-from-buffer ()
  "Add the current buffer as a source, auto-detecting type.

- eww-mode      → adds the current page URL
- pdf-view-mode → adds the PDF file path
- org-mode      → exports buffer to a temp .org file
- other         → exports buffer text to a temp .txt file"
  (interactive)
  (cond
   ((derived-mode-p 'eww-mode)
    (let ((url (eww-current-url)))
      (rsr/nlm--run-mgmt (list "source" "add" url)
                         (format "source add (eww) %s" url))))
   ((derived-mode-p 'pdf-view-mode)
    (let ((path (or (buffer-file-name)
                    (error "PDF buffer has no associated file"))))
      (rsr/nlm--run-mgmt (list "source" "add" path)
                         (format "source add (pdf) %s"
                                 (file-name-nondirectory path)))))
   ((derived-mode-p 'org-mode)
    (let ((tmp (make-temp-file "nlm-" nil ".org")))
      (write-region (point-min) (point-max) tmp)
      (rsr/nlm--run-mgmt (list "source" "add" tmp)
                         (format "source add (org) %s" (buffer-name)))))
   (t
    (let ((tmp (make-temp-file "nlm-" nil ".txt")))
      (write-region (point-min) (point-max) tmp)
      (rsr/nlm--run-mgmt (list "source" "add" tmp)
                         (format "source add (txt) %s" (buffer-name)))))))

;;;###autoload
(defun rsr/nlm-source-list ()
  "List sources in the active notebook."
  (interactive)
  (rsr/nlm--run-mgmt '("source" "list") "source list"))

;;; ─── Generation & download ───────────────────────────────────────────────────

(defconst rsr/nlm--generate-types
  '("audio" "video" "report" "quiz" "flashcards"
    "slide-deck" "infographic" "mind-map" "data-table")
  "Supported notebooklm generate types.")

(defconst rsr/nlm--no-instructions-types '("mind-map")
  "Generate types that accept no instruction string.")

;;;###autoload
(defun rsr/nlm-generate ()
  "Choose an artifact type and generate it for the active notebook."
  (interactive)
  (let* ((type (completing-read "Generate type: "
                                rsr/nlm--generate-types nil t))
         (args (if (member type rsr/nlm--no-instructions-types)
                   (list "generate" type)
                 (let ((inst (read-string
                              (format "Instructions for %s (optional): " type))))
                   (if (string-empty-p inst)
                       (list "generate" type)
                     (list "generate" type inst))))))
    (rsr/nlm--run-mgmt args (format "generate %s" type))))

;;;###autoload
(defun rsr/nlm-artifact-list ()
  "List artifacts and their status."
  (interactive)
  (rsr/nlm--run-mgmt '("artifact" "list") "artifact list"))

(defconst rsr/nlm--download-extensions
  '(("audio"       . ".mp3")
    ("video"       . ".mp4")
    ("slide-deck"  . ".pdf")
    ("report"      . ".md")
    ("mind-map"    . ".json")
    ("quiz"        . ".md")
    ("flashcards"  . ".md")
    ("infographic" . ".png")
    ("data-table"  . ".csv"))
  "Default extensions per artifact type.")

;;;###autoload
(defun rsr/nlm-download ()
  "Choose an artifact type and download it."
  (interactive)
  (let* ((type (completing-read "Download type: "
                                (mapcar #'car rsr/nlm--download-extensions)
                                nil t))
         (ext  (cdr (assoc type rsr/nlm--download-extensions)))
         (dest (expand-file-name
                (read-file-name
                 "Save as: "
                 rsr/nlm-download-dir nil nil
                 (format "notebooklm-%s%s"
                         (format-time-string "%Y%m%d") ext)))))
    (rsr/nlm--run-mgmt (list "download" type dest)
                       (format "download %s → %s"
                               type (file-name-nondirectory dest)))))

;;; ─── Capture ─────────────────────────────────────────────────────────────────

;;;###autoload
(defun rsr/nlm-capture-answer ()
  "Insert the last NLM answer at point in the current buffer.

Extracts text from the last '** NLM' heading in the chat buffer
and inserts it at point, trimmed."
  (interactive)
  (let ((log-buf    (get-buffer rsr/nlm-buffer-name))
        (target-buf (current-buffer)))
    (unless log-buf
      (user-error "No NotebookLM chat buffer — run a query first"))
    (let ((answer
           (with-current-buffer log-buf
             (save-excursion
               (goto-char (point-max))
               (when (re-search-backward "^\\*\\* NLM " nil t)
                 (forward-line 1)           ;; skip the heading line itself
                 (skip-chars-forward "\n")
                 (let ((start (point))
                       (end   (progn (org-end-of-subtree t t) (point))))
                   (string-trim
                    (buffer-substring-no-properties start end))))))))
      (unless (and answer (not (string-empty-p answer)))
        (user-error "No NLM answer found in chat buffer"))
      (with-current-buffer target-buf
        (insert answer "\n")))))

;;; ─── Buffer utilities ────────────────────────────────────────────────────────

;;;###autoload
(defun rsr/nlm-clear ()
  "Erase the NotebookLM chat buffer and start fresh."
  (interactive)
  (when (yes-or-no-p "Clear the NotebookLM chat buffer? ")
    (with-current-buffer (rsr/nlm--chat-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "* NotebookLM\n\n")))
    (message "NotebookLM chat cleared.")))

;;; ─── Keybindings ─────────────────────────────────────────────────────────────
;;
;; Mirrored from gptel (M-m a …) so muscle memory transfers:
;;
;;   gptel       notebooklm    action
;;   ───────     ──────────    ──────────────────────────────────────
;;   a a         n n           open chat buffer  (quick open)
;;   a s         n s           send question     (global: open+focus)
;;   a f         n f           add file as source
;;   a F         n .           add current buffer as source
;;   a +         n +           add URL source
;;   ─           n A           prefill from region (C-u: context mode)
;;   ─           n y           capture last answer at point
;;   ─           n l           list/switch notebooks
;;   ─           n c           create notebook
;;   ─           n i           info / status
;;   ─           n S           list sources
;;   ─           n g           generate artifact
;;   ─           n G           list artifacts
;;   ─           n d           download artifact
;;   ─           n x           clear chat buffer

(define-key rsr/global-prefix-map (kbd "n n") #'rsr/nlm-open)
(define-key rsr/global-prefix-map (kbd "n s") #'rsr/nlm-send)
(define-key rsr/global-prefix-map (kbd "n f") #'rsr/nlm-source-add-file)
(define-key rsr/global-prefix-map (kbd "n .") #'rsr/nlm-source-add-from-buffer)
(define-key rsr/global-prefix-map (kbd "n +") #'rsr/nlm-source-add)
(define-key rsr/global-prefix-map (kbd "n A") #'rsr/nlm-prefill-region)
(define-key rsr/global-prefix-map (kbd "n y") #'rsr/nlm-capture-answer)
(define-key rsr/global-prefix-map (kbd "n l") #'rsr/nlm-list)
(define-key rsr/global-prefix-map (kbd "n c") #'rsr/nlm-create)
(define-key rsr/global-prefix-map (kbd "n i") #'rsr/nlm-status)
(define-key rsr/global-prefix-map (kbd "n S") #'rsr/nlm-source-list)
(define-key rsr/global-prefix-map (kbd "n g") #'rsr/nlm-generate)
(define-key rsr/global-prefix-map (kbd "n G") #'rsr/nlm-artifact-list)
(define-key rsr/global-prefix-map (kbd "n d") #'rsr/nlm-download)
(define-key rsr/global-prefix-map (kbd "n x") #'rsr/nlm-clear)

(provide 'notebooklm)
;;; notebooklm.el ends here
