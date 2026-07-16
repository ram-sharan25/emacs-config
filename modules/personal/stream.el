;;; stream.el --- Topic capture streams: thoughts + questions  -*- lexical-binding: t; -*-

;; A "stream" is one org file per topic you are reading or watching.
;; You type thoughts as plain lines.  When a line is a question, press
;; `C-c SPC' to turn it into a checkbox item with an :Answer: drawer and a
;; unique target.  The drawer's explicit :End: means the question never
;; swallows the thoughts you type after it.  An auto-built index at the top
;; of the file lists every open (unchecked) question and drops answered ones
;; on save.
;;
;; Streams are date-prefixed org files in `my/resources-dir' (see paths.el).
;; `my/stream-mode' turns itself on automatically for any org buffer that
;; contains the open-questions block, so regular resource notes are untouched.

;;; Code:

(declare-function org-update-all-dblocks "org")

;;;; Helpers

(defun my/stream--slugify (s)
  "Turn string S into a filename-safe slug."
  (let* ((slug (downcase (string-trim s)))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
         (slug (replace-regexp-in-string "\\(?:^-+\\|-+$\\)" "" slug)))
    (if (string-empty-p slug) "untitled" slug)))

(defun my/stream--unique-id ()
  "Return a question target id unique within the current buffer."
  (let* ((base (format "q-%s" (format-time-string "%Y%m%d%H%M%S")))
         (id base)
         (n 1))
    (save-excursion
      (while (progn (goto-char (point-min))
                    (search-forward (format "<<%s>>" id) nil t))
        (setq id (format "%s-%d" base n)
              n (1+ n))))
    id))

(defun my/stream--refresh-index ()
  "Rebuild the open-questions dynamic block in this buffer.
Safe to run from `before-save-hook'."
  (when (derived-mode-p 'org-mode)
    (save-excursion (org-update-all-dblocks))))

;;;; Dynamic block: the open-questions index

(defun org-dblock-write:open-questions (_params)
  "Write the open-questions index.
Lists every unchecked question in the buffer as a link to its target.
Handles questions whose text auto-fills across several physical lines: each
target is matched first, then we scan back to its enclosing checkbox item and
collapse the wrapped text into one line."
  (let ((questions '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<<\\(q-[0-9-]+\\)>>" nil t)
        (let ((id (match-string 1))
              (target-start (match-beginning 0)))
          (save-excursion
            ;; Walk back to the checkbox that owns this target.
            (when (re-search-backward "^[ \t]*- \\[\\([ X]\\)\\][ \t]+" nil t)
              (let ((state (match-string 1))
                    (text-beg (match-end 0)))
                (when (string= state " ")
                  (let ((text (string-trim
                               (replace-regexp-in-string
                                "[ \t\n]+" " "
                                (buffer-substring-no-properties
                                 text-beg target-start)))))
                    (push (cons id text) questions)))))))))
    (setq questions (nreverse questions))
    (if questions
        (let ((first t))
          (dolist (q questions)
            (unless first (insert "\n"))
            (setq first nil)
            (insert (format "- [[%s][%s]]" (car q) (cdr q)))))
      (insert "/No open questions./"))))

;;;; Commands

(defun my/stream-question-new (question)
  "Jot QUESTION without disturbing the line you are writing.
Prompts for the text, inserts a checkbox item with a unique target and an
empty :Answer: drawer on a fresh line just below the current one, then puts
point back exactly where it was.  Bound to `C-c SPC'.  Use
`my/stream-question' (`C-c S-SPC') to convert the current line itself."
  (interactive "sQuestion: ")
  (setq question (string-trim question))
  (when (string-empty-p question)
    (user-error "No question text"))
  (let ((id (my/stream--unique-id)))
    ;; `save-excursion' restores point; the insert happens past end-of-line,
    ;; strictly after point, so the writing position is never shifted.
    (save-excursion
      (end-of-line)
      (insert (format "\n- [ ] %s   <<%s>>\n  :Answer:\n  :End:" question id))))
  (my/stream--refresh-index))

(defun my/stream-question ()
  "Turn the current line into a stream question.
Rewrites the line as a checkbox item with a unique target and inserts an
empty :Answer: drawer beneath it.  On an empty line, prompts for the text.
Parks point on a fresh line after the drawer so you can keep typing.
Bound to `C-c S-SPC'; see `my/stream-question-new' (`C-c SPC') to add a
question without converting the current line."
  (interactive)
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (line (string-trim (buffer-substring-no-properties bol eol))))
    (when (string-match-p "<<q-[0-9-]+>>" line)
      (user-error "This line is already a question"))
    (when (string-empty-p line)
      (setq line (string-trim (read-string "Question: ")))
      (when (string-empty-p line)
        (user-error "No question text")))
    (let ((id (my/stream--unique-id)))
      (delete-region bol eol)
      (goto-char bol)
      (insert (format "- [ ] %s   <<%s>>\n" line id))
      (insert "  :Answer:\n  :End:\n\n")
      (forward-line -1)))
  (my/stream--refresh-index))

(defun my/stream--scaffold (title)
  "Return the initial scaffold text for a stream titled TITLE.
Ends right after the `* Stream' heading so a capture body can append the
first thought.  Shared by `my/stream-new' and the stream capture template."
  (format (concat "#+TITLE: %s\n"
                  "#+STARTUP: overview\n"
                  "#+FILETAGS: :stream:\n\n"
                  "* Open Questions\n"
                  "#+BEGIN: open-questions\n"
                  "#+END:\n\n"
                  "* Stream\n")
          title))

(defun my/stream--new-file (title)
  "Return the stream file path in `my/resources-dir' for TITLE.
Creates the directory if needed.  Names files `YYYY-MM-DD-slug.org'."
  (unless (file-directory-p my/resources-dir)
    (make-directory my/resources-dir t))
  (expand-file-name
   (format "%s-%s.org"
           (format-time-string "%Y-%m-%d")
           (my/stream--slugify title))
   my/resources-dir))

(defun my/stream-new (title)
  "Create and open a new capture stream file for TITLE."
  (interactive "sTopic: ")
  (let ((file (my/stream--new-file title)))
    (find-file file)
    (when (zerop (buffer-size))
      (insert (my/stream--scaffold title))
      (my/stream--refresh-index)
      (goto-char (point-max)))
    ;; Enable explicitly: the `org-mode-hook' auto-enable runs at `find-file'
    ;; time, before the scaffold (and its detection marker) is inserted.
    (my/stream-mode 1)))

(defun my/stream-open ()
  "Open an existing capture stream file.
Streams are the date-prefixed org files in `my/resources-dir'."
  (interactive)
  (let ((files (and (file-directory-p my/resources-dir)
                    (directory-files
                     my/resources-dir nil
                     "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-.*\\.org\\'"))))
    (unless files (user-error "No capture streams yet"))
    (find-file (expand-file-name
                (completing-read "Stream: " files nil t)
                my/resources-dir))))

;;;; Org-capture integration

(defvar my/stream--capture-title nil
  "Topic stashed by `my/stream--capture-target' for the capture body.")

(defvar my/stream--capture-file nil
  "Path of the stream being created by capture, for after-finalize reopen.")

(defun my/stream--capture-target ()
  "Org-capture target: prompt a topic and return a new stream file path.
Saves streams in `my/resources-dir' as `YYYY-MM-DD-slug.org'.  Stashes the
title and path so the template body and `my/stream--capture-after-finalize'
can use them."
  (let ((title (string-trim (read-string "Stream topic: "))))
    (when (string-empty-p title)
      (user-error "No stream topic"))
    (setq my/stream--capture-title title
          my/stream--capture-file (my/stream--new-file title))))

(defun my/stream--capture-after-finalize ()
  "Reopen a freshly captured stream and turn on `my/stream-mode'.
Stream mode cannot auto-enable during capture because the detection marker
is inserted after `org-mode-hook' has already run.  Does nothing when the
capture was aborted or was not a stream capture."
  (let ((file my/stream--capture-file))
    (setq my/stream--capture-file nil)
    (when (and file
               (not (bound-and-true-p org-note-abort))
               (file-exists-p file))
      (find-file file)
      (my/stream-mode 1)
      (goto-char (point-max)))))

(add-hook 'org-capture-after-finalize-hook
          #'my/stream--capture-after-finalize)

;;;; Minor mode

(defvar my/stream-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c SPC")   #'my/stream-question-new)
    (define-key map (kbd "C-c S-SPC") #'my/stream-question)
    map)
  "Keymap for `my/stream-mode'.")

(define-minor-mode my/stream-mode
  "Minor mode for topic capture streams.
Binds question capture, keeps the open-questions index fresh on save, and
soft-wraps long lines instead of hard-filling them."
  :lighter " Stream"
  :keymap my/stream-mode-map
  (if my/stream-mode
      (progn
        (add-hook 'before-save-hook #'my/stream--refresh-index nil t)
        ;; A stream is prose capture, not code: never let `auto-fill-mode'
        ;; bake a hard newline into the middle of a thought or question (that
        ;; splits an item across lines and breaks folding).  Wrap visually.
        (auto-fill-mode -1)
        (visual-line-mode 1))
    (remove-hook 'before-save-hook #'my/stream--refresh-index t)
    (visual-line-mode -1)))

(defun my/stream--stream-buffer-p ()
  "Return non-nil if the current buffer is a capture stream.
Detected by the presence of the open-questions block, so regular
resource notes sharing the directory are left untouched."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^#\\+BEGIN: open-questions" 10000 t)))

(defun my/stream--maybe-enable ()
  "Enable `my/stream-mode' for org buffers that are capture streams.
Hung on `after-change-major-mode-hook' (appended) so it runs *after* the
global `turn-on-auto-fill' (init.el), letting stream mode's soft-wrap setup
win the last word over auto-fill."
  (when (and buffer-file-name
             (derived-mode-p 'org-mode)
             (my/stream--stream-buffer-p))
    (my/stream-mode 1)))

(add-hook 'after-change-major-mode-hook #'my/stream--maybe-enable t)

(provide 'stream)
;;; stream.el ends here
