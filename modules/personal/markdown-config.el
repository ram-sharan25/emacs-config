;;; markdown-config.el --- Markdown editing and rich EWW preview via pandoc -*- lexical-binding: t; -*-

;;; Commentary:
;; Rich markdown preview inside Emacs using pandoc + EWW.
;; Pandoc converts markdown to styled HTML, EWW renders it in a side window.
;; Preview auto-refreshes on save.

;;; Code:

(defvar rsr/markdown-preview-css
  "
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
  font-size: 16px;
  line-height: 1.6;
  color: #c9d1d9;
  background-color: #0d1117;
  max-width: 800px;
  margin: 0 auto;
  padding: 20px;
}
h1, h2, h3, h4, h5, h6 {
  color: #e6edf3;
  margin-top: 24px;
  margin-bottom: 16px;
  font-weight: 600;
  line-height: 1.25;
}
h1 { font-size: 2em; padding-bottom: 0.3em; border-bottom: 1px solid #30363d; }
h2 { font-size: 1.5em; padding-bottom: 0.3em; border-bottom: 1px solid #30363d; }
h3 { font-size: 1.25em; }
a { color: #58a6ff; text-decoration: none; }
a:hover { text-decoration: underline; }
code {
  background-color: #161b22;
  padding: 0.2em 0.4em;
  border-radius: 6px;
  font-size: 85%;
  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
}
pre {
  background-color: #161b22;
  padding: 16px;
  border-radius: 6px;
  overflow: auto;
  line-height: 1.45;
}
pre code {
  background: none;
  padding: 0;
  font-size: 100%;
}
blockquote {
  padding: 0 1em;
  color: #8b949e;
  border-left: 0.25em solid #30363d;
  margin: 0 0 16px 0;
}
table {
  border-collapse: collapse;
  width: 100%;
  margin-bottom: 16px;
}
th, td {
  padding: 6px 13px;
  border: 1px solid #30363d;
}
th { background-color: #161b22; font-weight: 600; }
tr:nth-child(even) { background-color: #161b22; }
hr { border: none; border-top: 1px solid #30363d; margin: 24px 0; }
img { max-width: 100%; }
ul, ol { padding-left: 2em; }
li + li { margin-top: 0.25em; }
.task-list-item { list-style-type: none; }
.task-list-item input { margin-right: 0.5em; }
"
  "CSS stylesheet for markdown preview in EWW (GitHub dark theme).")

(defvar-local rsr/markdown-preview--eww-buffer nil
  "EWW buffer associated with this markdown buffer.")

(defvar-local rsr/markdown-preview--source-buffer nil
  "Source markdown buffer associated with this EWW preview.")

(defvar rsr/markdown-preview--css-file nil
  "Path to the temporary CSS file for pandoc.")

(defun rsr/markdown-preview--ensure-css ()
  "Write CSS to a temp file if not already done.  Return the path."
  (unless (and rsr/markdown-preview--css-file
               (file-exists-p rsr/markdown-preview--css-file))
    (setq rsr/markdown-preview--css-file
          (make-temp-file "markdown-preview-" nil ".css"))
    (with-temp-file rsr/markdown-preview--css-file
      (insert rsr/markdown-preview-css)))
  rsr/markdown-preview--css-file)

(defun rsr/markdown-preview--render ()
  "Convert current markdown buffer to HTML via pandoc and display in EWW."
  (let* ((md-buffer (current-buffer))
         (md-file (buffer-file-name md-buffer))
         (css-file (rsr/markdown-preview--ensure-css))
         (html-file (make-temp-file "markdown-preview-" nil ".html"))
         (pandoc-args (list "pandoc"
                            "--from=gfm"
                            "--to=html5"
                            "--standalone"
                            "--highlight-style=breezedark"
                            (concat "--css=file://" css-file)
                            "--embed-resources"
                            "-o" html-file)))
    ;; Feed buffer contents to pandoc (works for unsaved buffers too)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (insert content)
        (apply #'call-process-region (point-min) (point-max)
               (car pandoc-args) nil nil nil
               (cdr pandoc-args))))
    ;; Render in EWW
    (let ((eww-buf (or (and (buffer-live-p rsr/markdown-preview--eww-buffer)
                            rsr/markdown-preview--eww-buffer)
                       (generate-new-buffer
                        (format "*Markdown Preview: %s*"
                                (file-name-nondirectory
                                 (or md-file "untitled")))))))
      (setq rsr/markdown-preview--eww-buffer eww-buf)
      (with-current-buffer eww-buf
        (setq rsr/markdown-preview--source-buffer md-buffer))
      ;; Display in side window if not visible
      (unless (get-buffer-window eww-buf)
        (display-buffer-in-side-window
         eww-buf '((side . right) (window-width . 0.5))))
      ;; Render HTML in the EWW buffer
      (with-selected-window (get-buffer-window eww-buf)
        (eww-open-file html-file)
        (setq rsr/markdown-preview--source-buffer md-buffer))
      ;; Clean up temp HTML after a short delay
      (run-with-timer 2 nil #'delete-file html-file))))

(defun rsr/markdown-preview--on-save ()
  "Refresh preview on save if active."
  (when (and (buffer-live-p rsr/markdown-preview--eww-buffer)
             (get-buffer-window rsr/markdown-preview--eww-buffer))
    (rsr/markdown-preview--render)))

(defun rsr/markdown-preview--cleanup ()
  "Clean up preview buffer and hooks when markdown buffer is killed."
  (when (buffer-live-p rsr/markdown-preview--eww-buffer)
    (let ((win (get-buffer-window rsr/markdown-preview--eww-buffer)))
      (when win (delete-window win)))
    (kill-buffer rsr/markdown-preview--eww-buffer)))

(defun rsr/markdown-preview ()
  "Toggle rich markdown preview in a side EWW window.

Uses pandoc to convert the current buffer's markdown to styled
HTML, then renders it in EWW in a side window.  The preview
auto-refreshes on save."
  (interactive)
  (unless (derived-mode-p 'markdown-mode 'gfm-mode)
    (user-error "Not a markdown buffer"))
  (if (and (buffer-live-p rsr/markdown-preview--eww-buffer)
           (get-buffer-window rsr/markdown-preview--eww-buffer))
      ;; Toggle off — close preview
      (progn
        (let ((win (get-buffer-window rsr/markdown-preview--eww-buffer)))
          (when win (delete-window win)))
        (kill-buffer rsr/markdown-preview--eww-buffer)
        (setq rsr/markdown-preview--eww-buffer nil)
        (remove-hook 'after-save-hook #'rsr/markdown-preview--on-save t)
        (remove-hook 'kill-buffer-hook #'rsr/markdown-preview--cleanup t)
        (message "Markdown preview closed."))
    ;; Toggle on — open preview
    (rsr/markdown-preview--render)
    (add-hook 'after-save-hook #'rsr/markdown-preview--on-save nil t)
    (add-hook 'kill-buffer-hook #'rsr/markdown-preview--cleanup nil t)
    (message "Markdown preview opened. Auto-refreshes on save.")))

(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :bind (:map markdown-mode-map
         ("C-c C-c p" . rsr/markdown-preview)
         :map gfm-mode-map
         ("C-c C-c p" . rsr/markdown-preview))
  :custom
  (markdown-command "pandoc --from=gfm --to=html5 --highlight-style=breezedark")
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t))

(provide 'markdown-config)
;;; markdown-config.el ends here
