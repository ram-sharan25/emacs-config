;;; markdown-config.el --- Markdown editing and live browser preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown editing plus a browser preview that refreshes over WebSockets while
;; text is edited.  The same local stylesheet is also used by markdown-mode's
;; one-shot `markdown-preview' command.

;;; Code:

(require 'eieio)

(defconst rsr/markdown-preview-css-file
  (expand-file-name "themes/markdown-preview.css" user-emacs-directory)
  "Stylesheet shared by static and live Markdown previews.")

(defconst rsr/markdown-preview-template-file
  (expand-file-name "themes/markdown-preview.html" user-emacs-directory)
  "Browser preview template with a restrictive content security policy.")

(defconst rsr/markdown-preview-client-file
  (expand-file-name "themes/markdown-preview-client.js" user-emacs-directory)
  "Client script used by the browser preview.")

(defconst rsr/markdown-preview-filter-file
  (expand-file-name "themes/markdown-preview-filter.lua" user-emacs-directory)
  "Pandoc filter that removes executable or unsafe Markdown content.")

(defun rsr/markdown-preview--inline-stylesheet ()
  "Return the preview stylesheet wrapped in an HTML style element."
  (unless (file-readable-p rsr/markdown-preview-css-file)
    (error "Markdown preview stylesheet is not readable: %s"
           rsr/markdown-preview-css-file))
  (with-temp-buffer
    (insert "<style>\n")
    (insert-file-contents rsr/markdown-preview-css-file)
    (goto-char (point-max))
    (insert "\n</style>")
    (buffer-string)))

(defun rsr/markdown-preview--start-http-server (port)
  "Start the restricted Markdown preview HTTP server on PORT.

Only the generated preview document and its fixed client script are served;
files beside the Markdown document are never exposed."
  (unless markdown-preview--http-server
    (advice-add 'make-network-process :filter-args
                #'markdown-preview--fix-network-process-wait)
    (unwind-protect
        (setq markdown-preview--http-server
              (ws-start
               (lambda (request)
                 (with-slots (process headers) request
                   (let* ((request-target (cdr (assoc :GET headers)))
                          (path (and (stringp request-target)
                                     (substring request-target 1)))
                          (uuid (markdown-preview--parse-uuid headers))
                          (buffer-name (and uuid
                                            (gethash uuid markdown-preview--preview-buffers)))
                          (preview-buffer (and buffer-name
                                               (get-buffer buffer-name))))
                     (cond
                      ((and (stringp path) (string= path "")
                            (buffer-live-p preview-buffer))
                       (ws-send-file
                        process
                        (with-current-buffer preview-buffer
                          (expand-file-name markdown-preview-file-name
                                            default-directory))))
                      ((and (stringp path)
                            (string= path ".markdown-preview-client.js"))
                       (ws-send-file process rsr/markdown-preview-client-file
                                     "application/javascript"))
                      (t (ws-send-404 process))))))
               port nil :host markdown-preview-http-host))
      (advice-remove 'make-network-process
                     #'markdown-preview--fix-network-process-wait))))

(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (markdown-command
   (format "pandoc --from=gfm --to=html5 --highlight-style=breezedark --lua-filter=%s"
           (shell-quote-argument rsr/markdown-preview-filter-file)))
  ;; Styles the one-shot `markdown-preview' command too.
  (markdown-css-paths (list rsr/markdown-preview-css-file))
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-map
         ("C-c C-c p" . markdown-preview-mode)
         :map gfm-mode-map
         ("C-c C-c p" . markdown-preview-mode))
  :custom
  (markdown-preview-auto-open 'http)
  (markdown-preview-delay-time 0.5)
  :config
  (dolist (file (list rsr/markdown-preview-template-file
                      rsr/markdown-preview-client-file
                      rsr/markdown-preview-filter-file))
    (unless (file-readable-p file)
      (error "Markdown preview resource is not readable: %s" file)))
  ;; Inline CSS is necessary because no document-directory files are exposed.
  (setq markdown-preview--preview-template rsr/markdown-preview-template-file
        markdown-preview-stylesheets
        (list (rsr/markdown-preview--inline-stylesheet)))
  (advice-add 'markdown-preview--start-http-server :override
              #'rsr/markdown-preview--start-http-server))

(provide 'markdown-config)
;;; markdown-config.el ends here
