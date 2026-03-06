(use-package smartrep
  :ensure t
  :demand t)

(require 'paths)

(use-package org-mpv-notes
  :ensure t
  :commands (org-mpv-notes-mode org-mpv-notes-open)
  :hook (org-mode . org-mpv-notes-setup-link)
  :config
  (require 'org-download)
  (setq org-mpv-notes-save-image-function #'org-download-image)
  (define-key org-mpv-notes-mode-map (kbd "M-n") (smartrep-map org-mpv-notes-key-bindings)))

(use-package mpv
  :ensure t)

;; configure .mp4 files to open with 'mpv' automatically
(add-to-list 'org-file-apps '("\\.mp4\\'" . "mpv \"%s\""))

(defun my/download-video (link-key filename url)
  "Download video to the directory defined by LINK-KEY in `org-link-abbrev-alist'.
Example: Enter `dsa_dir' to download to the DSA lectures folder."
  (interactive
   (let ((keys (mapcar #'car org-link-abbrev-alist)))
     (list (completing-read "Target Directory Key (e.g., dsa_lec): " keys)
           (read-string "Enter filename (without extension): ")
           (read-string "Enter URL: "))))

  (let* ((expansion (cdr (assoc link-key org-link-abbrev-alist)))
         ;; The expansion looks like "~/path/to/lectures/%s".
         ;; We replace "%s" with empty string to get the base directory.
         (clean-path (replace-regexp-in-string "%s" "" expansion))
         (target-dir (file-name-as-directory (expand-file-name clean-path))))

    ;; Create directory if it doesn't exist (safety check)
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))

    ;; Construct and run the command
    (let ((default-directory target-dir)
          (cmd (format "yt-dlp -o '%s/%%(title)s.%%(ext)s' -o '%s/%s.%%(ext)s' '%s' --cookies-from-browser chrome"
                       target-dir
                       target-dir
                       filename
                       url)))
      (message "Downloading to: %s" target-dir)
      (async-shell-command cmd (format "*yt-dlp: %s*" filename)))))

(add-to-list 'org-link-abbrev-alist (cons "dsa_lec" (concat my/dsa-lectures "%s")))
(add-to-list 'org-link-abbrev-alist (cons "coa_lec" (concat my/coa-lectures "%s")))
