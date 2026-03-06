(use-package smartrep
  :ensure t
  :after org-mpv-notes)

(require 'paths)

(use-package org-mpv-notes
  :ensure t
  :commands (org-mpv-notes-mode org-mpv-notes-open)
  :hook (org-mode . org-mpv-notes-setup-link)
  :config
  (require 'org-download)
  (setq org-mpv-notes-save-image-function #'org-download-image)
  (define-key org-mpv-notes-mode-map (kbd "C-c v") (smartrep-map org-mpv-notes-key-bindings)))

(use-package mpv
  :ensure t)

;; open video files via org-mpv-notes so IPC connection is established
(dolist (ext '("\\.mp4\\'" "\\.mkv\\'" "\\.webm\\'"))
  (add-to-list 'org-file-apps `(,ext . (lambda (file _link) (org-mpv-notes-open file)))))

(defun my/download-video (link-key filename url)
  "Download video to the directory defined by LINK-KEY in `org-link-abbrev-alist'.
Example: Enter `dsa_dir' to download to the DSA lectures folder."
  (interactive
   (let ((keys (mapcar #'car org-link-abbrev-alist)))
     (list (completing-read "Target Directory Key (e.g., dsa_lec): " keys)
           (read-string "Enter filename (without extension): ")
           (read-string "Enter URL: "))))

  (let* ((expansion (cdr (assoc link-key org-link-abbrev-alist)))
         (clean-path (replace-regexp-in-string "%s" "" expansion))
         (target-dir (file-name-as-directory (expand-file-name clean-path))))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))

    (let ((cmd (format "yt-dlp -o %s --cookies-from-browser chrome %s"
                       (shell-quote-argument (format "%s%s.%%(ext)s" target-dir filename))
                       (shell-quote-argument url))))
      (message "Downloading to: %s" target-dir)
      (async-shell-command cmd (format "*yt-dlp: %s*" filename)))))

(add-to-list 'org-link-abbrev-alist (cons "dsa_lec" (concat my/dsa-lectures "%s")))
(add-to-list 'org-link-abbrev-alist (cons "coa_lec" (concat my/coa-lectures "%s")))
