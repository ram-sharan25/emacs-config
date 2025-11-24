(require 'paths)
(defun sync-notes-with-rclone ()
  "Sync notes directory to a designated remote with rclone."
  (interactive)
  (message "Syncing notes with rclone...")
  ;; Use start-process for background execution without capturing output
  (start-process "rclone-sync" nil "rclone" "copy"
                 (expand-file-name my/brain-dir)
                 "One Drive:Personal/")
  (message "Sync initiated in background."))


(add-hook 'kill-emacs-hook 'sync-notes-with-rclone)
