(defun sync-notes-with-rclone ()
  (interactive)
  "Sync notes directory to a designated remote with rclone."
  (message "Syncing notes with rclone...")
  ;; The & is important to run the command in the background
  (shell-command-to-string "rclone sync ~/Stillness/Personal/ One\ Drive:Personal/ &")
  (message "Sync complete."))

(add-hook 'kill-emacs-hook 'sync-notes-with-rclone)
