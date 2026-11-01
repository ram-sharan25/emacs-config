;;; private-files.el --- Close decrypted Private/ buffers once left -*- lexical-binding: t; -*-

;;; Commentary:
;; Files under `my/private-dir' are GPG-encrypted and gpg-agent caches no
;; passphrase, but an open buffer keeps the plain text readable.  So as soon
;; as a Private *.gpg buffer is shown in no window, save it if modified and
;; kill it: coming back always means decrypting, and so the passphrase, again.

;;; Code:

(require 'paths)

(defun my/private-gpg-buffer-p (buf)
  "Non-nil if BUF visits an encrypted file under `my/private-dir'."
  (let ((file (buffer-file-name buf)))
    (and file
         (string-suffix-p ".gpg" file)
         (file-in-directory-p file my/private-dir))))

(defun my/private-close-hidden-buffers ()
  "Save and kill Private *.gpg buffers that no window shows.
Skipped while the minibuffer is active (a passphrase prompt may be up), and
for buffers an indirect buffer still uses, such as an open `org-capture'."
  (when (zerop (minibuffer-depth))
    (dolist (buf (buffer-list))
      (when (and (my/private-gpg-buffer-p buf)
                 (not (get-buffer-window buf t))
                 (not (seq-some (lambda (b) (eq (buffer-base-buffer b) buf))
                                (buffer-list))))
        (with-current-buffer buf
          (when (buffer-modified-p)
            (condition-case err
                (save-buffer)
              (error (message "Private: could not save %s: %s"
                              (buffer-name) (error-message-string err))))))
        (unless (buffer-modified-p buf)
          (kill-buffer buf))))))

;; Run once the command that changed windows has finished, so a file still
;; being decrypted by `find-file' is never killed half-way.
(defvar my/private-close-timer nil)

(defun my/private-schedule-close (&rest _)
  "Close hidden Private buffers when Emacs is next idle."
  (unless (timerp my/private-close-timer)
    (setq my/private-close-timer
          (run-with-idle-timer 0 nil
                               (lambda ()
                                 (setq my/private-close-timer nil)
                                 (my/private-close-hidden-buffers))))))

(add-hook 'window-buffer-change-functions #'my/private-schedule-close)

(provide 'private-files)
;;; private-files.el ends here
