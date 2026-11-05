;;; private-files.el --- Close decrypted Private/ buffers once left -*- lexical-binding: t; -*-

;;; Commentary:
;; Files under `my/private-dir' are GPG-encrypted and gpg-agent caches no
;; passphrase, but an open buffer keeps the plain text readable.  So as soon
;; as a Private *.gpg buffer is shown in no window, save it if modified and
;; kill it: coming back always means decrypting, and so the passphrase, again.

;;; Code:

(require 'epa-file)
(require 'paths)

(defun my/private-gpg-buffer-p (buf)
  "Non-nil if BUF visits an encrypted file under `my/private-dir'."
  (let ((file (buffer-file-name buf)))
    (and file
         (string-suffix-p ".gpg" file)
         (file-in-directory-p file my/private-dir))))

(defun my/private-use-default-encryption-recipient ()
  "Use the configured GPG recipient for a new Private encrypted file.
EasyPG localizes recipients after decrypting an existing file, but a new .gpg
file has no encrypted packet to read them from.  Localizing the configured
default prevents EasyPG from falling back to its recursive recipient selector."
  (when (my/private-gpg-buffer-p (current-buffer))
    (setq-local epa-file-encrypt-to
                (copy-sequence (default-value 'epa-file-encrypt-to)))))

(add-hook 'find-file-hook #'my/private-use-default-encryption-recipient)

(defvar my/private-close-in-progress nil
  "Non-nil while hidden Private buffers are being saved and closed.")

(defun my/private-encryption-prompt-active-p ()
  "Non-nil while EasyPG is recursively selecting encryption recipients."
  (and (boundp 'epa-keys-buffer)
       (buffer-live-p epa-keys-buffer)
       (with-current-buffer epa-keys-buffer
         (eq epa-exit-buffer-function #'abort-recursive-edit))))

(defun my/private-close-hidden-buffers ()
  "Save and kill Private *.gpg buffers that no window shows.
Skipped while a minibuffer or EasyPG recipient prompt is active, while another
close pass is running, and for buffers still used by an indirect buffer such as
an open `org-capture'."
  (unless (or (> (minibuffer-depth) 0)
              (my/private-encryption-prompt-active-p)
              my/private-close-in-progress)
    (let ((my/private-close-in-progress t))
      (dolist (buf (buffer-list))
        (when (and (my/private-gpg-buffer-p buf)
                   (not (get-buffer-window buf t))
                   ;; An encrypted conversation may own the active Org clock.
                   ;; Keep that buffer alive until clock-out, then close it.
                   (not (and (boundp 'org-clock-marker)
                             (markerp org-clock-marker)
                             (eq (marker-buffer org-clock-marker) buf)))
                   (not (seq-some (lambda (b) (eq (buffer-base-buffer b) buf))
                                  (buffer-list))))
          (with-current-buffer buf
            (when (buffer-modified-p)
              (condition-case err
                  (save-buffer)
                (error (message "Private: could not save %s: %s"
                                (buffer-name) (error-message-string err))))))
          (unless (buffer-modified-p buf)
            (kill-buffer buf)))))))

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
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook #'my/private-schedule-close)
  (add-hook 'org-clock-cancel-hook #'my/private-schedule-close))

(provide 'private-files)
;;; private-files.el ends here
