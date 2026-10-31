;;; linear-config.el --- Linear issues in Org -*- lexical-binding: t -*-

;;; Commentary:
;; linear-emacs is cloned, not on MELPA.  Its deps (request, dash, s) come from
;; ~/.emacs.d/elpa.  Issues land in Brain/Dashboard/linear.org.
;; Keys live under C-c j (`rsr/linear-map' in keymaps.el).

;;; Code:

(add-to-list 'load-path
             (expand-file-name "~/Stillness/Development/cloned-projects/linear-emacs"))
(require 'org)
(require 'paths)
(require 'linear-emacs)
(setq linear-emacs-org-file-path (expand-file-name "linear.org" my/dashboard-dir))

;; The key lives in ~/.emacs.d/secrets.el.gpg as `my/linear-key'.  gcal.el
;; usually loads that file at startup already; otherwise decrypt it on the
;; first Linear request rather than at startup, so launching never prompts.
(defun rsr/linear-load-key (&rest _)
  (unless linear-emacs-api-key
    (unless (or (boundp 'my/linear-key) (fboundp 'my/linear-key))
      (require 'epa-file)
      (setq epg-pinentry-mode 'loopback)
      (load "~/.emacs.d/secrets.el.gpg"))
    (setq linear-emacs-api-key (if (functionp 'my/linear-key)
                                   (my/linear-key)
                                 my/linear-key))))
(advice-add 'linear-emacs--headers :before #'rsr/linear-load-key)

(defun rsr/linear-open-org ()
  "Open the Org file Linear issues are pulled into."
  (interactive)
  (find-file linear-emacs-org-file-path))

(provide 'linear-config)
;;; linear-config.el ends here
