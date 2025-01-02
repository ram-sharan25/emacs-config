(use-package smartrep
  :ensure t
  :demand t)

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
