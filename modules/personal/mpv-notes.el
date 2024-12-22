(use-package smartrep
  :ensure t
  :demand t)

(use-package org-mpv-notes
  :ensure t
  :after org-download
  :commands (org-mpv-notes-mode org-mpv-notes-open)
  :hook (org-mode . org-mpv-notes-setup-link)
  :config
  (setq org-mpv-notes-save-image-function #'org-download-image)
  (define-key org-mpv-notes-mode-map (kbd "M-n") (smartrep-map org-mpv-notes-key-bindings)))

(use-package mpv
  :ensure t)
