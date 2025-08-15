(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq pdf-view-use-scaling t
	pdf-view-display-size 'fit-width
	pdf-annot-activate-created-annotations t)
  :config
  (pdf-loader-install)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-pdftools
  :ensure t
  :hook ((org-mode . org-pdftools-setup-link)
	 (pdf-view-mode . (lambda () (require 'org-pdftools)))))
