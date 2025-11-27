;; Add the DIRECTORY containing the theme file to the load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") ;; Assumes this is the correct path

;; Now, load the theme by its symbol name
(load-theme 'zenburn t)

;; Comprehensive Visual Cleanup (Zenburn Optimized)
(custom-set-faces
 ;; Make scaffolding (drawer lines, meta lines) small and dim
 '(org-drawer ((t (:foreground "#b42cbeff" :height 0.8))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :height 0.8))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :height 0.8))))
 '(org-property-value ((t (:height 0.8))))
 
 ;; Give Blocks (Quotes, Src) a subtle background to make them pop
 '(org-block ((t (:background "#2B2B2B" :family "Fira Code" :extend t)))) ;; Zenburn Deep (Premium Dark)
 '(org-block-begin-line ((t (:inherit org-meta-line :background "#2B2B2B" :extend t))))
 '(org-block-end-line ((t (:inherit org-meta-line :background "#2B2B2B" :extend t))))
 
 ;; Make Quotes specifically Pale Mint on Slate (Cool contrast to Zenburn)
 '(org-quote ((t (:inherit org-block :slant italic :foreground "#DEF7E0" :background "#4A5057")))))
;; ;;(load-theme ../../themes/mac-os-theme.el t)



(use-package org-bullets
  :ensure t
  :hook (org-mode-hook . org-bullets-mode))
;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-medium t)

;;   ;; Softer "white" for main text
;;   (set-face-attribute 'default nil :foreground "#dddddd")

;;   ;; Optional: keep comments dim, but readable
;;   (custom-set-faces
;;    '(font-lock-comment-face ((t (:foreground "#928374"))))
;;    '(font-lock-string-face  ((t (:foreground "#b8bb26")))))
;; )
