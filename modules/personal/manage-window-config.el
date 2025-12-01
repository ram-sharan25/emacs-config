;; manage windows in emacs

(defun rsr/split-vertical ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (scratch-buffer))

(global-set-key (kbd "C-x 3") 'rsr/split-vertical)

(defun rsr/split-horizontal ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (scratch-buffer))

(global-set-key (kbd "C-x 2") 'rsr/split-horizontal)


;; Enable Vertico.
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
   (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))


(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0) ;; Example: customize a specific variable here
  :init
  (marginalia-mode))
