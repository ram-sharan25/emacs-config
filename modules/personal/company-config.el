;;;; auto completion package
(use-package company
  :ensure t
  :demand t
  :custom
  (company-auto-complete t)
  (company-auto-complete-chars "")

  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-quickhelp-color-background "#4F4F4F")
  (company-quickhelp-color-foreground "#DCDCCC")

  (company-require-match nil)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 10)

  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t)
  :init
  (setf company-frontends
	'(company-echo-frontend company-pseudo-tooltip-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend))

  ;; Enable Company
  (global-company-mode 1)
  ;; bind Tab key to company completion with indent
  ;;(define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)

  (bind-keys :map company-active-map
	     ("M-n" . company-select-next)
	     ("M-p" . company-select-previous)
	     ("M-m d c" . company-show-doc-buffer)))

(use-package company-quickhelp
  :ensure t
  :after company
  :custom
  (company-quickhelp-color-background "#4F4F4F")
  (company-quickhelp-color-foreground "#DCDCCC")
  (company-quickhelp-delay 0.2)
  (company-quickhelp-mode t)
  (company-quickhelp-use-propertized-text t)
  :config
  (add-to-list 'company-frontends 'company-quickhelp-frontend))



;;; for some unknown reason
;;; company-quickhelp-delay >= company-ideal-delay + 0.1
;;; otherwise completion in slime throws error "Reply to canceled synchoronous eval request"
