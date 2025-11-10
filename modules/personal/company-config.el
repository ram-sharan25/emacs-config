(use-package company
  :ensure t
  :demand t
  :after lsp-mode ; Ensure company loads after lsp-mode
  :hook (lsp-mode . company-mode) ; Automatically enable company-mode when LSP is active
  :custom
  (company-idle-delay 0.1)               ;; Faster response for experienced users
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 15)             ;; More suggestions for verbose languages
  (company-show-quick-access t)          ;; Quick selection numbers
  (company-require-match 'never)
  (company-dabbrev-downcase nil)        ;; Case-sensitive completion
  (company-global-modes '(not vterm-mode)) ;; Disable in specific modes

  :config
  (global-company-mode 1)

  ;; Smart backends prioritization
  (setq company-backends
	'((company-capf company-yasnippet) ;; Completion-at-point  ;; Snippet expansion
	  (company-dabbrev-code)  ;; Code-aware dabbrev
	  (company-keywords)   ;; Language keywords
	  (company-files))))  ;; File path completion


;; Enhanced UI & Features
;; Ensure all-the-icons is available for company-box
(use-package all-the-icons
  :ensure t)

;; Enhanced UI & Features for Company
;; With use-package:
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Or:
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)
(use-package company-statistics
  :ensure t
  :config (company-statistics-mode))


;; Keybinding Optimizations

(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map (kbd "<tab>") #'company-complete-selection)
(define-key company-active-map (kbd "C-w") #'company-complete-common)
(define-key company-active-map (kbd "C-j") #'company-select-next)
(define-key company-active-map (kbd "C-k") #'company-select-previous)
