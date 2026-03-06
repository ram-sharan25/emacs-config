(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 15)             ;; More suggestions for verbose languages
  (company-show-quick-access t)          ;; Quick selection numbers
  (company-require-match 'never)
  (company-dabbrev-downcase nil)        ;; Case-sensitive completion
  (company-global-modes '(not vterm-mode)) ;; Disable in specific modes

  :config
  ;; Smart backends prioritization
  (setq company-backends
	'((company-capf company-yasnippet) ;; Completion-at-point  ;; Snippet expansion
	  (company-dabbrev-code)  ;; Code-aware dabbrev
	  (company-keywords)   ;; Language keywords
	  (company-files))))  ;; File path completion


(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-nerd-icons))
(use-package company-statistics
  :ensure t
  :config (company-statistics-mode))


;; Keybinding Optimizations

(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map (kbd "<tab>") #'company-complete-selection)
(define-key company-active-map (kbd "C-w") #'company-complete-common)
(define-key company-active-map (kbd "C-j") #'company-select-next)
(define-key company-active-map (kbd "C-k") #'company-select-previous)
