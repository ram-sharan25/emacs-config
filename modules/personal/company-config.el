;; Company Mode Core Setup
(use-package company
  :ensure t
  :demand t
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
	'(company-files          ;; File path completion
	  (company-capf          ;; Completion-at-point
	   company-dabbrev-code  ;; Code-aware dabbrev
	   company-keywords)     ;; Language keywords
	   company-yasnippet)))  ;; Snippet expansion


;; Language-Specific Support

;; Python
(use-package company-jedi
  :ensure t
  :hook (python-mode . (lambda () (add-to-list 'company-backends 'company-jedi))))

;; C/C++
(use-package company-rtags
  :ensure t
  :hook (c-mode-common . (lambda () (add-to-list 'company-backends 'company-rtags))))


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
