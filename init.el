(cond ((eq system-type 'windows-nt)
       (add-to-list 'package-archives
		    '("melpa" . "http://melpa.org/packages/") t))
      (t
       (add-to-list 'package-archives
		    '("melpa" . "https://melpa.org/packages/") t)))
(package-initialize)


(setf custom-file (expand-file-name ".custom" user-emacs-directory))

;; Add the modules/personal directory to the load path
(add-to-list 'load-path (expand-file-name "modules/personal" user-emacs-directory))


(define-prefix-command 'bp/global-prefix-map)
(define-key global-map (kbd "M-m") 'bp/global-prefix-map)

;; Install tree-sitter grammar for typescript and tsx






;; Function to load all .el files from a directory
(defun load-directory (directory)
  "Load all .el files in DIRECTORY."
  (let ((files (directory-files directory t "\\.el$")))
    (dolist (file files)
      (load (file-name-sans-extension file)))))

;; Load all .el files from the personal directory
(load-directory (expand-file-name "modules/personal" user-emacs-directory))






(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))


;; show lines in all modes
(global-display-line-numbers-mode 1)


(setq org-src-fontify-natively t)  ;; Enable syntax highlighting for source code blocks

(global-font-lock-mode 1)

(set-face-attribute 'default nil :height 160)
