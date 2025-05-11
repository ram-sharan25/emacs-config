(require 'package)
(package-initialize)  ;; Ensure packages are initialized

;; Add MELPA repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install `use-package` if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Always ensure packages are installed
(setq use-package-always-ensure t)

(setf custom-file (expand-file-name ".custom" user-emacs-directory))

(push "/Users/rrimal/.local/bin" exec-path)
(setenv "PATH" (concat "/Users/rrimal/.local/bin/:" (getenv "PATH")))

;; Add personal directory to load path
(add-to-list 'load-path (expand-file-name "modules/personal" user-emacs-directory))

(define-prefix-command 'rsr/global-prefix-map)
(define-key global-map (kbd "M-m") 'rsr/global-prefix-map)

;; Function to load all .el files from a directory
(defun load-directory (directory)
  "Load all .el files in DIRECTORY."
  (let ((files (directory-files directory t "\\.el$")))
    (dolist (file files)
      (message "Loading %s" file)
      (load (file-name-sans-extension file)))))

;; Install & configure swiper
(use-package swiper
  :bind ("C-s" . swiper))

(setq org-src-fontify-natively t)  ;; Enable syntax highlighting for source code blocks
(global-font-lock-mode 1)
(set-face-attribute 'default nil :height 160)

;; Load custom modules AFTER setting up `use-package`
(load-directory (expand-file-name "modules/personal" user-emacs-directory))

(setq dired-use-ls-dired nil)

(require 'org)
(setq org-babel-python-command "python3") ; Or "python"
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))
(setq org-confirm-babel-evaluate nil)
