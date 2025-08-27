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
;; Use the Super key (now the middle key) as Meta
(setq x-super-modifier 'meta)
(define-prefix-command 'rsr/global-prefix-map)
(define-key global-map (kbd "M-m") 'rsr/global-prefix-map) ;

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
(setq python-shell-completion-native-enable nil)


;; Set maximum line length for wrapping
(setq-default fill-column 80)

;; Enable auto-fill-mode in all buffers
(add-hook 'after-change-major-mode-hook #'turn-on-auto-fill)

;; Ensure M-q (fill-paragraph) is always available
(global-set-key (kbd "M-q") #'fill-paragraph)

;; run jedi in virtual environment
 (setq jedi:server-command
      '("/Users/rrimal/.emacs.d/.venv-jedi/bin/python"
	"/Users/rrimal/.emacs.d/elpa/jedi-core-*/jediepcserver.py"))


(use-package exec-path-from-shell
  :config
  ;; This ensures your Emacs instance has the same PATH as your shell.
  (when (memq window-system '(mac ns x))
    ;; The following two lines are the fix for the slow startup.
    ;; 1. Define which environment variables to copy from the shell.
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))

    ;; 2. Enable caching. Emacs will only re-run the shell command
    ;;    if your shell config files (like .zshrc) have changed.
    (exec-path-from-shell-initialize)))
