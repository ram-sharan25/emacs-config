;;; Package Setup

(require 'package)
(package-initialize)
;; compat must be in load-path early — required by org-timeblock and other packages
(when-let ((compat-dir (car (last (sort
                                   (seq-filter
                                    (lambda (d) (not (string-suffix-p ".signed" d)))
                                    (file-expand-wildcards
                                     (expand-file-name "elpa/compat-*" user-emacs-directory)))
                                   #'string<)))))
  (add-to-list 'load-path compat-dir))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Paths

(setf custom-file (expand-file-name ".custom" user-emacs-directory))
(push "/Users/rrimal/.local/bin" exec-path)
(setenv "PATH" (concat "/Users/rrimal/.local/bin/:" (getenv "PATH")))
(add-to-list 'load-path (expand-file-name "modules/personal"    user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/git-modules" user-emacs-directory))

;;; Global Prefix

(setq x-super-modifier 'meta)
(define-prefix-command 'rsr/global-prefix-map)
(define-key global-map (kbd "M-m") 'rsr/global-prefix-map)

;;; Module Loader

(defun load-directory (directory)
  "Load all .el files in DIRECTORY."
  (dolist (file (directory-files directory t "\\.el$"))
    (load (file-name-sans-extension file))))

(load-directory (expand-file-name "modules/personal"    user-emacs-directory))
(load-directory (expand-file-name "modules/git-modules" user-emacs-directory))

;;; Shell PATH

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))

;;; Appearance

(global-font-lock-mode 1)
(set-face-attribute 'default nil :height 160)

;;; Icons

(use-package all-the-icons
  :defer t)

(use-package all-the-icons-ibuffer
  :commands all-the-icons-ibuffer-mode
  :hook (after-init-hook . all-the-icons-ibuffer-mode))

;;; Editing Defaults

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(dolist (hook '(emacs-lisp-mode-hook
               python-mode-hook
               js-mode-hook
               typescript-mode-hook
               css-mode-hook
               html-mode-hook
               sh-mode-hook
               c-mode-hook
               c++-mode-hook
               java-mode-hook
               ruby-mode-hook
               rust-mode-hook
               go-mode-hook))
  (add-hook hook #'display-fill-column-indicator-mode))
(add-hook 'after-change-major-mode-hook #'turn-on-auto-fill)
(global-set-key (kbd "M-q") #'fill-paragraph)
(setq dired-use-ls-dired nil)

;;; Org Defaults

(setq org-src-fontify-natively t
      org-hide-emphasis-markers t
      org-confirm-babel-evaluate nil
      python-shell-completion-native-enable nil)

(require 'org)
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell  . t)))

;;; Server

(require 'server)
(unless (server-running-p)
  (server-start))

;;; Agent Skills (Claude Code)

(dolist (skill '("describe" "highlight" "open" "select" "dired"))
  (let ((path (expand-file-name
               (concat ".agent/skills/" skill)
               user-emacs-directory)))
    (add-to-list 'load-path path)
    (require (intern (concat "agent-skill-" skill)) nil t)))
(put 'narrow-to-region 'disabled nil)
