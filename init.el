

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

;; Custom's writes go nowhere: this config is hand-written elisp, and a
;; custom-file that is written but never loaded is a trap -- it accumulated a
;; stale copy of every face here and would have silently overridden the theme
;; the day anyone added (load custom-file).
(setf custom-file null-device)

(defun my/add-local-exec-paths ()
  "Add available user and Homebrew executable directories to PATH."
  (dolist (directory (list "/opt/homebrew/bin"))
    (when (file-directory-p directory)
      (add-to-list 'exec-path directory)
      (setenv "PATH" (concat directory path-separator (getenv "PATH"))))))

(my/add-local-exec-paths)
(add-to-list 'load-path (expand-file-name "modules/personal"    user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/git-modules" user-emacs-directory))

;;; Global Prefix

(define-prefix-command 'rsr/global-prefix-map)
(define-key global-map (kbd "M-m") 'rsr/global-prefix-map)

;;; Module Loader

(defun load-directory (directory)
  "Load all .el files in DIRECTORY."
  (dolist (file (directory-files directory t "\\.el$"))
    (load (file-name-sans-extension file))))

;; Refresh foundational path constants before loading modules.  This explicit
;; load also makes reloading init.el pick up newly added paths in a live session,
;; where `(require 'paths)' would otherwise keep the older definitions.
(load (expand-file-name "modules/personal/paths.el" user-emacs-directory))
(load-directory (expand-file-name "modules/personal"    user-emacs-directory))
(load-directory (expand-file-name "modules/git-modules" user-emacs-directory))

;;; Shell PATH

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize)
    (my/add-local-exec-paths)))

;;; Appearance

(global-font-lock-mode 1)
;; :weight medium is deliberate, not the default match falling through.
;; Installed cuts here: regular, medium, semi-bold, bold (no light).  Keyword
;; and constant faces track this weight -- see `rsr/match-keyword-weight' in
;; theme-config.el -- so changing it here keeps the whole buffer consistent.
(set-face-attribute 'default nil :family "Fira Code" :height 173 :weight 'medium)

;;; Line spacing
;;
;; A float, not a pixel count: a float is a multiple of the line's own height,
;; so org at Monaco 16pt and code at Fira Code 17pt each get proportional
;; spacing rather than one fixed gap tuned to whichever buffer came first.
;;
;; Known cost, accepted: `line-spacing' puts its pixels *below* the glyphs and
;; paints them with the line's face, so selections, hl-line and diff bands sit
;; slightly under their text.  Emacs has added the space below since 21.1 and
;; cannot centre it -- a `line-spacing-vertical-center' patch went to
;; emacs-devel in 2019 and never landed.
;;
;; The `line-height' text property can add space above instead, and was tried.
;; It is per-newline with an absolute pixel value, so a buffer at a different
;; font size gets the wrong gap -- smaller fonts get bigger gaps, backwards --
;; and it needs a jit-lock hook in every buffer to stay applied.  Not worth the
;; machinery; don't reach for it again without remembering why.
(setq-default line-spacing 0.175)

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
 (setq org-latex-prefer-user-labels t)
