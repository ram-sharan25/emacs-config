

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

;; Vendored modules require Compat even when Emacs itself is recent.
(use-package compat :ensure t :demand t)

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

;;; Machine-local overrides (optional, never committed)

(load (expand-file-name "local.el" user-emacs-directory) t t)

;;; Module Loader

(defvar rsr/local-disabled-modules nil
  "Module basenames disabled by this machine's local.el, without .el suffix.")

(defun load-directory (directory)
  "Load .el files in DIRECTORY except explicitly disabled local modules."
  ;; Skip .#lock, #autosave# and flycheck_ temp files left beside a module
  ;; being edited; loading one of those can abort startup.
  (dolist (file (directory-files directory t "\\`[^.#].*\\.el\\'"))
    (cond
     ((string-prefix-p "flycheck_" (file-name-nondirectory file)))
     ((member (file-name-base file) rsr/local-disabled-modules)
      (display-warning 'local-config
                       (format "Module %s is disabled locally; install its dependencies and update local.el."
                               (file-name-base file))))
     (t (load (file-name-sans-extension file))))))

;; Refresh foundational path constants before loading modules.  This explicit
;; load also makes reloading init.el pick up newly added paths in a live session,
;; where `(require 'paths)' would otherwise keep the older definitions.
(load (expand-file-name "modules/personal/paths.el" user-emacs-directory))
;; Global keys and prefix maps next, so every module can bind into them.
;; `load-directory' loads it again; harmless at startup, since `defvar-keymap'
;; won't redefine.  The flip side: after editing a prefix map, restart Emacs;
;; reloading leaves the old map (and removed keys) in place.
(load (expand-file-name "modules/personal/keymaps.el" user-emacs-directory))
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
;; so org at Menlo 16pt and code at Fira Code 17pt each get proportional
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
  :hook (after-init . all-the-icons-ibuffer-mode))

;;; Editing Defaults

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
;; The *-base-mode hooks run for both the regex mode and its tree-sitter
;; counterpart (python-mode / python-ts-mode, js-mode / js-ts-mode, ...).
;; treesit-config.el remaps to the ts-modes, so a hook on the regex mode
;; alone never fires.
(dolist (hook '(emacs-lisp-mode-hook
               python-base-mode-hook
               js-base-mode-hook
               typescript-mode-hook
               typescript-ts-base-mode-hook
               css-base-mode-hook
               html-mode-hook
               sh-base-mode-hook
               c-mode-hook
               c++-mode-hook
               java-mode-hook
               ruby-mode-hook
               rust-mode-hook
               go-mode-hook))
  (add-hook hook #'display-fill-column-indicator-mode))
(add-hook 'after-change-major-mode-hook #'turn-on-auto-fill)
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

(put 'narrow-to-region 'disabled nil)
 (setq org-latex-prefer-user-labels t)
