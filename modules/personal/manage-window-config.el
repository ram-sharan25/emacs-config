;;; manage-window-config.el --- Window management and completion -*- lexical-binding: t; -*-

;;; Code:

;;; --- Window Splits ---

(defun rsr/split-vertical ()
  "Split window right and open scratch buffer in new window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (scratch-buffer))

(defun rsr/split-horizontal ()
  "Split window below and open scratch buffer in new window."
  (interactive)
  (split-window-below)
  (other-window 1)
  (scratch-buffer))

;;; --- Vertico ---

(use-package vertico
  :ensure t
  :demand t
  :custom
  (vertico-count 12)   ;; reasonable height
  (vertico-cycle t)
  :init
  (vertico-mode))

;;; --- Orderless (fuzzy matching) ---

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  ;; `orderless' comes FIRST for files.  Vertico's README suggests only
  ;; (basic partial-completion) here, to keep ~/ and TRAMP paths expanding
  ;; during directory navigation -- but both are prefix-anchored, so typing
  ;; `print' never surfaces app/components/PrintLink.tsx.
  ;;
  ;; Appending orderless does not fix that: `completion-all-completions' tries
  ;; styles in order and returns the FIRST that matches anything, so
  ;; partial-completion matching a literal print/ directory ends the search
  ;; and orderless never runs.  It has to lead.
  (completion-category-overrides
   '((file (styles orderless partial-completion basic))))
  ;; Projectile hands `completing-read' a plain list of relative paths, not a
  ;; file-name collection, so `read-file-name-completion-ignore-case' does not
  ;; apply to it and `print' would not match `Print'.
  (completion-ignore-case t))

;;; --- Marginalia (annotations in minibuffer) ---

(use-package marginalia
  :ensure t
  :demand t
  :init
  (marginalia-mode))

;;; Emacs 29 compatibility — consult 3.x references this Emacs 30 variable
(unless (boundp 'minibuffer-visible-completions)
  (defvar minibuffer-visible-completions nil))

;;; --- Consult (enhanced commands) ---

(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
         ("C-x M-f" . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ;; consult README: turn a C-s search into the line list.
         :map isearch-mode-map
         ("M-s l"   . consult-line)))

;;; --- Embark (context-sensitive actions) ---

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)            ;; act on thing at point or current candidate
         ("C-;" . embark-dwim))          ;; do-what-i-mean (smarter default action)
  :config
  (setq embark-prompter 'embark-keymap-prompter))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; savehist — M-x remembers command history (replaces smex)
(savehist-mode 1)

;;; --- Window Navigation ---

;; ace-window — jump, swap, close windows with letter overlays.  Replaces
;; `other-window' (C-x o) via a remap in keymaps.el, as its README suggests.
(use-package ace-window
  :ensure t
  :defer t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always t)
  ;; The letter overlay merges with the face of the text it covers.  Over a
  ;; Nerd Font icon (the agent-sessions dashboard's first line) it inherits
  ;; the symbols-only :family and draws as an empty box; pin the family.
  (set-face-attribute 'aw-leading-char-face nil
                      :family (face-attribute 'default :family)))

(defun rsr/toggle-window-split ()
  "Toggle between horizontal and vertical split for two windows."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Only works with exactly 2 windows"))
  (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (vertical-p (= (car this-win-edges)
                        (car (window-edges (next-window))))))
    (delete-other-windows)
    (if vertical-p
        (split-window-horizontally)
      (split-window-vertically))
    (set-window-buffer (selected-window) this-win-buffer)
    (set-window-buffer (next-window) next-win-buffer)))

;;; Keybindings
;; C-x 2 / C-x 3 / C-x w t are in keymaps.el.

;; Unbind windmove shift+arrow keys — conflict with org scheduling
(global-unset-key [S-left])
(global-unset-key [S-right])
(global-unset-key [S-up])
(global-unset-key [S-down])

(provide 'manage-window-config)
;;; manage-window-config.el ends here
