;;; evil-config.el --- Vim editing, in editing buffers only -*- lexical-binding: t; -*-

;;; Commentary:
;; Vim modal editing for files you edit.  Magit, Dired, the agenda, Info,
;; help and every other special-purpose buffer keep their native Emacs keys:
;; `evil-default-state' is emacs, and normal state is switched on only for
;; prog-mode, text-mode and conf-mode.  That is the whole scoping rule --
;; there is no list of modes to maintain.
;;
;; Nothing in this config collided with evil.  Every key bound elsewhere is
;; under s- (Cmd), C-c, C-x or the M-m prefix, and evil claims none of those,
;; so all existing bindings keep working in both states.
;;
;; Three deliberate departures from evil's defaults are below, each commented
;; where it is set: C-u, TAB, and the undo system.

;;; Code:

(defvar rsr/evil-editing-modes
  '(prog-mode text-mode conf-mode)
  "Modes where vim editing is wanted, derived modes included.
Everything else stays in emacs state.")

(use-package evil
  :ensure t
  :init
  ;; C-u stays `universal-argument'.  Vim scrolls with it, but in Emacs it is
  ;; how every command receives a count or a variation, and this config
  ;; documents flows that depend on it (C-u M-m f c).  Scroll with C-b / M-v.
  (setq evil-want-C-u-scroll nil)

  ;; C-i is the same key as TAB on a terminal, and evil binds it to
  ;; jump-forward in normal state.  That would shadow TAB -- org heading
  ;; cycling, indentation, completion -- so leave TAB alone.
  (setq evil-want-C-i-jump nil)

  ;; Start every buffer in emacs state; the hooks below opt editing buffers
  ;; into normal state.  Doing it this way round means a newly installed
  ;; package's special-purpose buffers behave normally without being listed.
  (setq evil-default-state 'emacs)

  :config
  ;; `u' and C-r drive Emacs 28+ native undo/redo, so they share one history
  ;; with s-z and C-/ rather than keeping a second, divergent one.
  (setq evil-undo-system 'undo-redo)

  ;; `evil-set-initial-state', not a mode hook calling `evil-normal-state':
  ;; evil decides a buffer's state from `after-change-major-mode-hook', which
  ;; runs after mode hooks, so a hook would be overridden right back to emacs
  ;; state.  These entries are consulted by derived modes too, so emacs-lisp,
  ;; typescript-ts, org and friends inherit from prog-mode / text-mode.
  (dolist (mode rsr/evil-editing-modes)
    (evil-set-initial-state mode 'normal))

  ;; SPC opens `rsr/global-prefix-map' -- the same map as M-m, not a copy.
  ;; Anything bound there is reachable both ways, and new entries appear
  ;; under both automatically.  SPC is free in normal state; its motion
  ;; (forward-char) is `l'.
  (evil-define-key '(normal visual motion) 'global
    (kbd "SPC") rsr/global-prefix-map)

  (evil-mode 1))

(provide 'evil-config)
;;; evil-config.el ends here
