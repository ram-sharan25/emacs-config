;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Runs before package and UI initialization.
;; Disabling UI elements here prevents drawing them then immediately hiding
;; them — which causes visible flicker and wastes startup time.

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)           ;; always load source over stale .elc

;; Third-party packages emit deprecation warnings during native compilation
;; (obsolete `cl', `loop', etc.) that we cannot fix upstream. Keep them in
;; *Warnings* for debugging, but don't let them steal window focus.
(setq native-comp-async-report-warnings-errors 'silent)

;; Modifier keys — emacs-mac (railwaycat) port.
;; The Mac port defaults to Command=meta / Option=alt, but this config was
;; written for the NS build's layout (Option=meta, Command=super): every s-
;; binding here assumes Cmd, e.g. "s-F" is documented as Cmd+Shift+F.
;; Set these early so a failure in any later module can't cost us M-x.
(when (boundp 'mac-option-modifier)
  (setq mac-option-modifier 'meta      ;; Option-x = M-x
        mac-command-modifier 'super))  ;; Cmd = s- (save/copy/paste, s-/ s-l ...)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
