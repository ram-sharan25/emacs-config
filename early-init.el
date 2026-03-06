;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Runs before package and UI initialization.
;; Disabling UI elements here prevents drawing them then immediately hiding
;; them — which causes visible flicker and wastes startup time.

(setq package-enable-at-startup nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
