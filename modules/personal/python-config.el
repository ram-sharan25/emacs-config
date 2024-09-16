
;;python3 configs
;; Load support for Python code blocks


(require 'org)
(require 'ob-python)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))


(setq org-src-fontify-natively t)  ;; Enable syntax highlighting for source code blocks

(global-font-lock-mode 1)

(set-face-attribute 'default nil :height 160)

