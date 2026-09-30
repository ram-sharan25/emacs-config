;;; treesit-config.el --- Tree-sitter grammars and mode routing -*- lexical-binding: t; -*-

;;; Commentary:
;; The regex-based modes colour far fewer token kinds than VS Code does --
;; optional TypeScript properties (`title?:') are the visible example: the
;; pattern matches `name:' but the `?' breaks it, so they fall through
;; uncoloured.  Tree-sitter parses the real grammar, so the property is a
;; property whatever punctuation follows it.
;;
;; Grammars are compiled into ~/.emacs.d/tree-sitter.  To add one, extend
;; `treesit-language-source-alist' and run M-x treesit-install-language-grammar.

;;; Code:

(require 'treesit)

(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.5" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.5" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src")
        (python     "https://github.com/tree-sitter/tree-sitter-python"     "v0.20.4" "src")
        (json       "https://github.com/tree-sitter/tree-sitter-json"       "v0.20.2" "src")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml"          "v0.5.0"  "src")
        (css        "https://github.com/tree-sitter/tree-sitter-css"        "v0.20.0" "src")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash"       "v0.20.4" "src")
        (html       "https://github.com/tree-sitter/tree-sitter-html"       "v0.20.1" "src")))

;; Colour every node kind the grammars expose.  The default of 3 leaves
;; function calls and properties plain, which is the gap we are closing.
(setq treesit-font-lock-level 4)

;; Route the regex modes to their tree-sitter counterparts, but only for
;; grammars that actually loaded -- a missing grammar would otherwise leave
;; the file in fundamental-mode.
(dolist (pair '((typescript-mode . typescript-ts-mode)
                (js-mode         . js-ts-mode)
                ;; auto-mode-alist maps .js to `javascript-mode', an alias of
                ;; `js-mode'.  The remap is looked up by symbol, so the alias
                ;; needs its own entry or .js silently skips tree-sitter.
                (javascript-mode . js-ts-mode)
                (js2-mode        . js-ts-mode)
                (python-mode     . python-ts-mode)
                (json-mode       . json-ts-mode)
                (js-json-mode    . json-ts-mode)
                (yaml-mode       . yaml-ts-mode)
                (css-mode        . css-ts-mode)
                (sh-mode         . bash-ts-mode)
                (mhtml-mode      . html-ts-mode)))
  (when-let* ((ts-mode (cdr pair))
              (lang (pcase ts-mode
                      ('typescript-ts-mode 'typescript)
                      ('js-ts-mode         'javascript)
                      ('python-ts-mode     'python)
                      ('json-ts-mode       'json)
                      ('yaml-ts-mode       'yaml)
                      ('css-ts-mode        'css)
                      ('bash-ts-mode       'bash)
                      ('html-ts-mode       'html))))
    (when (treesit-language-available-p lang)
      (add-to-list 'major-mode-remap-alist pair))))

;; .tsx went to web-mode, which parses JSX with regexes.  tsx-ts-mode parses
;; it properly, so claim the extension back.  web-mode still owns .jsx and
;; plain templates.
(when (treesit-language-available-p 'tsx)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

;; .jsx too: the javascript grammar parses JSX with no errors, so js-ts-mode
;; colours tags, attributes and expressions properly.
(when (treesit-language-available-p 'javascript)
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode)))

;; The ts-modes are separate major modes, so the lsp hooks attached to the
;; regex modes do not fire for them.
(with-eval-after-load 'lsp-mode
  (dolist (hook '(typescript-ts-mode-hook
                  tsx-ts-mode-hook
                  js-ts-mode-hook
                  python-ts-mode-hook
                  yaml-ts-mode-hook
                  css-ts-mode-hook))
    (add-hook hook #'lsp-deferred)))

(provide 'treesit-config)
;;; treesit-config.el ends here
