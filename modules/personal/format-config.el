;;; format-config.el --- Autoformat on save, per project -*- lexical-binding: t; -*-

;;; Commentary:
;; The Emacs equivalent of VS Code's Biome extension with formatOnSave.
;;
;; `apheleia' runs the formatter in a subprocess and applies the result as a
;; patch, so point, scroll position and the mark survive formatting -- unlike
;; the naive replace-buffer-contents approach, which makes the window jump on
;; large files.
;;
;; Formatter choice is per project, not per major mode.  `apheleia-mode-alist'
;; is global and already maps the ts/js modes to prettier, so hardcoding biome
;; there would reformat a prettier project with the wrong tool.  Instead
;; `rsr/apheleia-pick-formatter' looks for a config file and sets the
;; buffer-local `apheleia-formatter', which overrides the mode alist:
;;
;;   biome.json / biome.jsonc  ->  biome
;;   .prettierrc & friends     ->  apheleia's default (prettier-*)
;;   neither                   ->  nothing; the buffer is left alone
;;
;; That last case matters: without it, apheleia would run prettier via npx in
;; any JS project, reformatting whole files to prettier defaults in a repo that
;; never opted into a formatter.
;;
;; Both formatters are invoked through `apheleia-npx', which resolves
;; node_modules/.bin from the file's directory upward, so the project-local
;; binary is used and nothing needs to be on PATH.

;;; Code:

(defconst rsr/biome-config-files '("biome.json" "biome.jsonc")
  "Files whose presence means a project formats with biome.")

(defconst rsr/prettier-config-files
  '(".prettierrc" ".prettierrc.json" ".prettierrc.yaml" ".prettierrc.yml"
    ".prettierrc.js" ".prettierrc.cjs" ".prettierrc.mjs" ".prettierrc.toml"
    "prettier.config.js" "prettier.config.cjs" "prettier.config.mjs")
  "Files whose presence means a project formats with prettier.")

(defun rsr/locate-config (dir names)
  "Walk up from DIR looking for any file in NAMES."
  (locate-dominating-file
   dir
   (lambda (d) (seq-some (lambda (f) (file-exists-p (expand-file-name f d))) names))))

(defun rsr/apheleia-configure ()
  "Choose this buffer's formatter; return non-nil to leave the buffer alone.

Runs from `apheleia-inhibit-functions', which `apheleia-mode-maybe' calls
when deciding whether to enable the mode.  That is the one moment where
both jobs can be done: the buffer is fully set up, and nothing has been
formatted yet.  Doing this from `find-file-hook' is too late -- the
globalized mode has already enabled `apheleia-mode' by then, and
`apheleia-inhibit' is not consulted again at format time."
  (when-let* ((file buffer-file-name)
              (dir (file-name-directory file)))
    (cond
     ((rsr/locate-config dir rsr/biome-config-files)
      (setq-local apheleia-formatter 'biome)
      nil)
     ((rsr/locate-config dir rsr/prettier-config-files)
      nil)                              ; keep the mode default
     ((derived-mode-p 'tsx-ts-mode 'typescript-ts-mode 'js-ts-mode
                      'json-ts-mode 'css-ts-mode)
      ;; A JS-family project with no formatter config: do not impose one,
      ;; or saving would reformat whole files to prettier defaults.
      t))))

(use-package apheleia
  :ensure t
  :config
  (add-hook 'apheleia-inhibit-functions #'rsr/apheleia-configure)
  (apheleia-global-mode +1)
  ;; Format without saving, for tidying mid-edit.
  (global-set-key (kbd "C-c f") #'apheleia-format-buffer))

(provide 'format-config)
;;; format-config.el ends here
