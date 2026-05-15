;;; mermaid.el --- Mermaid configuration -*- lexical-binding: t; -*-

(require 'paths)
(require 'ob-mermaid)

;; Use the specific chrome version installed via puppeteer
(setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")

;; Default to dark theme with transparent background for dark Emacs themes
(setf (alist-get :theme org-babel-default-header-args:mermaid) "dark")
(setf (alist-get :background-color org-babel-default-header-args:mermaid) "transparent")

(defun my/org-babel-mermaid-default-dir (orig-fun &rest args)
  "Advice to force Mermaid output to `my/data-dir` unless absolute path is given."
  (let ((default-directory my/data-dir))
    (apply orig-fun args)))

(advice-add 'org-babel-execute:mermaid :around #'my/org-babel-mermaid-default-dir)

(defun my/org-babel-mermaid-fix-result-link ()
  "Replace file: with data: in the #+RESULTS block after mermaid execution.
Uses the `data:` org-link abbreviation defined in org-config.el.
Runs from `org-babel-after-execute-hook' so the result is already inserted."
  (when (string= "mermaid" (org-element-property :language (org-element-at-point)))
    (save-excursion
      (goto-char (org-babel-where-is-src-block-result))
      (forward-line 1)
      (when (looking-at "\\[\\[file:\\([^]]+\\)\\]\\]")
        (replace-match "[[data:\\1]]")))))

(add-hook 'org-babel-after-execute-hook #'my/org-babel-mermaid-fix-result-link)

(provide 'mermaid)
;;; mermaid.el ends here
