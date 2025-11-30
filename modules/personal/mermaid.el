;;; mermaid.el --- Mermaid configuration -*- lexical-binding: t; -*-

(require 'paths)
(require 'ob-mermaid)

;; Use the specific chrome version installed via puppeteer
(setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")

(defun my/org-babel-mermaid-default-dir (orig-fun &rest args)
  "Advice to force Mermaid output to `my/data-dir` unless absolute path is given."
  (let ((default-directory my/data-dir))
    (apply orig-fun args)))

(advice-add 'org-babel-execute:mermaid :around #'my/org-babel-mermaid-default-dir)

(provide 'mermaid)
;;; mermaid.el ends here
