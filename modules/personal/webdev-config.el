;;; webdev-config.el --- JS / TS / JSX / TSX / YAML / Terraform / Env -*- lexical-binding: t; -*-

;;; Code:

;;; JavaScript (.js)
(add-hook 'js-mode-hook #'lsp-deferred)
(setq js-indent-level 2)

;;; TypeScript (.ts)
(use-package typescript-mode
  :ensure t
  :defer t
  :hook (typescript-mode . lsp-deferred)
  :config (setq typescript-indent-level 2))

;;; JSX (.jsx) — js-ts-mode; the javascript grammar parses JSX natively.
;; web-mode was removed: it owned .jsx and .tsx, but both are now handled by
;; tree-sitter modes (see treesit-config.el), and it parsed JSX with regexes.
;; If a mixed-template language ever comes up (.vue, .erb, .php), web-mode is
;; the mode to bring back for it.

;;; YAML (.yml .yaml) — yaml-language-server (npm i -g yaml-language-server)
(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yml\\'"  . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :hook (yaml-mode . lsp-deferred))

;;; Terraform (.tf .tfvars) — terraform-ls (brew install hashicorp/tap/terraform-ls)
(use-package terraform-mode
  :ensure t
  :defer t
  :hook (terraform-mode . lsp-deferred))

;;; .env files — syntax highlighting only, no LSP needed
(use-package dotenv-mode
  :ensure t
  :defer t
  :mode (("\\.env\\'"        . dotenv-mode)
         ("\\.env\\..*\\'"   . dotenv-mode)))


(provide 'webdev-config)
;;; webdev-config.el ends here
