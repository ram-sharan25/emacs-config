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

;;; JSX + TSX (.jsx .tsx) — web-mode handles mixed HTML/JS syntax
(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :hook (web-mode . lsp-deferred)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil))

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
