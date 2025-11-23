;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; WEB DEVELOPMENT CONFIGURATION (JS/TS/React)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Core language modes
;; (use-package rjsx-mode
;;   :mode (("\\.js\\'" . rjsx-mode) ("\\.jsx\\'" . rjsx-mode))
;;   :config
;;   (setq js2-basic-offset 2 sgml-basic-offset 2)
;;   (with-eval-after-load 'rjsx-mode
;;     (define-key rjsx-mode-map "<" nil)
;;     (define-key rjsx-mode-map ">" nil)))

;; (use-package typescript-mode
;;   :mode ("\\.tsx?\\'" . typescript-mode)
;;   :config
;;   (setq typescript-indent-level 2))


;; ;; Tell LSP how to handle JSX syntax
;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascriptreact"))
;;   (setq lsp-typescript-suggest-auto-imports t
;;   lsp-typescript-format-enable t
;;   lsp-typescript-preferences-quote-style "single"))

;; ;; Development tools
;; (use-package add-node-modules-path
;;   :hook ((rjsx-mode . add-node-modules-path)
;;    (typescript-mode . add-node-modules-path)))

;; (use-package prettier-js)
;; (defun my-add-prettier-keybinding ()
;;   "Add keybinding for manual Prettier formatting."
;;   (local-set-key (kbd "M-F") #'prettier-js))
;; (add-hook 'rjsx-mode-hook #'my-add-prettier-keybinding)
;; (add-hook 'typescript-mode-hook #'my-add-prettier-keybinding)

;; (use-package emmet-mode
;;   :hook ((rjsx-mode . emmet-mode)
;;    (typescript-mode . emmet-mode)
;;    (css-mode . emmet-mode))
;;   :config (setq emmet-expand-jsx-className? t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; YAML AND .ENV FILE SUPPORT
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; A. Install Major Modes for Syntax Highlighting
;; ;; This ensures .yml and .env files get proper colors and indentation.
;; (use-package yaml-mode
;;   :ensure t
;;   :mode "\\.ya?ml\\'")

;; (use-package dotenv-mode
;;   :ensure t
;;   :mode "\\.env\\'")

;; ;; B. Enable Autocompletion for YAML
;; ;; This hooks lsp-mode into yaml-mode. It will automatically start the
;; ;; yaml-language-server (if you installed it via npm).
;; (add-hook 'yaml-mode-hook #'lsp-deferred)


;; (add-hook 'rjsx-mode-hook #'lsp-deferred)
;; (add-hook 'typescript-mode-hook #'lsp-deferred)
;; (add-hook 'js-mode-hook #'lsp-deferred)
;; ;; React snippets and helpers
;; (use-package react-snippets :after yasnippet)

;; (defun my/insert-react-component (name)
;;   "Insert a React functional component template."
;;   (interactive "sComponent name: ")
;;   (insert (format "import React from 'react';

;; const %s = () => {
;;   return (
;;     <div>
;;       <h1>%s Component</h1>
;;     </div>
;;   );
;; };

;; export default %s;" name name name)))

;; (defun my/insert-react-hook (hook-name)
;;   "Insert a custom React hook template."
;;   (interactive "sHook name (without 'use' prefix): ")
;;   (let ((full-hook-name (if (string-prefix-p "use" hook-name) hook-name
;;         (concat "use" (capitalize hook-name)))))
;;     (insert (format "import { useState, useEffect } from 'react';

;; const %s = () => {
;;   const [state, setState] = useState();

;;   useEffect(() => {
;;     // Effect logic here
;;   }, []);

;;   return state;
;; };

;; export default %s;" full-hook-name full-hook-name))))

;; (global-set-key (kbd "C-c r c") 'my/insert-react-component)
;; (global-set-key (kbd "C-c r h") 'my/insert-react-hook)

;; ;; LSP navigation keybindings for web modes
;; (defun my-dev-mode-setup-keys ()
;;   "Set up keybindings for LSP/xref navigation in dev modes."
;;   (define-key (current-local-map) (kbd "M-.") #'lsp-find-definition)
;;   (define-key (current-local-map) (kbd "M-,") #'xref-pop-marker-stack)
;;   (define-key (current-local-map) (kbd "M-?") #'lsp-find-references)
;;   (define-key (current-local-map) (kbd "C-M-.") #'lsp-find-implementation))

;; (add-hook 'rjsx-mode-hook #'my-dev-mode-setup-keys)
;; (add-hook 'typescript-mode-hook #'my-dev-mode-setup-keys)

;; ;; This line is crucial for modular configurations
;; (provide 'web-dev-config)
