(require 'my-secrets)
(use-package gptel
  :ensure t
  :bind (:map rsr/global-prefix-map
              (("g g" . gptel-send)
               ("g b" . gptel)
               ("g f" . gptel-add-file)
               ("g a" . gptel-context-add)
               ("g r" . gptel-context-remove-all)
               ("g q" . gptel-abort)))
  :config
  ;; Set the api key in .authinfo file like:
  ;; machine generativelanguage.googleapis.com apikey password <<YOUR_API_KEY>>
  (setf gptel-default-mode 'org-mode
        gptel-model 'gemini-flash-latest
        gptel-backend (gptel-make-gemini "Gemini" :stream t :key my/gemini-key)))
