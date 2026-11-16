;;; magit_config.el --- Git interface via magit -*- lexical-binding: t; -*-

;;; Code:

(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-diff-hide-trailing-cr-characters t)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :custom
  (magit-diff-refine-hunk t)   ;; highlight changed words within a hunk
  ;; Scanned, not listed: any repo under these shows up on its own.
  (magit-repository-directories my/project-search-dirs))
;; C-x g, C-c M-g and C-x M-g come from magit's own global bindings.

;; forge — GitHub issues and PRs inside magit (N in a magit buffer)
;; The token lives in `my/secrets-file' as `my/github-token', not in
;; authinfo.  gcal.el usually loads that file at startup; otherwise decrypt it
;; on the first GitHub request.  Other hosts still fall through to auth-source.
(use-package forge
  :ensure t
  :after magit
  :config
  (defun rsr/ghub-token-from-secrets (host _username _package &rest _)
    (when (member host '("api.github.com" "github.com"))
      (unless (boundp 'my/github-token)
        (my/load-secrets))
      my/github-token))
  (advice-add 'ghub--token :before-until #'rsr/ghub-token-from-secrets))

;; orgit / orgit-forge — C-c l in a magit or forge buffer stores an org link to
;; the repo, commit, log, file or PR/issue; following it reopens that buffer.
;; Their autoloads register the link types once org and magit are loaded.
(use-package orgit
  :ensure t
  :defer t)

(use-package orgit-forge
  :ensure t
  :defer t
  :config
  ;; orgit-forge only links topics selected by the region.  Also link the one
  ;; at point, and keep the repo's status link from competing with it (org
  ;; would otherwise prompt for which of the two to store).
  (advice-add 'orgit-topic-store :before-until #'rsr/orgit-topic-store-at-point)
  (advice-add 'orgit-status-store :before-while #'rsr/magit-no-topic-at-point))

(defun rsr/magit-topic-at-point ()
  "Return the PR or issue at point in a magit buffer, without a region."
  (when-let* (((derived-mode-p 'magit-mode))
              ((not (region-active-p)))
              (section (magit-current-section))
              ((memq (oref section type) '(issue pullreq))))
    (oref section value)))

(defun rsr/magit-no-topic-at-point ()
  "Return non-nil unless point is on a PR or issue."
  (not (rsr/magit-topic-at-point)))

(defun rsr/orgit-topic-store-at-point ()
  "Store a link to the PR or issue at point, if any."
  (when-let ((topic (rsr/magit-topic-at-point)))
    (orgit-topic-store-1 topic)
    t))

;; diff-hl — show git changes in fringe while editing (like VS Code git gutter)
(use-package diff-hl
  :ensure t
  :hook
  (prog-mode    . diff-hl-mode)
  (org-mode     . diff-hl-mode)
  (dired-mode   . diff-hl-dired-mode)
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; git-messenger — show last commit that touched the current line (on demand)
(use-package git-messenger
  :ensure t
  :defer t
  :custom
  (git-messenger:show-detail t)     ;; include full commit message, not just summary
  (git-messenger:use-magit-popup t)) ;; press 's' in popup to open full magit show
;; C-x v p (git-messenger README), in keymaps.el.

(provide 'magit_config)
;;; magit_config.el ends here
