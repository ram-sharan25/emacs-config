(setq org-html-head-include-default-style nil)
 "#+setupfile: /Users/rrimal/.emacs.d/modules/git-modules/src/comfy_inline/comfy_inline.theme"

(defun bp/org-publish--add-setupfile (&rest args)
  (goto-char (point-min))
  (search-forward "#+title")
  (beginning-of-line)
  (insert "#+setupfile: /Users/rrimal/.emacs.d/modules/git-modules/src/comfy_inline/comfy_inline.theme\n"))

(use-package ox
  :defer t
  :config
  (add-hook 'org-export-before-processing-functions #'bp/org-publish--add-setupfile))
