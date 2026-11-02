;;; mermaid.el --- Mermaid configuration -*- lexical-binding: t; -*-

(require 'paths)
(use-package ob-mermaid
  :ensure t
  :demand t)

;; Mermaid CLI from PATH (Homebrew's on this Mac).
(setq ob-mermaid-cli-path (or (executable-find "mmdc") "mmdc"))

;; Default to dark theme with transparent background for dark Emacs themes
(setf (alist-get :theme org-babel-default-header-args:mermaid) "dark")
(setf (alist-get :background-color org-babel-default-header-args:mermaid) "transparent")

(defun my/org-babel-mermaid-default-dir (orig-fun &rest args)
  "Advice to force Mermaid output to `my/data-dir` unless absolute path is given."
  (let ((default-directory my/data-dir))
    (apply orig-fun args)))

(advice-add 'org-babel-execute:mermaid :around #'my/org-babel-mermaid-default-dir)

;; -------------------------------------------------------------------------
;; Auto mermaid → LaTeX figure via org-native attrs (global, all files)
;;
;; On C-c C-c of any mermaid block:
;;   1. Rewrites [[file:X]] → [[data:X]] for Emacs inline display.
;;   2. Keeps #+CAPTION / #+NAME / #+ATTR_LATEX before #+RESULTS.
;;   3. Replaces the previous image result instead of appending another one.
;; -------------------------------------------------------------------------
(defun my/mermaid-latex-attrs ()
  "Add an idempotent Org figure result after executing a Mermaid block.
Keep figure metadata before `#+RESULTS' so Org can replace the image link on
later executions instead of appending duplicate output."
  (let* ((info (ignore-errors
                 (save-excursion
                   (org-babel-goto-src-block-head)
                   (org-babel-get-src-block-info 'light))))
         (lang (and info (car info)))
         (file (and info (cdr (assq :file (nth 2 info))))))
    (when (and (equal lang "mermaid") file)
      (let* ((fig-name (file-name-nondirectory file))
             (base (file-name-sans-extension fig-name))
             (label (replace-regexp-in-string "_" "-" base))
             (caption (capitalize (replace-regexp-in-string "_" " " base)))
             (caption-line (format "#+CAPTION: %s" caption))
             (name-line (format "#+NAME: fig:%s" label))
             (attr-line "#+ATTR_LATEX: :width 0.8\\textwidth :height 1.0\\textheight :options keepaspectratio :float t")
             (generated-line-re
              (concat
               "^[ \t]*\\(?:"
               "#\\+RESULTS\\(?:[^\n]*\\)"
               "\\|" (regexp-quote caption-line)
               "\\|" (regexp-quote name-line)
               "\\|" (regexp-quote attr-line)
               "\\|\\[\\[\\(?:file\\|data\\):[^]]+\\]\\]"
               "\\)[ \t]*$")))
        (save-excursion
          ;; Ensure the generated image is exported.
          (org-babel-goto-src-block-head)
          (when (re-search-forward ":exports +none" (line-end-position) t)
            (replace-match ":exports results"))

          (org-babel-goto-src-block-head)
          (when-let ((rp (org-babel-where-is-src-block-result)))
            ;; Include metadata left immediately before #+RESULTS by a previous
            ;; execution, then consume all old generated result lines after it.
            (goto-char rp)
            (beginning-of-line)
            (let ((start (point)))
              (while (and (= (forward-line -1) 0)
                          (looking-at-p generated-line-re))
                (setq start (point)))
              (goto-char rp)
              (beginning-of-line)
              (while (and (not (eobp))
                          (looking-at-p generated-line-re))
                (forward-line 1))
              (delete-region start (point))
              (goto-char start)
              (insert (format "%s\n%s\n%s\n#+RESULTS:\n[[data:%s]]\n"
                              caption-line name-line attr-line fig-name)))))))))

(add-hook 'org-babel-after-execute-hook #'my/mermaid-latex-attrs)

(provide 'mermaid)
;;; mermaid.el ends here
