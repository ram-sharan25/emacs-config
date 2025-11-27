(setq org-html-head-include-default-style nil)

(defun bp/org-publish--add-setupfile (&rest args)
  (goto-char (point-min))
  (search-forward "#+title")
  (beginning-of-line)
  (insert "#+setupfile: /Users/rrimal/.emacs.d/modules/git-modules/src/comfy_inline/comfy_inline.theme\n"))

;; (use-package ox
;;   :defer t
;;   :config
;;   (add-hook 'org-export-before-processing-functions #'bp/org-publish--add-setupfile))


(use-package org
  :config
  (setq org-fontify-quote-and-verse-blocks t) ;; Enable special highlighting for quote blocks
  (setq org-preview-latex-image-directory "/tmp/ltximg/")
  (setq org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :background "Transparent")

  (defun bp/adjust-latex-previews-scale ()
    "Adjust the size of latex preview fragments when changing the
buffer's text scale."
    (pcase major-mode
      ('latex-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'category)
                 'preview-overlay)
             (bp/latex-preview--resize-fragment ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'org-overlay-type)
                 'org-latex-overlay)
             (bp/latex-preview--resize-fragment ov))))))

  (defun bp/latex-preview--resize-fragment (ov)
    (overlay-put
     ov 'display
     (cons 'image
           (plist-put
            (cdr (overlay-get ov 'display))
            :scale (* 2 (/ (frame-char-height) 12) (expt text-scale-mode-step text-scale-mode-amount))))))

  (add-hook 'text-scale-mode-hook #'bp/adjust-latex-previews-scale)
  (defadvice org-latex-preview (after bp/org-latex-preview--adjust-scale activate)
    (bp/adjust-latex-previews-scale)))

;; -------------------------------------------------------------------------
;; Custom Highlighting for :THOUGHTS: Drawers
;; -------------------------------------------------------------------------

(defface my/thought-face
  '((t (:foreground "#DCDCCC" :background "#4a5750" :extend t)))
  "Face for THOUGHTS drawer background.")

(defface my/thought-tag-q-face
  '((t (:foreground "#DFAF8F" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Orange (Curiosity)
  "Face for QUESTION tags.")

(defface my/thought-tag-h-face
  '((t (:foreground "#94BFF3" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Blue (Theory)
  "Face for HYPOTHESIS tags.")

(defface my/thought-tag-a-face
  '((t (:foreground "#F4F4D0" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Yellowish Cream (Fact)
  "Face for ANALYSIS tags.")

(defun my/thought-drawer-extend-region ()
  "Extend region to include the full drawer for background highlighting."
  (save-excursion
    (let ((changed nil))
      (goto-char font-lock-beg)
      (when (re-search-backward "^[ \t]*:THOUGHTS:" nil t)
        (setq font-lock-beg (match-beginning 0))
        (setq changed t))
      (goto-char font-lock-end)
      (when (re-search-forward "^[ \t]*:END:" nil t)
        (setq font-lock-end (match-end 0))
        (setq changed t))
      changed)))

(defun my/match-thought-drawer (limit)
  "Search for :THOUGHTS: drawer content."
  (let ((res nil))
    (while (and (not res) (re-search-forward "^[ \t]*:THOUGHTS:" limit t))
      (let ((start (line-beginning-position 2))
            (end (save-excursion
                   (if (re-search-forward "^[ \t]*:END:" nil t)
                       (match-beginning 0)
                     (point-max)))))
        (when (< start end)
          (put-text-property start end 'font-lock-multiline t)
          (set-match-data (list start end))
          (goto-char end)
          (setq res t))))
    res))

(defun my/activate-thought-highlighting ()
  "Add custom font-lock keywords for THOUGHTS drawers."
  (add-hook 'font-lock-extend-region-functions #'my/thought-drawer-extend-region nil t)

  (font-lock-add-keywords nil
    '((my/match-thought-drawer 0 'my/thought-face t)
      ("\\<\\(QUESTION\\|Q\\):.*$" (0 'my/thought-tag-q-face t))
      ("\\<\\(HYPOTHESIS\\|H\\):.*$" (0 'my/thought-tag-h-face t))
      ("\\<\\(ANALYSIS\\|ANSWER\\|A\\):.*$" (0 'my/thought-tag-a-face t)))
    'append))

(add-hook 'org-mode-hook #'my/activate-thought-highlighting)
(add-hook 'org-mode-hook #'org-bullets-mode)
