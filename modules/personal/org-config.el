;;; org-config.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Code:

(require 'paths)
(require 'org-protocol)
(require 'org-tempo)

;;; Link Abbreviations
;; [[data:image.png]] expands to my/data-dir/image.png
(add-to-list 'org-link-abbrev-alist (cons "data" (concat my/data-dir "%s")))

;; imap-message: links — open in Thunderbird on macOS.
;; macOS doesn't register imap-message:// as a system scheme, so we pass
;; the URL directly to Thunderbird with open -a.
(org-link-set-parameters "imap-message"
  :follow (lambda (path)
            (start-process "org-imap-open" nil "open"
                           "-a" "Thunderbird"
                           (concat "imap-message:" path))))

;;; Org Core Settings

(use-package org
  :config
  ;; Disable the org-element cache.  In org 9.7 the cache desyncs after edits
  ;; (notably refile, which moves subtrees), then the agenda reads a stale
  ;; position and crashes on refresh with "Args out of range" /
  ;; "wrong-type-argument stringp nil".  Files here are small, so the parse
  ;; cost is negligible and disabling the cache eliminates the crashes.
  (setq org-element-use-cache nil)
  (setq org-html-head-include-default-style nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-startup-indented t)          ;; align content under heading text
  (setq org-startup-folded 'content)     ;; show headlines, hide body/drawers
  (setq org-hide-drawer-startup t)       ;; collapse all drawers on open
  (setq org-preview-latex-image-directory "/tmp/ltximg/")
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (plist-put org-format-latex-options :background "Transparent")

  ;; Scale LaTeX preview fragments when text-scale changes
  (defun rsr/adjust-latex-previews-scale ()
    "Resize latex preview overlays to match current text scale."
    (pcase major-mode
      ('latex-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when (eq (overlay-get ov 'category) 'preview-overlay)
           (rsr/latex-preview--resize-fragment ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
           (rsr/latex-preview--resize-fragment ov))))))

  (defun rsr/latex-preview--resize-fragment (ov)
    "Resize a single latex preview overlay OV to match text scale."
    (overlay-put ov 'display
                 (cons 'image
                       (plist-put (cdr (overlay-get ov 'display))
                                  :scale (* 2
                                            (/ (frame-char-height) 12)
                                            (expt text-scale-mode-step
                                                  text-scale-mode-amount))))))

  (add-hook 'text-scale-mode-hook #'rsr/adjust-latex-previews-scale)
  (advice-add 'org-latex-preview :after #'rsr/adjust-latex-previews-scale))

;;; THOUGHTS Drawer Highlighting
;; Visually distinguishes :THOUGHTS: drawers with a subtle background.
;; Tags inside (:Q:, :H:, :A:) colour their content in distinct colours;
;; the markers themselves are shrunk and dimmed, like :THOUGHTS:/:END:.
;; Pure font-lock — only the region being displayed is ever examined.

(defface my/thought-face
  '((t (:background "#4a5750" :extend t)))
  "Background face for :THOUGHTS: drawer content.")

;; The tag faces set no background on purpose: inside a drawer they are
;; prepended onto `my/thought-face', so the drawer's background shows
;; through, and outside a drawer they sit on the normal buffer background.

(defface my/thought-tag-marker-face
  '((t (:inherit shadow :height 0.8 :weight normal)))
  "Face for the `:Q:'/`:H:'/`:A:' markers themselves.
Deliberately small and unemphasised so the tag's content carries the
styling, the way :THOUGHTS: and :END: recede behind the drawer body.")

(defface my/thought-tag-q-face
  '((t (:foreground "#DFAF8F" :weight bold)))
  "Face for the content of a `:Q:' question tag.")

(defface my/thought-tag-h-face
  '((t (:foreground "#94BFF3" :weight bold)))
  "Face for the content of a `:H:' hypothesis tag.")

(defface my/thought-tag-a-face
  '((t (:foreground "#F4F4D0" :weight bold)))
  "Face for the content of an `:A:' analysis/answer tag.")

(defconst my/thought-tag-max-span 2000
  "Maximum characters a delimited thought tag span may cover.
Caps how far the closing marker is searched for, so an unclosed tag
costs a bounded scan instead of one proportional to the rest of the
buffer.")

(defun my/thought-tag-token-p (beg)
  "Return non-nil if the `:Q:'/`:H:'/`:A:' marker at BEG is a thought tag.
Rejects a marker glued to a word, as in `ratio:A:', and one on a headline,
where `:A:' is an org tag rather than a thought marker.

Shared by the font-lock matcher and the auto-close hook: both must agree
on what counts as a tag, or auto-close will insert closing markers after
headline tags that font-lock then declines to colour."
  (and (not (and (> beg (point-min))
                 (memq (char-syntax (char-before beg)) '(?w ?_))))
       (not (save-excursion (goto-char beg)
                            (beginning-of-line)
                            (looking-at-p "\\*+ ")))))

(defun my/thought-tag-match (letter limit)
  "Font-lock matcher for one thought tag, searching forward as far as LIMIT.
LETTER is the tag letter, so LETTER \"A\" matches a `:A: … :A:' span.

Sets group 1 to the opening marker, group 2 to the content between the
markers, and group 3 to the closing marker.  When no closing marker is
found within `my/thought-tag-max-span' the content runs to end of line
and group 3 is absent, so an unclosed tag still colours its own line.

Plain searches rather than one non-greedy regexp: `\\(?:.\\|\n\\)*?'
backtracks per character and overflows the regexp stack on an unclosed
tag in a large buffer."
  (let* ((token (concat ":" letter ":"))
         (width (length token))
         (case-fold-search nil)
         (found nil))
    (while (and (not found) (search-forward token limit t))
      (let ((beg (match-beginning 0)))
        (when (my/thought-tag-token-p beg)
          (let* ((bound (min (point-max) (+ beg my/thought-tag-max-span)))
                 (close-beg (save-excursion (and (search-forward token bound t)
                                                 (match-beginning 0))))
                 (open-end (+ beg width))
                 (close-end (and close-beg (+ close-beg width)))
                 (body-end (or close-beg (line-end-position)))
                 (end (or close-end body-end)))
            (set-match-data
             (list beg end                       ; whole span
                   beg open-end                  ; 1: opening marker
                   open-end body-end             ; 2: content
                   close-beg close-end))         ; 3: closing marker, may be nil
            ;; Always advance, so a zero-width result cannot loop forever.
            (goto-char (max end (1+ beg)))
            (setq found t)))))
    found))

(defun my/thought-tag-match-q (limit)
  "Font-lock matcher for `:Q:' spans, searching as far as LIMIT."
  (my/thought-tag-match "Q" limit))

(defun my/thought-tag-match-h (limit)
  "Font-lock matcher for `:H:' spans, searching as far as LIMIT."
  (my/thought-tag-match "H" limit))

(defun my/thought-tag-match-a (limit)
  "Font-lock matcher for `:A:' spans, searching as far as LIMIT."
  (my/thought-tag-match "A" limit))

(defun my/thought-legacy-match (regexp limit)
  "Font-lock matcher for a legacy `A:'-style tag, searching as far as LIMIT.
REGEXP must set group 1 to the marker and group 2 to the rest of the
line.  Occurrences preceded by `:' are skipped: those are part of a
`:A:' marker, which `my/thought-tag-match' already handles, and matching
them here would paint the same text twice."
  (let ((case-fold-search nil)
        (found nil))
    (while (and (not found) (re-search-forward regexp limit t))
      (let ((beg (match-beginning 1)))
        (unless (and (> beg (point-min)) (eq (char-before beg) ?:))
          (setq found t))))
    found))

(defun my/thought-legacy-match-q (limit)
  "Font-lock matcher for legacy `QUESTION:'/`Q:' lines, up to LIMIT."
  (my/thought-legacy-match "\\<\\(\\(?:QUESTION\\|Q\\):\\)\\(.*\\)$" limit))

(defun my/thought-legacy-match-h (limit)
  "Font-lock matcher for legacy `HYPOTHESIS:'/`H:' lines, up to LIMIT."
  (my/thought-legacy-match "\\<\\(\\(?:HYPOTHESIS\\|H\\):\\)\\(.*\\)$" limit))

(defun my/thought-legacy-match-a (limit)
  "Font-lock matcher for legacy `ANALYSIS:'/`ANSWER:'/`A:' lines, up to LIMIT."
  (my/thought-legacy-match "\\<\\(\\(?:ANALYSIS\\|ANSWER\\|A\\):\\)\\(.*\\)$" limit))

(defconst my/thought-drawer-max-span 20000
  "Maximum characters a :THOUGHTS: drawer body may cover.
Caps the search for the closing :END:.  The cap matters more than the
number: this search runs once per fontified chunk, not once per buffer,
so an unbounded scan over an unclosed drawer costs O(buffer) every time
the display refreshes.")

(defun my/thought-drawer-match (limit)
  "Font-lock matcher for :THOUGHTS: drawer bodies, searching as far as LIMIT.
Sets the match to the text between the :THOUGHTS: line and its :END:.

The closing :END: is searched for only within `my/thought-drawer-max-span'
and only up to the next headline — a drawer cannot span an entry boundary,
so without that stop an unclosed :THOUGHTS: would claim the :END: of some
later :PROPERTIES: or :LOGBOOK: drawer and paint everything in between.
A drawer with no :END: of its own is skipped."
  (let ((case-fold-search nil)
        (found nil))
    (while (and (not found) (re-search-forward "^[ \t]*:THOUGHTS:" limit t))
      (let* ((start (line-beginning-position 2))
             (bound (min (point-max) (+ start my/thought-drawer-max-span)))
             ;; Group 1 matches only on the :END: branch, so a headline hit
             ;; yields nil and the drawer is treated as unclosed.
             (end (save-excursion
                    (and (re-search-forward "^[ \t]*\\(:END:\\)\\|^\\*+ " bound t)
                         (match-beginning 1)
                         (match-beginning 0)))))
        (if (and end (< start end))
            (progn (set-match-data (list start end))
                   (goto-char end)
                   (setq found t))
          (goto-char (line-end-position)))))
    found))

(defun my/thought-extend-region ()
  "Extend the font-lock region back to an enclosing :THOUGHTS: line.

Run from `font-lock-extend-region-functions'.  Font-lock hands a matcher
a region starting at a line boundary, so a drawer whose :THOUGHTS: line
sits above that region never matches and the screenful goes unpainted
until something edits it.  `font-lock-multiline' does not cover this: it
re-expands text that already matched once, which says nothing about the
first paint.  Walking `font-lock-beg' back to the opening line does.

The widened region is handed to every keyword, so this also lets the tag
matchers see an opening `:Q:' that started higher up the same drawer.

Returns non-nil when it moved `font-lock-beg', as the hook requires."
  (let ((start font-lock-beg)
        (case-fold-search nil))
    (save-excursion
      (goto-char font-lock-beg)
      (beginning-of-line)
      ;; Idempotent, and it has to be: font-lock re-runs the extend
      ;; functions in a loop until none of them changes the region.  Once
      ;; `font-lock-beg' sits on the opening line there is nothing left to
      ;; do — without this test the next pass would walk back to an earlier
      ;; unclosed drawer, and the next to the one before that, and the loop
      ;; would not settle.
      (unless (looking-at-p "^[ \t]*:THOUGHTS:")
        (let ((bound (max (point-min) (- (point) my/thought-drawer-max-span))))
          ;; The nearest delimiter above tells us where we are.  Group 1
          ;; matches only on the :THOUGHTS: branch: an :END: or a headline
          ;; found first means the last drawer already closed above us, so
          ;; there is nothing to extend to.  Bounded for the same reason
          ;; `my/thought-drawer-match' is — this runs on every chunk.
          (when (and (re-search-backward
                      "^[ \t]*\\(:THOUGHTS:\\)\\|^[ \t]*:END:\\|^\\*+ " bound t)
                     (match-beginning 1))
            (setq font-lock-beg (match-beginning 0))))))
    (/= start font-lock-beg)))

(defconst my/thought-tag-font-lock-keywords
  '(;; Drawer background first, so the tag faces layer on top of it.
    ;; `append', not `t': these keywords run after org's own, and `t' would
    ;; overwrite them — every link, emphasis and timestamp inside a drawer
    ;; would lose its face.  `append' puts this background behind whatever
    ;; org already applied, which is what the old overlay did.
    (my/thought-drawer-match (0 'my/thought-face append))
    ;; Everything below uses `prepend' rather than `t': the tag face is
    ;; pushed in front of whatever is already there, so its foreground wins
    ;; while the drawer's background underneath survives.
    ;;
    ;; Legacy `A:' tags from notes written before the `:A:' syntax.  Plain
    ;; `.*$' regexps, single line only — kept so old notes stay readable.
    ;; Group 1 is the marker, group 2 the content, same split as the spans.
    (my/thought-legacy-match-q
     (1 'my/thought-tag-marker-face prepend) (2 'my/thought-tag-q-face prepend))
    (my/thought-legacy-match-h
     (1 'my/thought-tag-marker-face prepend) (2 'my/thought-tag-h-face prepend))
    (my/thought-legacy-match-a
     (1 'my/thought-tag-marker-face prepend) (2 'my/thought-tag-a-face prepend))
    ;; Delimited spans last, so they win over the legacy single-line match.
    ;; Group 3 is laxmatched — an unclosed tag has no closing marker.
    (my/thought-tag-match-q
     (1 'my/thought-tag-marker-face prepend) (2 'my/thought-tag-q-face prepend)
     (3 'my/thought-tag-marker-face prepend t))
    (my/thought-tag-match-h
     (1 'my/thought-tag-marker-face prepend) (2 'my/thought-tag-h-face prepend)
     (3 'my/thought-tag-marker-face prepend t))
    (my/thought-tag-match-a
     (1 'my/thought-tag-marker-face prepend) (2 'my/thought-tag-a-face prepend)
     (3 'my/thought-tag-marker-face prepend t)))
  "Font-lock keywords colouring :THOUGHTS: drawers and thought tag spans.")

(defcustom my/thought-tag-auto-close t
  "When non-nil, typing `:Q:', `:H:' or `:A:' inserts its closing marker."
  :type 'boolean
  :group 'org)

(defun my/thought-tag-maybe-auto-close ()
  "Insert the closing marker after a freshly typed `:Q:', `:H:' or `:A:'.
Runs from `post-self-insert-hook'.  Fires only on a `:' typed at the end
of a line, and only for the first tag on that line — a second one is the
user closing the span by hand.  Point stays before the inserted marker,
ready for the tag's text.

Gated on `my/thought-tag-token-p', so a headline tag (`* Task :A:') or a
marker typed against a word (`ratio:A:') is left alone — those are not
thought tags, and font-lock does not colour them either."
  (when (and my/thought-tag-auto-close
             (eq last-command-event ?:)
             (eolp))
    (let ((case-fold-search nil))
      (when (looking-back ":\\([QHA]\\):" (max (point-min) (- (point) 3)))
        (let* ((tag (match-string 1))
               (token-start (match-beginning 0))
               (opened-already
                (save-excursion
                  (goto-char (line-beginning-position))
                  (re-search-forward ":[QHA]:" token-start t))))
          (when (and (not opened-already)
                     (my/thought-tag-token-p token-start))
            (save-excursion (insert " :" tag ":"))))))))

(defun my/activate-thought-highlighting ()
  "Enable :THOUGHTS: drawer and thought tag highlighting via font-lock."
  (add-hook 'post-self-insert-hook #'my/thought-tag-maybe-auto-close nil t)
  ;; Drawer bodies and tag spans cross line boundaries, and font-lock needs
  ;; help with that in both directions.  `font-lock-multiline' marks a match
  ;; so a later edit inside it re-expands to cover the whole thing; the
  ;; extend-region function handles the case it cannot — a region that opens
  ;; above the chunk being fontified, i.e. scrolling into a long drawer.
  ;; Appended, so `font-lock-extend-region-wholelines' runs first.
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions #'my/thought-extend-region t t)
  (font-lock-add-keywords nil my/thought-tag-font-lock-keywords 'append))

(add-hook 'org-mode-hook #'my/activate-thought-highlighting)

;;; Custom Timer
;; Reads :TIMER_MINUTES: and :NOTIFY_BEFORE_MINUTES: from the heading.
;; Auto-starts on clock-in, cancels on clock-out.
;; Plays alarm sound (afplay on macOS) or falls back to beep.

(defcustom my/timer-sound-file "/Users/rrimal/.emacs.d/data/alarm_sound.mp3"
  "Sound file to play for timer notifications. nil uses system beep."
  :type '(choice (const :tag "None" nil) (file :tag "Sound File"))
  :group 'org)

(defvar my/custom-timer-notification-object nil
  "Timer object for the pre-notification warning.")

(defun my/org-play-notification-sound ()
  "Play the notification sound via afplay (macOS) or fall back to beep."
  (let ((sound-file (and my/timer-sound-file (expand-file-name my/timer-sound-file))))
    (if (and sound-file (file-exists-p sound-file))
        (let ((proc (start-process "org-timer-sound" nil "afplay" sound-file)))
          (set-process-sentinel proc
                                (lambda (p e)
                                  (unless (eq 0 (process-exit-status p))
                                    (message "Sound process failed: %s" e)))))
      (beep))))

(defun my/org-start-custom-timer ()
  "Start a timer from :TIMER_MINUTES: with optional :NOTIFY_BEFORE_MINUTES: warning."
  (interactive)
  (when my/custom-timer-notification-object
    (cancel-timer my/custom-timer-notification-object)
    (setq my/custom-timer-notification-object nil))
  (let* ((minutes (string-to-number (or (org-entry-get (point) "TIMER_MINUTES") "0")))
         (notify-mins (string-to-number (or (org-entry-get (point) "NOTIFY_BEFORE_MINUTES") "0"))))
    (if (<= minutes 0)
        (message "No :TIMER_MINUTES: property found or invalid.")
      (org-timer-set-timer (format "%d" minutes))
      (when (and (> notify-mins 0) (< notify-mins minutes))
        (setq my/custom-timer-notification-object
              (run-at-time (format "%d sec" (* (- minutes notify-mins) 60)) nil
                           (lambda (rem)
                             (my/org-play-notification-sound)
                             (message "Timer: %d minutes remaining." rem))
                           notify-mins))))))

(defun my/org-timer-on-clock-in ()
  "Auto-start custom timer on clock-in if :TIMER_MINUTES: is set."
  (unless (bound-and-true-p my/mobile-sync-in-progress)
    (when (org-entry-get (point) "TIMER_MINUTES")
      (my/org-start-custom-timer))))

(defun my/org-cancel-timer-on-clock-out ()
  "Cancel the custom timer and notification on clock-out."
  (ignore-errors (org-timer-stop))
  (when my/custom-timer-notification-object
    (cancel-timer my/custom-timer-notification-object)
    (setq my/custom-timer-notification-object nil)))

(add-hook 'org-clock-in-hook  #'my/org-timer-on-clock-in)
(add-hook 'org-clock-out-hook #'my/org-cancel-timer-on-clock-out)

;;; Keybindings

;; C-c . / C-c ! always open date+time picker — RET inserts current date+time
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c .") (lambda ()
    (interactive)
    (org-time-stamp '(4))))
  (define-key org-mode-map (kbd "C-c !") (lambda ()
    (interactive)
    (org-time-stamp-inactive '(4)))))

(provide 'org-config)
;;; org-config.el ends here
