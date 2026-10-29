;;; keymaps.el --- Global key bindings -*- lexical-binding: t -*-

;;; Commentary:
;; Global keys live here, so a new key has one obvious place to go.  Package
;; keys the redesign left unchanged stay with their package (consult C-x b /
;; C-x M-f / M-y, embark C-. / C-;, treemacs C-x t, the S-arrow unsets).
;; Loaded from init.el right after paths.el, before the other modules, so they
;; can bind into these maps.  Mode-map keys stay in their own module, as do
;; keys for commands that exist only once a lazy package has loaded (gptel,
;; org-roam, dogears): those use `use-package :bind' into the maps below.
;;
;;   Cmd (s-)   macOS and VS Code keys, kept on purpose
;;   C-c a/c/l/L  Org agenda / capture / store link (Org manual)
;;   C-c f      format buffer          C-c n  notes (org-roam)
;;   C-c g      Google calendar        C-c o  open
;;   C-c h      hide/show (prog modes) C-c p  projectile (projectile-config.el)
;;   C-c i      AI and agents          C-c r  research / papers
;;   C-c m      multiple cursors       C-c s  sync
;;   C-c w      workspaces (perspective-config.el)
;;   C-c t      time and tools
;;   M-g        goto (built in)        M-s    search (built in)
;;   C-x w      windows (built in)     C-x v  version control (built in)
;;
;; Rules: C-c + letter is ours, one letter per area; no global C-c C-…,
;; C-c + punctuation or digit (those belong to modes); built-in prefixes
;; first, package README keys second; never an Option chord skhd captures.
;; See Plans/emacs-d/keybinding-redesign.

;;; Code:

;;; --- Prefix maps ---

(defvar-keymap rsr/nlm-map
  :doc "NotebookLM commands (C-c i n); filled by notebooklm.el."
  :prefix 'rsr/nlm-map)

(defvar-keymap rsr/ai-map
  :doc "AI and agent commands (C-c i).
Filled by gptel-config.el, terminal-config.el, ai-sessions-config.el and
devlog.el."
  :prefix 'rsr/ai-map
  "n" 'rsr/nlm-map)

(defvar-keymap rsr/capture-map
  :doc "Capture (C-c c)."
  :prefix 'rsr/capture-map
  "c" #'org-capture
  "i" #'my/capture-inbox
  "s" #'my/capture-study
  "q" #'my/capture-project
  "n" #'my/capture-note
  "j" #'my/capture-journal
  "d" #'my/capture-devlog
  "m" #'my/capture-monthly-review
  "w" #'my/capture-weekly-review
  "h" #'my/capture-log-time
  "r" #'my/capture-resource
  "W" #'my/capture-web
  "t" #'my/capture-activity
  "y" #'my/capture-job
  "L" #'my-create-new-leetcode-note)

(defvar-keymap my/gcal-map
  :doc "Google calendar (C-c g)."
  :prefix 'my/gcal-map
  "p" #'my/gcal-push-at-point
  "s" #'my/sync-google-calendar
  "e" #'my/gcal-push-event
  "o" #'my/gcal-open-calendar
  "r" #'my/gcal-reauth)

;; :repeat: after C-c m n, keep tapping n / p.
(defvar-keymap rsr/mc-map
  :doc "Multiple cursors (C-c m)."
  :prefix 'rsr/mc-map
  :repeat (:exit (mc/insert-numbers mc/insert-letters))
  "p" #'mc/mark-previous-like-this
  "n" #'mc/mark-next-like-this
  "0" #'mc/insert-numbers
  "a" #'mc/insert-letters)

(defvar-keymap rsr/notes-map
  :doc "Notes (C-c n); org-roam keys are bound in roam-config.el."
  :prefix 'rsr/notes-map
  "C" #'my/resource-compile-notes
  "x" #'my/notes-rebuild-index)

(defvar-keymap rsr/open-map
  :doc "Open (C-c o)."
  :prefix 'rsr/open-map
  "t" #'rsr/agent-tmux-terminal
  "j" #'my/open-journal-file
  "d" #'my/open-tasks-file
  "h" #'my/open-logbook-file
  "p" #'my/open-projects-file
  "y" #'my/open-job-applications-file
  "n" #'my/open-notes-file
  "a" #'my/open-area-file
  "l" #'my-open-leetcode-folder
  "v" #'my/watch-random-video)

(defvar-keymap rsr/research-map
  :doc "Research and papers (C-c r)."
  :prefix 'rsr/research-map
  "p" #'rsr/open-paper
  "n" #'rsr/edit-paper-note
  "c" #'org-ref-insert-link)

(defvar-keymap rsr/sync-map
  :doc "Sync (C-c s)."
  :prefix 'rsr/sync-map
  "e" #'my/export-mobile-agenda-minimal
  "s" #'my/mobile-sync-from-json)

(defvar-keymap mind-state-map
  :doc "Mind-state tracking (C-c t m)."
  :prefix 'mind-state-map
  "s" #'mind-state-start-tracking
  "q" #'mind-state-stop-tracking
  "l" #'mind-state-manual-log
  "?" #'mind-state-status
  "c" #'mind-state-configure)

(defvar-keymap rsr/time-map
  :doc "Time and tools (C-c t)."
  :prefix 'rsr/time-map
  "i" #'org-clock-in
  "o" #'org-clock-out
  "g" #'org-clock-goto
  "b" #'rsr/org-timeblock-split-view
  "c" #'calc
  "d" #'dictionary-search
  "m" 'mind-state-map)

;;; --- C-c + letter ---

(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" 'rsr/capture-map)
(keymap-global-set "C-c f" #'apheleia-format-buffer)
(keymap-global-set "C-c g" 'my/gcal-map)
(keymap-global-set "C-c i" 'rsr/ai-map)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c L" #'rsr/org-store-link-to-clipboard)
(keymap-global-set "C-c m" 'rsr/mc-map)
(keymap-global-set "C-c n" 'rsr/notes-map)
(keymap-global-set "C-c o" 'rsr/open-map)
(keymap-global-set "C-c r" 'rsr/research-map)
(keymap-global-set "C-c s" 'rsr/sync-map)
(keymap-global-set "C-c t" 'rsr/time-map)

;;; --- Goto (M-g) and search (M-s) ---

;; Jump to the last edit, then tap . / , to keep walking (Vim's g; / g,).
(defvar-keymap rsr/goto-chg-repeat-map
  :repeat t
  "." #'goto-last-change
  "," #'goto-last-change-reverse)

(keymap-global-set "M-g i" #'consult-imenu)
(keymap-global-set "M-g ." #'goto-last-change)
(keymap-global-set "M-g ," #'goto-last-change-reverse)

(keymap-global-set "M-s b" #'rsr/search-brain)
(keymap-global-set "M-s g" #'rgrep)
(keymap-global-set "M-s l" #'consult-line)
(keymap-global-set "M-s r" #'consult-ripgrep)

(keymap-global-set "C-:" #'avy-goto-char-timer)

;;; --- Windows and version control ---

(keymap-global-set "<remap> <other-window>" #'ace-window)
(keymap-global-set "<remap> <split-window-below>" #'rsr/split-horizontal)
(keymap-global-set "<remap> <split-window-right>" #'rsr/split-vertical)
(keymap-global-set "C-x w t" #'rsr/toggle-window-split)
(keymap-global-set "C-x v p" #'git-messenger:popup-message)

;;; --- Cmd (s-): macOS and VS Code ---

;; The NS build defined these in ns-win.el; the emacs-mac port does not, since
;; it assumes Cmd=meta. We remap Cmd to super in early-init.el, so recreate the
;; standard macOS chords here. Cmd-C matters most: M-w is unreachable because
;; the window manager binds Option-w.
(keymap-global-set "s-s" #'save-buffer)
(keymap-global-set "s-c" #'rsr/copy-region-or-line)  ; no region: copy the line (VS Code)
(keymap-global-set "s-v" #'yank)
(keymap-global-set "s-x" #'rsr/kill-region-or-line)  ; no region: cut the line (VS Code)
(keymap-global-set "s-z" #'undo)
(keymap-global-set "s-Z" #'undo-redo)                ; Cmd+Shift+Z
(keymap-global-set "s-a" #'mark-whole-buffer)
(keymap-global-set "s-w" #'delete-window)   ;; Cmd+W closes the split,
;; not the frame. The NS build bound this to `delete-frame'; use Cmd+Shift+W for
;; that, so a stray Cmd+W can never take the whole frame down with it.
(keymap-global-set "s-W" #'delete-frame)
(keymap-global-set "s-u" #'revert-buffer-quick) ;; Cmd+U reloads from disk

;; The rest of the ns-win.el chords worth having on the mac port.  These were
;; free from the NS build and vanished with the switch; `ns-open-file-using-panel'
;; and friends have no mac-port equivalent, so Cmd+O gets plain `find-file'.
(keymap-global-set "s-q" #'save-buffers-kill-emacs) ;; Cmd+Q
(keymap-global-set "s-n" #'make-frame)              ;; Cmd+N  new frame
(keymap-global-set "s-m" #'iconify-frame)           ;; Cmd+M  minimise
(keymap-global-set "s-o" #'find-file)               ;; Cmd+O
(keymap-global-set "s-," #'customize)               ;; Cmd+,  preferences
(keymap-global-set "s-`" #'other-frame)             ;; Cmd+`  cycle frames
(keymap-global-set "s-&" #'kill-current-buffer)     ;; Cmd+&

;; Zoom. One function for all four: `text-scale-adjust' reads the key that
;; invoked it to decide in/out/reset, and stays active for bare = / - repeats.
;; Inert on Linux — `x-super-modifier' is meta there, so nothing emits s-.
;; The portable equivalent is `C-x C-=', which works on every platform.
(keymap-global-set "s-=" #'text-scale-adjust)  ;; Cmd+=       zoom in
(keymap-global-set "s-+" #'text-scale-adjust)  ;; Cmd+Shift+= zoom in
(keymap-global-set "s--" #'text-scale-adjust)  ;; Cmd+-       zoom out
(keymap-global-set "s-0" #'text-scale-adjust)  ;; Cmd+0       reset

(keymap-global-set "s-f" #'consult-line)       ;; Cmd+F       list matching lines
(keymap-global-set "s-F" #'consult-ripgrep)    ;; Cmd+Shift+F project-wide search
(keymap-global-set "s-/" #'rsr/comment-or-uncomment)
;; C-/ stays comment, not undo: M-; is unreachable (skhd owns Option+;).
(keymap-global-set "C-/" #'rsr/comment-or-uncomment)
(keymap-global-set "s-k" #'kill-whole-line)
;; s-l is lsp's prefix (lsp-mode-config.el); s-p is projectile's.

;;; --- Legacy M-m leader (temporary) ---

;; The old M-m keys, kept working while the C-c layout is learned.  Delete this
;; whole block (and the leader in init.el) in the leader cleanup.
(keymap-set rsr/global-prefix-map "a" 'rsr/ai-map)
(keymap-set rsr/global-prefix-map "n" 'rsr/nlm-map)
(keymap-set rsr/global-prefix-map "s" 'rsr/sync-map)
(keymap-set rsr/global-prefix-map "t" 'rsr/time-map)
(keymap-set rsr/global-prefix-map "m" 'rsr/mc-map)
(keymap-set rsr/global-prefix-map "g m" #'git-messenger:popup-message)
(keymap-set rsr/global-prefix-map "r f" #'org-roam-node-find)
(keymap-set rsr/global-prefix-map "r i" #'org-roam-node-insert)
(keymap-set rsr/global-prefix-map "r F" #'org-roam-ref-find)
(keymap-set rsr/global-prefix-map "r c" #'org-roam-capture)
(keymap-set rsr/global-prefix-map "r g" #'org-roam-graph)
(keymap-set rsr/global-prefix-map "r r" #'org-roam-buffer-toggle)
(keymap-set rsr/global-prefix-map "r e" #'org-roam-ref-add)
(keymap-set rsr/global-prefix-map "r a" #'org-roam-alias-add)
(keymap-set rsr/global-prefix-map "r t" #'org-roam-tag-add)
(keymap-set rsr/global-prefix-map "r s" #'rsr/search-brain)
(keymap-set rsr/global-prefix-map "r p" #'rsr/open-paper)
(keymap-set rsr/global-prefix-map "r n" #'rsr/edit-paper-note)
(keymap-set rsr/global-prefix-map "r C" #'my/resource-compile-notes)

;;; --- which-key labels ---

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c c" "capture"     "C-c g" "gcal"        "C-c i" "AI"
    "C-c i n" "NotebookLM" "C-c m" "multi-cursor" "C-c n" "notes"
    "C-c o" "open"        "C-c r" "research"    "C-c s" "sync"
    "C-c t" "time/tools"  "C-c t m" "mind-state" "C-c w" "workspaces"
    "M-m" "legacy leader" "M-m a" "AI" "M-m n" "NotebookLM" "M-m s" "sync"
    "M-m t" "time/tools" "M-m m" "multi-cursor" "M-m r" "legacy roam"
    "M-m g" "legacy git"))

(provide 'keymaps)
;;; keymaps.el ends here
