;;; brain-roam-ids.el --- roam node filter + org-id duplicate guard -*- lexical-binding: t; -*-

;;; Commentary:
;; Prototype for D16 (brain-roam-id-links).  Self-contained so it can be
;; tested under `emacs -Q`; the config patch splices these forms into
;; roam-config.el.  Requires only `my/brain-dir' (paths.el) or the fallback
;; `org-roam-directory'.
;;
;; Three parts:
;;  1. Roam node policy: which headings become nodes (R2, R3, R4).
;;  2. org-id coverage of the whole repo, independent of roam (R6).
;;  3. Duplicate-ID guard on paste/yank/refile-copy + finder/fixer (R5).

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)

;;;; 1. Roam node policy ---------------------------------------------------

(defvar my/org-roam-file-only-dirs '("Areas/")
  "Dirs (relative to `org-roam-directory') where only the FILE is a node.
Project files and area hubs live here: their headings (tasks, Log, Notes,
Dashboard scaffolding) carry IDs for linking but must not be roam nodes.")

(defun my/org-roam--file-only-p ()
  "Non-nil if the current buffer's file is under `my/org-roam-file-only-dirs'."
  (when-let* ((file (buffer-file-name (buffer-base-buffer))))
    (let ((rel (file-relative-name file org-roam-directory)))
      (cl-some (lambda (d) (string-prefix-p d rel)) my/org-roam-file-only-dirs))))

(defun my/org-roam--task-p ()
  "Non-nil if heading at point, or any ancestor, has a TODO keyword."
  (save-excursion
    (catch 'task
      (while t
        (when (org-get-todo-state) (throw 'task t))
        (unless (org-up-heading-safe) (throw 'task nil))))))

(defun my/org-roam-node-include-p ()
  "Value for `org-roam-db-node-include-function'.
Called by org-roam with point on a heading that has an ID (or at
point-min for the file node).  File-level nodes always pass.  Under
`my/org-roam-file-only-dirs' no heading passes.  Elsewhere a heading passes
unless it or an ancestor is a task (TODO keyword)."
  (cond ((= (org-outline-level) 0) t)
        ((my/org-roam--file-only-p) nil)
        (t (not (my/org-roam--task-p)))))

(defvar my/org-roam-exclude-regexps
  '("^gtd/" "^Archives/" "^Trackers/" "^Private/" "^data/" "^output/"
    "^cleanup_2026_02_27/" "^phone_inbox/" "\\.bak\\'" "~\\'")
  "Value for `org-roam-file-exclude-regexp' (target layout).")

(defun my/org-roam-files-without-file-id ()
  "List roam files that have no file-level node (no :ID: drawer at the top).
Links written inside their tasks (or other excluded headings) are
dropped by roam, because there is no enclosing node to credit.  Run after
`org-roam-db-sync'; give each listed file a file-level ID."
  (interactive)
  (let (missing)
    (dolist (f (org-roam-list-files))
      (unless (org-roam-db-query [:select id :from nodes :where (and (= file $s1) (= level 0))] f)
        (push (file-relative-name f org-roam-directory) missing)))
    (setq missing (sort missing #'string<))
    (if (called-interactively-p 'any)
        (with-current-buffer (get-buffer-create "*roam files without file ID*")
          (erase-buffer) (dolist (f missing) (insert f "\n")) (pop-to-buffer (current-buffer)))
      missing)))

;;;; 2. org-id coverage of the whole repo -----------------------------------

(defvar my/org-id-scan-root nil
  "Root scanned for IDs; defaults to `org-roam-directory'.")

(defvar my/org-id-scan-exclude '("/\\.git/" "/output/" "/data/" "\\.bak\\'" "~\\'" "/#")
  "Regexps of paths never scanned for IDs.")

(defun my/org-id-files ()
  "Every .org file under the brain, minus `my/org-id-scan-exclude'.
`.org.gpg' files are not matched, so Private/ needs no passphrase."
  (let ((root (or my/org-id-scan-root org-roam-directory)))
    (cl-remove-if (lambda (f) (cl-some (lambda (re) (string-match-p re f))
                                        my/org-id-scan-exclude))
                  (directory-files-recursively root "\\.org\\'"))))

(defvar my/org-id-extra-files nil
  "Refreshed from `my/org-id-files' before every org-id rescan.")

(defun my/org-id--refresh-extra-files (&rest _)
  (setq my/org-id-extra-files (my/org-id-files)))

;; org-id rescans (also the automatic one `org-id-find' runs on a miss) then
;; cover the whole repo, not only agenda files.
(setq org-id-extra-files 'my/org-id-extra-files)
(advice-add 'org-id-update-id-locations :before #'my/org-id--refresh-extra-files)

(defun my/org-id-rescan ()
  "Rebuild `org-id-locations' from every .org file in the brain."
  (interactive)
  (org-id-update-id-locations nil t)
  (message "org-id: %d IDs in %d files"
           (hash-table-count org-id-locations) (length org-id-files)))

;;;; 3a. Duplicate-ID guard on paste ---------------------------------------

(defvar my/org-id-paste-mode nil
  "Dynamically bound to `move' (never regenerate IDs) or `copy' (always).
Nil means: regenerate an ID only if the original entry still exists
somewhere else, i.e. the subtree was copied rather than cut.")

(defconst my/org-id--id-line-re "^[ \t]*:ID:[ \t]+\\([^ \t\n\r]+\\)")

(defun my/org-id--entry-exists-elsewhere-p (id &optional beg end)
  "Non-nil if an entry with ID exists outside region BEG..END of this buffer,
or in the file `org-id-locations' records for it."
  (or (org-with-wide-buffer
       (goto-char (point-min))
       (catch 'found
         (while (re-search-forward (concat "^[ \t]*:ID:[ \t]+" (regexp-quote id) "[ \t]*$") nil t)
           (unless (and beg end (<= beg (point)) (<= (point) end))
             (throw 'found t)))))
      (when-let* ((file (org-id-find-id-file id)))
        (and (not (equal (file-truename file)
                         (and buffer-file-name (file-truename buffer-file-name))))
             (file-exists-p file)
             (with-current-buffer (find-file-noselect file t)
               (org-with-wide-buffer (org-find-entry-with-id id)))))))

(defun my/org-id--dedupe-string (txt exists-p)
  "Return TXT with every :ID: for which (EXISTS-P id) is true replaced by a
fresh ID.  id: links inside TXT that point at a replaced ID are rewritten
too, so a copied project keeps its internal links on the copy."
  (let ((map nil) (s 0))
    (while (string-match my/org-id--id-line-re txt s)
      (setq s (match-end 0))
      (let ((id (match-string 1 txt)))
        (when (and (not (assoc id map))
                   (or (eq my/org-id-paste-mode 'copy)
                       (and (not (eq my/org-id-paste-mode 'move))
                            (funcall exists-p id))))
          (push (cons id (org-id-new)) map))))
    (if (null map) txt
      (let ((out txt))
        (dolist (pair map)
          (setq out (replace-regexp-in-string
                     (concat "\\(^[ \t]*:ID:[ \t]+\\)" (regexp-quote (car pair)) "\\([ \t]*$\\)")
                     (concat "\\1" (cdr pair) "\\2") out t))
          (setq out (replace-regexp-in-string
                     (concat "\\[\\[id:[ \t]*" (regexp-quote (car pair)) "\\]")
                     (concat "[[id:" (cdr pair) "]") out t t)))
        (message "org-id: %d duplicate ID%s regenerated in pasted subtree"
                 (length map) (if (= 1 (length map)) "" "s"))
        out))))

(defun my/org-id--paste-subtree-filter (args)
  "`:filter-args' for `org-paste-subtree' (LEVEL TREE FOR-YANK REMOVE).
Dedupes the text before it is inserted, so `org-id-paste-tracker' then
registers the new IDs."
  (unless (eq my/org-id-paste-mode 'move)
    (let* ((tree (or (nth 1 args) (ignore-errors (current-kill 0))))
           (new (and tree (my/org-id--dedupe-string
                           tree #'my/org-id--entry-exists-elsewhere-p))))
      (when (and new (not (eq new tree)))
        (setq args (list (nth 0 args) new (nth 2 args) (nth 3 args))))))
  args)

(defun my/org-id--dedupe-region (beg end)
  "Dedupe IDs in BEG..END of the current buffer (used after plain yank)."
  (let* ((txt (buffer-substring-no-properties beg end))
         (new (my/org-id--dedupe-string
               txt (lambda (id) (my/org-id--entry-exists-elsewhere-p id beg end)))))
    (unless (eq new txt)
      (save-excursion
        (goto-char beg) (delete-region beg end) (insert new))
      (when (fboundp 'org-id-paste-tracker) (org-id-paste-tracker new)))))

(defun my/org-id--yank-around (orig &rest args)
  "`:around' for `org-yank'.  When the kill is a subtree and org pastes it
with plain `yank' (`org-yank-adjusted-subtrees' nil), dedupe the region."
  (if (or (eq my/org-id-paste-mode 'move)
          (not (derived-mode-p 'org-mode))
          (not (org-kill-is-subtree-p)))
      (apply orig args)
    (let ((beg (point-marker)))
      (apply orig args)
      (let ((end (copy-marker (max (point) (or (mark t) (point))))))
        (my/org-id--dedupe-region (min beg end) (max beg end))
        (set-marker beg nil) (set-marker end nil)))))

(defun my/org-id--refile-around (orig &optional arg &rest args)
  "`:around' for `org-refile': a plain refile is a move; keep IDs."
  (let ((my/org-id-paste-mode
         (if (or (bound-and-true-p org-refile-keep) (equal arg 3)) 'copy 'move)))
    (apply orig arg args)))

(defun my/org-id--move-around (orig &rest args)
  "`:around' for functions that cut-and-paste (archive): keep IDs."
  (let ((my/org-id-paste-mode 'move))
    (apply orig args)))

(advice-add 'org-paste-subtree :filter-args #'my/org-id--paste-subtree-filter)
(advice-add 'org-yank :around #'my/org-id--yank-around)
(advice-add 'org-refile :around #'my/org-id--refile-around)
(dolist (f '(org-archive-subtree org-archive-to-archive-sibling))
  (advice-add f :around #'my/org-id--move-around))

;;;; 3b. Duplicate finder / fixer ------------------------------------------

(defun my/org-id--collect-locations ()
  "Alist (ID . ((FILE . LINE) ...)) for every :ID: property in the brain."
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (dolist (file (my/org-id-files))
        (erase-buffer)
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward my/org-id--id-line-re nil t)
          (push (cons file (line-number-at-pos))
                (gethash (match-string 1) table)))))
    (let (out)
      (maphash (lambda (id locs) (push (cons id (nreverse locs)) out)) table)
      out)))

(defun my/org-id-find-duplicates ()
  "List every ID that occurs more than once across the brain.
Opens a buffer with one section per ID; each location is a link.
In that buffer, RET follows a link; put point in the entry that must
get a NEW id and run `my/org-id-regenerate-here'."
  (interactive)
  (let* ((dups (cl-remove-if-not (lambda (e) (> (length (cdr e)) 1))
                                 (my/org-id--collect-locations)))
         (buf (get-buffer-create "*org-id duplicates*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: %d duplicate ID%s\n\n" (length dups)
                        (if (= 1 (length dups)) "" "s")))
        (dolist (e (cl-sort dups #'string< :key (lambda (x) (caadr x))))
          (insert (format "* %s\n" (car e)))
          (dolist (loc (cdr e))
            (insert (format "- [[file:%s::%d][%s:%d]]\n"
                            (car loc) (cdr loc)
                            (file-relative-name (car loc) (or my/org-id-scan-root org-roam-directory))
                            (cdr loc)))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)
    (message "%d duplicate ID%s" (length dups) (if (= 1 (length dups)) "" "s"))
    dups))

(defun my/org-id-regenerate-here ()
  "Give the entry at point a fresh ID and register it.
Links elsewhere keep pointing at the OTHER copy, which keeps the old ID."
  (interactive)
  (let ((old (org-entry-get nil "ID")))
    (unless old (user-error "No :ID: on this entry"))
    (org-entry-delete nil "ID")
    (let ((new (org-id-get-create)))
      (message "org-id: %s -> %s" old new)
      new)))

(provide 'brain-roam-ids)
;;; brain-roam-ids.el ends here
