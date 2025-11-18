;;; project-tasks.el --- A system for capturing tasks linked to projects -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'org-id)

;;; --- 1. File Paths ---

(defvar my/projects-file "~/Stillness/Personal/Writings/Projects.org"
  "The file containing high-level project definitions.")

(defvar my/tasks-file "~/Stillness/Personal/Writings/Tasks.org"
  "The file where all daily, actionable tasks will be captured.")
(defvar my/diary-file "~/Stillness/Personal/Writings/Diary.org"
  "The file where all daily, actionable tasks will be captured.")
(defvar my/google-calendar-file "~/Stillness/Personal/Writings/gcal.org")
;;; --- 2. Packages ---

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode))

;;; --- 3. Helper Functions to Read Project Names ---

(defun my/org-get-project-headings ()
  "Return a list of all level-1 headings from `my/projects-file`."
  (with-current-buffer (find-file-noselect my/projects-file)
    (let ((headings '()))
      (org-map-entries
       (lambda ()
   (push (org-get-heading t t) headings))
       "LEVEL=1" 'file)
      (nreverse headings))))

(defun my/org-select-project-allow-empty ()
  "Prompt user to select a project, allowing empty input."
  (let ((project-list (my/org-get-project-headings)))
    (completing-read "Select Project (or leave empty for Dump): "
         project-list nil t)))

;;; --- 4. TODO Keywords ---

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)"
      "CANCELED(c)"  "REVIEW(r)" "MAYBE(m)"
      "DEFERRED(f)" )))

;;; --- 5. Shared Status Groups ---

(defvar my/org-super-agenda-status-groups
  '((:name "TODO"        :todo "TODO"        :order 1)
    (:name "IN-PROGRESS" :todo "IN-PROGRESS" :order 2)
    (:name "WAITING"     :todo "WAITING"     :order 3)
    (:name "REVIEW"      :todo "REVIEW"      :order 5)
    (:name "MAYBE"       :todo "MAYBE"       :order 6)
    (:name "DEFERRED"    :todo "DEFERRED"    :order 7)
    (:name "DONE"        :todo "DONE"        :order 9)
    (:name "CANCELED"    :todo "CANCELED"    :order 10)
    (:discard (:anything t)))
  "Reusable org-super-agenda groups by TODO keyword.")

;;; --- 6. Capture Template ---

(add-to-list 'org-capture-templates
       '("p" "Project Task" entry
         (file my/tasks-file)
         (function
    (lambda ()
      (let (project-name goal-link-string)
        ;; Check if we are capturing from a project heading
        (if (and (buffer-file-name)
           (equal (expand-file-name (buffer-file-name))
            (expand-file-name my/projects-file))
           (org-at-heading-p))
      ;; Context: from a Project heading
      (progn
        (setq project-name (org-get-heading t t))
        (setq goal-link-string
        (concat "** Goal: " (org-store-link nil) "\n")))
          ;; Context: anywhere else
          (let ((selected (my/org-select-project-allow-empty)))
      (setq project-name (if (string-empty-p selected)
                 "Dump" selected))
      (setq goal-link-string "")))
        ;; Final template string
        (format
"* TODO %%^{Task Title}\n:PROPERTIES:\n:PROJECT: %s\n:ID: %%(org-id-new)\n:CREATED:  %%U\n:END:\n  \n** Description\n- %%? \n\n** Walkthrough\n- [ ] "
         project-name
         goal-link-string))))

         :empty-lines 1))

;;; --- 7. Keybinding ---

(global-set-key (kbd "C-c t") (lambda () (interactive) (org-capture nil "p")))


(defun my/org-agenda-project-suffix ()
  "Format as [Project]:Category, pad to fixed width, and hide 'nil'."
  (let* ((cat (org-get-category))
         (project (org-entry-get nil "PROJECT"))
         (width 25)) ;; Adjust column width here

    (if (or (null cat) (string= (format "%s" cat) "nil"))
        ;; Time Grid lines get empty space
        (make-string width ?\s)

      ;; Build the string
      (let ((output-str
             (if project
                 ;; SWAPPED: [Project] first, then Category
                 (concat (propertize (format "[%s]" project)
                                     'face '(:foreground "orange" :weight bold))
                         ":" cat)
               ;; If no project, just "Category:"
               (format "%s:" cat))))

        ;; Pad result to fixed width
        (format (format "%%-%ds" width) output-str)))))

(setq org-agenda-prefix-format
      '((agenda . " %i %(my/org-agenda-project-suffix) %?-12t% s")
        (todo   . " %i %(my/org-agenda-project-suffix) ")
        (tags   . " %i %(my/org-agenda-project-suffix) ")
        (search . " %i %(my/org-agenda-project-suffix) ")))


(setq org-agenda-custom-commands
      '(("p" "Projects Dashboard" alltodo ""
         (
          ;; 1. Configure Super Agenda to group by the "PROJECT" property
          (org-super-agenda-groups
           '((:auto-property "PROJECT")
             (:auto-todo t)))
          (org-agenda-prefix-format
           '((todo . "  %-12:c %?-12t% s")))
          ))))

(setq org-agenda-span 'day)
(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'project-tasks-config)
