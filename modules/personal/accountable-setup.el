;;; accountable-setup.el --- Daily tracking and accountability system -*- lexical-binding: t -*-
;;; Commentary:
; Provides a daily tracking system for todos, journal entries, and monkey mind tracking
; with easy capture templates and navigation.
;;; Code:
(require 'org)
(require 'org-capture)
;; Set up base directory and file
(setq personal-dir "~/Stillness/Personal/Writings/")
 ;;"Base directory for personal writings."
(setq daily-file (concat personal-dir "daily.org"))
 ;;"File for daily entries and tracking."
;; Function to ensure daily heading exists
(defun date-tree-exists-p ()
 "Check if today's date tree already exists."
 (let ((year (format-time-string "%Y"))
       (month (format-time-string "%Y-%m %B"))
       (day (format-time-string "%Y-%m-%d %A")))
   (save-excursion
     (goto-char (point-min))
     (and (re-search-forward (format "^\\* %s$" year) nil t)
	  (re-search-forward (format "^\\** %s$" month) nil t)
	  (re-search-forward (format "^\\*** %s$" day) nil t)))))
(defun ensure-date-tree ()
 "Create or find today's date tree structure."
 (unless (date-tree-exists-p)  ; Only create if it doesn't exist
   (let ((year (format-time-string "%Y"))
	 (month (format-time-string "%Y-%m %B"))
	 (day (format-time-string "%Y-%m-%d %A")))
     (goto-char (point-min))
     (unless (re-search-forward (format "^\\* %s$" year) nil t)
       (goto-char (point-max))
       (insert (format "\n* %s" year)))
     (goto-char (point-min))
     (re-search-forward (format "^\\* %s$" year))
     (unless (re-search-forward (format "^\\** %s$" month) nil t)
       (org-end-of-subtree)
       (insert (format "\n** %s" month)))
     (goto-char (point-min))
     (re-search-forward (format "^\\* %s$" year))
     (re-search-forward (format "^\\** %s$" month))
     (unless (re-search-forward (format "^\\*** %s$" day) nil t)
       (org-end-of-subtree)
       (insert (format "\n*** %s\n**** TODO Items\n**** Journal\n**** Review" day))))))
;; Function to ensure we're in the correct section
(defun ensure-correct-section (section)
 "Ensure we're in the correct SECTION of today's entry."
 (let ((year (format-time-string "%Y"))
       (month (format-time-string "%Y-%m %B"))
       (day (format-time-string "%Y-%m-%d %A")))
   (goto-char (point-min))
   (re-search-forward (format "^\\* %s$" year))
   (re-search-forward (format "^\\** %s$" month))
   (re-search-forward (format "^\\*** %s$" day))
   (re-search-forward (format "^\\**** %s$" section))))
;; Update capture templates
(setq org-capture-templates
     `(
       ("t" "Todo" entry
        (file+function daily-file
                      (lambda ()
                        (ensure-date-tree)
                        (ensure-correct-section "TODO Items")))
        "***** %<%I:%M %p> [ ] %? \n [[%F][source]] "
        :empty-lines 1
        :prepend t)
        ("i" "Ideas" entry
        (file+function daily-file
                      (lambda ()
                        (ensure-date-tree)
                        (ensure-correct-section "Journal")))
        "***** %<%I:%M %p> [ ] :IDEA: %? \n [[%F][source]] "
        :empty-lines 1)
        ("j" "Journal" entry
        (file+function daily-file
                      (lambda ()
                        (ensure-date-tree)
                        (ensure-correct-section "Journal")))
        "***** %<%I:%M %p> : \n - %? \n [[%F][source]]"
        :empty-lines 1)
        ("m" "Monkey Mind" entry
        (file+function daily-file
                      (lambda ()
                        (ensure-date-tree)
                        (ensure-correct-section "Journal")))
        "***** %<%I:%M %p> Mind Map 🐒 :\n - %? \n [[%F][source]] "
        :empty-lines 1)
        ("r" "Review" entry
        (file+function daily-file
                      (lambda ()
                        (ensure-date-tree)
                        (ensure-correct-section "Review")))
        "***** %<%I:%M %p> Day Review: \n - %? \n [[%F][source]]\n "
        :empty-lines 1)))

(defun ensure-capture-location ()
 "Ensure we're at the right location in the tree before capturing."
 (save-excursion
   (let ((year (format-time-string "%Y"))
	 (month (format-time-string "%Y-%m %B"))
	 (day (format-time-string "%Y-%m-%d %A")))
     (goto-char (point-min))
     (re-search-forward (format "^\\* %s$" year))
     (re-search-forward (format "^\\** %s$" month))
     (re-search-forward (format "^\\*** %s$" day)))))
;; Ensure daily heading exists before capture
(advice-add 'org-capture :before
	   (lambda (&rest _)
	     (with-current-buffer (find-file-noselect daily-file)
	       (ensure-date-tree))))
;; Capture functions
(defun my/capture-todo ()
 "Capture a new TODO item."
 (interactive)
 (org-capture nil "t"))
(defun my/capture-journal ()
 "Capture a journal entry."
 (interactive)
 (org-capture nil "j"))
(defun my/capture-monkey ()
 "Capture a monkey mind moment."
 (interactive)
 (org-capture nil "m"))
(defun my/capture-review ()
 "Capture a review entry."
 (interactive)
 (org-capture nil "r"))
(defun my/capture-idea ()
 "Capture a review entry."
 (interactive)
 (org-capture nil "i"))
;; Global keybindings
(global-set-key (kbd "C-c t") 'my/capture-todo)
(global-set-key (kbd "C-c j") 'my/capture-journal)
(global-set-key (kbd "C-c i") 'my/capture-idea)
(global-set-key (kbd "C-c m") 'my/capture-monkey)
(global-set-key (kbd "C-c r") 'my/capture-review)
;; Function to open daily file and jump to today
(defun open-daily-file ()
 "Open daily file and jump to today's entry."
 (interactive)
 (find-file daily-file)
 (unless (file-exists-p daily-file)
   (write-region "" nil daily-file))
 (ensure-date-tree)
 (let ((year (format-time-string "%Y"))
       (month (format-time-string "%Y-%m %B"))
       (day (format-time-string "%Y-%m-%d %A")))
   (goto-char (point-min))
   (re-search-forward (format "^\\* %s$" year))
   (re-search-forward (format "^\\** %s$" month))
   (re-search-forward (format "^\\*** %s$" day))))
(global-set-key (kbd "C-c C-o d") 'open-daily-file)

(provide 'accountable-setup)
;; accountable-setup.el ends here
