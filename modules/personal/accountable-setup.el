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
(defun ensure-daily-heading ()
 "Create or find today's heading with all required sections."
 (let ((today (format-time-string "%Y-%m-%d %A")))
   (with-current-buffer (find-file-noselect daily-file)
     (unless (file-exists-p daily-file)
       (write-region "" nil daily-file))
     (goto-char (point-min))
     (unless (re-search-forward (format "^\\* %s" today) nil t)
       (goto-char (point-max))
       (when (> (point) 1) (insert "\n"))
       (insert (format "* %s\n" today))
       (insert "** TODO Items\n")
       (insert "** Journal\n")
       (insert "** Review\n")
       (save-buffer))
     today)))
;; Set up capture templates
(setq org-capture-templates
     `(
       ("t" "Todo" entry
	(file+function daily-file
		      (lambda ()
			(ensure-daily-heading)
			(re-search-forward "^\\** TODO Items" nil t)))
	"* %<%I:%M %p> [ ] %? \n [[%F][source]] "
	:empty-lines 1
	:prepend t)
	("i" "Ideas" entry
	(file+function daily-file
		      (lambda ()
			(ensure-daily-heading)
			(re-search-forward "^\\** Journal" nil t)))
	"*  %<%I:%M %p> [ ] :IDEA: %? \n [[%F][source]] "
	:empty-lines 1)
	("j" "Journal" entry
	(file+function daily-file
		      (lambda ()
			(ensure-daily-heading)
			(re-search-forward "^\\** Journal" nil t)))
	"* %<%I:%M %p> :  \n - %? \n [[%F][source]]"
	:empty-lines 1
	tree-type day)
	("m" "Monkey Mind" entry
	(file+function daily-file
		      (lambda ()
			(ensure-daily-heading)
			(re-search-forward "^\\** Journal" nil t)))
	"* %<%I:%M %p> Mind Map 🐒 :\n - %?  \n [[%F][source]] "
	:empty-lines 1)
	("r" "Review" entry
	(file+function daily-file
		      (lambda ()
			(ensure-daily-heading)
			(re-search-forward "^\\** Review" nil t)))
	"* %<%I:%M %p> Day Review:  \n - %? \n [[%F][source]]\n "
	:empty-lines 1)))
;; Ensure daily heading exists before capture
(advice-add 'org-capture :before
	  (lambda (&rest _)
	    (ensure-daily-heading)))
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
 (find-file "~/Stillness/Personal/Writings/daily.org")
 (let ((today (format-time-string "%Y-%m-%d")))
   (goto-char (point-min))
   (if (re-search-forward (format "^\\* %s" today) nil t)
       (beginning-of-line)
     (ensure-daily-heading)
     (goto-char (point-min))
     (re-search-forward (format "^\\* %s" today) nil t)
     (beginning-of-line))))
(global-set-key (kbd "C-c C-o d") 'open-daily-file)

(provide 'accountable-setup)
;; accountable-setup.el ends here
