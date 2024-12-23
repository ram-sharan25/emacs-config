;; Directory setup
(require 'org)
(setq org-capture-templates nil)  ; Clear existing templates

(setq personal-dir "~/Stillness/Personal/Writings/")
(setq tasks-dir (concat personal-dir "tasks"))
(setq journal-dir (concat personal-dir "journal"))
(setq notes-dir (concat personal-dir "notes"))
(setq templates-dir (concat personal-dir "templates"))
(setq monkey-dir "~/Stillness/Personal/Writings/monkey/")
(setq monkey-data-dir (concat monkey-dir "/data"))


;; Create all necessary directories
(dolist (dir (list personal-dir tasks-dir journal-dir notes-dir templates-dir monkey-dir monkey-data-dir))
  (unless (file-exists-p dir)
    (make-directory dir t)))

;; Create initial org files if they don't exist
(dolist (file '(("tasks.org" . "* Tasks\n")
		("quick-notes.org" . "* Quick Notes\n")
		("journal.org" . "* Journal\n")))
  (let ((filepath (concat (cond
			  ((string= (car file) "tasks.org") tasks-dir)
			  ((string= (car file) "quick-notes.org") notes-dir)
			  ((string= (car file) "journal.org") journal-dir))
			 "/" (car file))))
    (unless (file-exists-p filepath)
      (with-temp-file filepath
	(insert (cdr file))))))

(defun monkey-get-file ()
  (expand-file-name
   (format-time-string "%Y-%m-%d-monkey.org")
   monkey-data-dir))


;; Set up capture templates BEFORE defining keybindings
(setq org-capture-templates
      `(;; Tasks, Notes, and Journal templates
	("t" "Task" entry
	 (file+headline ,(concat tasks-dir "/tasks.org") "Tasks")
	 "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%a"
	 :empty-lines 1)

	("n" "Quick Note" entry
	 (file+olp+datetree ,(concat notes-dir "/quick-notes.org"))
	 "* %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%a"
	 :empty-lines 1
	 :tree-type week)

	("j" "Journal" entry
	 (file+olp+datetree ,(concat journal-dir "/journal.org"))
	 "* %<%I:%M %p> %?\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%a"
	 :empty-lines 1
	 :tree-type day)

	("J" "Journal with Template" entry
	 (file+olp+datetree ,(concat journal-dir "/journal.org"))
	 (file ,(concat templates-dir "/journal-template.org"))
	 :empty-lines 1
	 :tree-type day)

	;; Monkey Mind template
	("m" "Monkey Mind" entry
	 (file+headline ,(monkey-get-file) "Distractions")
	 (file ,(concat monkey-dir "/templates/monkey-template.org"))
	 :empty-lines 1)))
;; Capture functions
(defun my/capture-task ()
  (interactive)
  (org-capture nil "t"))

(defun my/capture-note ()
  (interactive)
  (org-capture nil "n"))

(defun my/capture-journal ()
  (interactive)
  (org-capture nil "j"))

;; Global keybindings for captures
(global-set-key (kbd "C-c t") 'my/capture-task)   ; Ctrl-c t for tasks
(global-set-key (kbd "C-c n") 'my/capture-note)   ; Ctrl-c n for notes
(global-set-key (kbd "C-c j") 'my/capture-journal) ; Ctrl-c j for journal

;; Create journal template if it doesn't exist
(let ((template-file (concat templates-dir "/journal-template.org")))
  (unless (file-exists-p template-file)
    (with-temp-file template-file
      (insert "* %<%I:%M %p> %^{Title}\n:PROPERTIES:\n:Created: %U\n:END:\n** How am I feeling?\n%?\n** What am I grateful for?\n\n** What are my intentions for today?\n"))))


;; Define monkey mode as a minor mode
(define-minor-mode monkey-mode
  "Minor mode for tracking distractions and monkey mind moments."
  :init-value nil
  :lighter " 🐒"
  :global t)

;; Predefined distraction titles with their types
(setq monkey-distractions
      '(("Instagram" . "social-media")
	("WhatsApp" . "social-media")
	("Twitter" . "social-media")
	("Phone Call" . "phone")
	("Text Message" . "phone")
	("Office Noise" . "noise")
	("Music" . "noise")
	("Chat with Colleague" . "conversation")
	("Random Browsing" . "web-surfing")
	("YouTube" . "web-surfing")
	("Overthinking" . "thoughts")
	("Daydreaming" . "thoughts")
	("Coffee Break" . "other")))


;; Function to create monkey file if it doesn't exist
(defun monkey-create-file ()
  (let ((file-path (monkey-get-file)))
    (unless (file-exists-p file-path)
      (with-temp-file file-path
	(insert (format-time-string "#+TITLE: Monkey Mind - %Y-%m-%d\n"))
	(insert "#+FILETAGS: :monkey:\n\n")
	(insert "* Distractions\n")))))

;; Function to open today's monkey log
(defun monkey-open-log ()
  (interactive)
  (monkey-create-file)
  (find-file (monkey-get-file)))

;; Function to select distraction with completion
(defun monkey-select-distraction ()
  (let* ((choices (mapcar 'car monkey-distractions))
	 (choice (completing-read "What distracted you? " choices nil t))
	 (type (cdr (assoc choice monkey-distractions))))
    (cons choice type)))

;; Store the selected distraction globally
(defvar monkey-current-distraction nil
  "Store the currently selected distraction.")

;; Modified function to store selected distraction
(defun monkey-capture-distraction ()
  (setq monkey-current-distraction (monkey-select-distraction))
  (car monkey-current-distraction))

;; Function to get stored distraction type
(defun monkey-get-distraction-type ()
  (cdr monkey-current-distraction))

;; Create templates directory if it doesn't exist
(dolist (template-dir (list (concat templates-dir)
			   (concat monkey-dir "/templates")))
  (unless (file-exists-p template-dir)
    (make-directory template-dir t)))

;; Create the template file with modified format
(let ((template-dir (concat monkey-dir "/templates")))
  (unless (file-exists-p template-dir)
    (make-directory template-dir t))
  (let ((template-file (concat template-dir "/monkey-template.org")))
    (unless (file-exists-p template-file)
      (with-temp-file template-file
	(insert "* %<%I:%M %p> %(monkey-capture-distraction)\n:PROPERTIES:\n:Created: %U\n:Type: %(monkey-get-distraction-type)\n:Duration: %^{Duration in minutes}m\n:END:\n%?")))))

;; Function to quickly capture monkey moment
(defun monkey-capture ()
  (interactive)
  (org-capture nil "m"))

;; Global key bindings
(global-set-key (kbd "C-c m") 'monkey-capture)  ; Quick capture
(global-set-key (kbd "C-c M") 'monkey-open-log) ; Open log

;; Enable monkey-mode by default
(monkey-mode 1)
