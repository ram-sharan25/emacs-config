;;; migration-utils.el --- Utilities for migrating legacy notes -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities to help migrate legacy notes from rough_notes.org (or anywhere)
;; to the new PARA structure.
;;
;; Usage:
;; 1. Load this file: (load-file "~/.emacs.d/modules/personal/migration-utils.el")
;; 2. Go to a heading you want to process.
;; 3. Run M-x my/process-legacy-note

;;; Code:

(require 'org)
(require 'org-id)
(require 'paths)
(require 'plan)

(defun my/get-area-names ()
  "Return a list of Area names."
  (mapcar #'car (my/get-areas-alist)))

(defun my/get-projects-alist (&optional area)
  "Return an alist of (Name . ID) for active projects.
If AREA is provided, filter projects by that Area."
  (let ((projects '()))
    (with-current-buffer (find-file-noselect my/projects-file)
      (org-map-entries
       (lambda ()
	 (let ((project-area (org-entry-get nil "AREA"))
	       (name (org-get-heading t t t t))
	       (id (org-id-get)))
	   (when (and id
		      (or (not area)
			  (and project-area (string= project-area area))))
	     (push (cons name id) projects))))
       "LEVEL=1"))
    (nreverse projects)))

(defun my/get-areas-alist ()
  "Return an alist of (Name . ID) for all Areas in `my/areas-dir`.
If an Area file is empty or missing an ID, initialize it."
  (let ((areas '())
	(files (directory-files my/areas-dir t "\\.org$")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
	(let ((id (org-id-get-create)) ; Create ID if missing (file-level)
	      (name (file-name-sans-extension (file-name-nondirectory file))))

	  ;; If file was empty/new, ensure it has a title
	  (goto-char (point-min))
	  (unless (re-search-forward "^#\\+title:" nil t)
	    (insert (format "#+title: %s\n" name)))

	  (save-buffer) ; Save the new ID/Title to disk
	  (push (cons name id) areas))))
    (nreverse areas)))

(defun my/archive-task-or-note ()
  "Archive the current entry.
   If AREA/PROJECT are missing, prompt for them.
   Adds ID-based links to Resources before archiving."
  (interactive)

  ;; 1. Get or Prompt for Metadata
  (let* ((area-name (org-entry-get nil "AREA"))
	 (project-name (org-entry-get nil "PROJECT"))
	 (area-id nil)
	 (project-id nil))

    ;; Prompt if missing
    (unless area-name
      (setq area-name (my/select-area-default-misc))
      (org-entry-put nil "AREA" area-name))

    (unless project-name
      (let* ((project-cons (my/org-select-project-allow-empty area-name)))
	(setq project-name (car project-cons))
	(org-entry-put nil "PROJECT" project-name)))

    ;; Retrieve IDs
    (setq area-id (my/get-area-id-by-name area-name))
    (let ((project-cons (assoc project-name (my/org-get-project-headings area-name))))
      (setq project-id (cdr project-cons)))

    ;; 2. Add Resources Links
    (save-excursion
      (let* ((end-pos (org-entry-end-position))
	     (resources-pos (save-excursion
			      (org-back-to-heading t)
			      (re-search-forward "^\\*+ Resources" end-pos t))))

	(if resources-pos
	    ;; Case A: Resources Exists -> Append Links
	    (progn
	      (goto-char resources-pos)
	      (org-end-of-subtree t) ;; Go to end of Resources content
	      (unless (bolp) (insert "\n"))

	      ;; Append Project Link if missing
	      (when (and project-id
			 (not (save-excursion
				(goto-char resources-pos)
				(re-search-forward (format "id:%s" project-id) (point-max) t))))
		(insert (format "- Project: [[id:%s][%s]]\n" project-id project-name)))

	      ;; Append Area Link if missing
	      (when (and area-id
			 (not (save-excursion
				(goto-char resources-pos)
				(re-search-forward (format "id:%s" area-id) (point-max) t))))
		(insert (format "- Area: [[id:%s][%s]]\n" area-id area-name))))

	  ;; Case B: Resources Missing -> Create New
	  (goto-char end-pos)
	  (insert "\n** Resources\n")
	  (when project-id
	    (insert (format "- Project: [[id:%s][%s]]\n" project-id project-name)))
	  (insert (format "- Area: [[id:%s][%s]]\n" area-id area-name)))))

    ;; 3. Archive
    (org-archive-subtree)
    (message "Archived: %s (Area: %s, Project: %s)"
	     (org-get-heading t t t t) area-name project-name)))

(defun my/batch-add-links ()
  "Scan the current buffer for notes with :AREA: and :PROJECT: properties.
If found, look up their IDs and insert 'Links: [[id:...]]' if not already present.
This is non-interactive and processes the whole buffer."
  (interactive)
  (message "DEBUG: Starting batch link insertion...")
  (let ((areas (my/get-areas-alist))
	(projects (my/get-projects-alist))
	(count 0))
    (message "DEBUG: Loaded %d Areas and %d Projects" (length areas) (length projects))
    (org-map-entries
     (lambda ()
       (let* ((heading (org-get-heading t t t t))
	      (area-name (org-entry-get nil "AREA"))
	      (project-name (org-entry-get nil "PROJECT"))
	      (area-id (cdr (assoc area-name areas)))
	      (project-id (cdr (assoc project-name projects))))

	 (message "DEBUG: Checking '%s' | Area: %s (ID: %s) | Project: %s (ID: %s)"
		  heading area-name area-id project-name project-id)

	 (when (and area-name area-id)
	   ;; Check if already linked
	   (if (save-excursion
		 (re-search-forward "^:LINKS: \\[\\[id:" (save-excursion (org-end-of-subtree) (point)) t))
	       (message "DEBUG: Skipping '%s' (Already linked)" heading)

	     ;; Insert Links
	     (save-excursion
	       (org-back-to-heading t)
	       (forward-line 1)
	       ;; Skip properties/drawer
	       (while (looking-at-p "^[ \t]*:[A-Za-z]+:") (forward-line 1))
	       (while (looking-at-p "^[ \t]*:END:") (forward-line 1))

	       (insert "\n")
	       (insert (format ":LINKS: [[id:%s][%s]]" area-id area-name))
	       (when (and project-name project-id)
		 (insert (format " [[id:%s][%s]]" project-id project-name)))
	       (insert "\n")
	       (setq count (1+ count))
	       (message "DEBUG: Linked '%s'" heading))))))
     "LEVEL=1")
    (message "Batch processed: Added links to %d notes." count)))

(provide 'migration-utils)
;;; migration-utils.el ends here
