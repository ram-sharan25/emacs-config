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

(defun my/process-legacy-note ()
  "Interactively assign AREA and PROJECT to the current heading, insert links, then archive it.
Prompts for:
1. Area (Name)
2. Project (Name, filtered by Area)
Sets :AREA: and :PROJECT: properties.
Inserts :LINKS: [[id:AREA_ID][Area]] [[id:PROJECT_ID][Project]].
Archives the subtree."
  (interactive)
  (let* ((areas (my/get-areas-alist))
         (area-names (mapcar #'car areas))
         ;; Prompt for Area
         (selected-area-name (completing-read "Select Area: " area-names nil t))
         (selected-area-id (cdr (assoc selected-area-name areas)))
         
         ;; Get projects filtered by Area
         (projects (my/get-projects-alist selected-area-name))
         (project-names (mapcar #'car projects))
         ;; Prompt for Project
         (selected-project-name (completing-read (format "Select Project for %s (or None): " selected-area-name) 
                                                 (cons "None" project-names) nil t))
         (selected-project-id (cdr (assoc selected-project-name projects))))

    ;; Set Properties
    (org-set-property "AREA" selected-area-name)
    (if (and selected-project-name (not (string= selected-project-name "None")))
        (org-set-property "PROJECT" selected-project-name)
      (org-delete-property "PROJECT"))

    ;; Insert Links
    (unless (save-excursion 
              (re-search-forward "^:LINKS: \\[\\[id:" (save-excursion (org-end-of-subtree) (point)) t))
      (save-excursion
        (org-back-to-heading t)
        (forward-line 1)
        ;; Skip properties/drawer
        (while (looking-at-p "^[ \t]*:[A-Za-z]+:") (forward-line 1))
        (while (looking-at-p "^[ \t]*:END:") (forward-line 1))
        
        (insert "\n")
        (insert (format ":LINKS: [[id:%s][%s]]" selected-area-id selected-area-name))
        (when (and selected-project-id (not (string= selected-project-name "None")))
          (insert (format " [[id:%s][%s]]" selected-project-id selected-project-name)))
        (insert "\n")))
    
    ;; Archive
    (org-archive-subtree)
    (message "Processed and archived: %s -> Area: %s, Project: %s"
             (org-get-heading t t t t) selected-area-name selected-project-name)))

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
