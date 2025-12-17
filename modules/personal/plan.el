;;; project-tasks.el --- A system for capturing tasks linked to projects -*- lexical-binding: t; -*-

(require 'paths)

;;; --- 3. Helper Functions to Read Project Names ---

(defun my/get-area-names ()
  "Return a list of Area names (filenames without extension) from `my/areas-dir`."
  (let ((files (directory-files my/areas-dir nil "\\.org$")))
    (mapcar #'file-name-sans-extension files)))

(defun my/get-area-id-by-name (area-name)
  "Return the ID of the Area file corresponding to AREA-NAME.
Creates the ID if it doesn't exist."
  (let ((file (expand-file-name (concat area-name ".org") my/areas-dir)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-id-get-create)))))

(defun my/org-get-project-headings (&optional area-filter)
  "Return an alist of (Project Name . ID) from `my/projects-file` with ACTIVE=TRUE.
If AREA-FILTER is provided, only include projects with a matching :AREA: property."
  (with-current-buffer (find-file-noselect my/projects-file)
    (let ((projects '()))
      (org-map-entries
       (lambda ()
         (let* ((heading (org-get-heading t t))
                (id (org-id-get-create))
                (area (org-entry-get nil "AREA")))
           ;; Filter by Area if provided
           (when (or (null area-filter)
                     (string= area area-filter))
             (push (cons heading id) projects))))
       "LEVEL=1+ACTIVE=\"TRUE\"" 'file)
      (nreverse projects))))

(defun my/select-area-default-misc ()
  "Prompt for Area, defaulting to 'Misc' and showing it first."
  (let* ((areas (my/get-area-names))
         ;; Ensure Misc is first and unique
         (options (cons "Misc" (remove "Misc" areas)))
         (selected (completing-read "Area: " options nil t nil nil "Misc")))
    (if (string-empty-p selected) "Misc" selected)))

(defun my/org-select-project-allow-empty (&optional area-filter)
  "Prompt user to select a project, optionally filtered by AREA-FILTER.
Returns a cons cell (Name . ID). Includes 'Dump' as the first option."
  (let* ((project-alist (my/org-get-project-headings area-filter))
         ;; Prepend "Dump" option explicitly so it appears first
         (options (cons '("Dump" . nil) project-alist))
         (project-names (mapcar #'car options))
         (selected-name (completing-read "Select Project: "
                                         project-names nil t nil nil "Dump")))
    (if (string-empty-p selected-name)
        '("Dump" . nil)
      (assoc selected-name options))))



(provide 'plan)
