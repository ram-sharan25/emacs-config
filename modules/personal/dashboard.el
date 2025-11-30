;;; dashboard.el --- Project & Area Dashboard for linear thinking -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides read-only dashboard views for Projects and Areas.
;; Aggregates Tasks, Notes, and Resources to foster synthesis.
;; Classifies nodes based on TODO state, Tags, and File location.

;;; Code:

(require 'org)
(require 'org-roam)
(require 'paths)
(require 'plan) ; For project selection logic

;;; --- Link Retrieval & Classification ---

(defun my/dashboard-get-all-links (id)
  "Return a list of unique IDs linked to/from ID."
  (let ((ids (list)))
    ;; 1. Get Backlinks (Nodes linking TO id)
    (let* ((source-node (org-roam-node-from-id id))
           (backlinks (if source-node (org-roam-backlinks-get source-node) '())))
      (dolist (backlink backlinks)
        (push (org-roam-node-id (org-roam-backlink-source-node backlink)) ids)))

    ;; 2. Get Forward Links (Links FROM id content)
    (save-excursion
      (org-id-goto id)
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (let ((link (match-string-no-properties 1)))
          (when (string-prefix-p "id:" link)
            (push (substring link 3) ids))))
      (widen))

    (delete-dups ids)))

(defun my/dashboard-classify-node (id)
  "Return a plist with node details and :type (TASK-ACTIVE, TASK-DONE, NOTE, RESOURCE)."
  (let ((node (org-roam-node-from-id id)))
    (if (not node)
        nil ;; Node not found in DB
      (let* ((todo (org-roam-node-todo node))
             (tags (org-roam-node-tags node))
             (file (org-roam-node-file node))
             (title (org-roam-node-title node))
             (props (org-roam-node-properties node))
             (date-str (or (cdr (assoc "CREATED" props))
                           (cdr (assoc "TIME" props))
                           (format-time-string "[%Y-%m-%d %a]")))
             (date-ts (condition-case nil
                          (org-time-string-to-seconds date-str)
                        (error (float-time))))
             (type
              (cond
               ;; 1. TASKS: If it has a TODO keyword
               ((and todo (not (string-empty-p todo)))
                (if (member todo '("DONE" "CANCELED" "DEFERRED"))
                    "TASK-DONE"
                  "TASK-ACTIVE"))

               ;; 2. NOTES: Explicit tag or file
               ((or (member "NOTE" tags)
                    (member "IDEA" tags)
                    (string-match-p "rough_notes.org" file))
                "NOTE")

               ;; 3. RESOURCES: Everything else
               (t "RESOURCE"))))

        (list :id id
              :headline title
              :date-str date-str
              :date date-ts
              :type type
              :todo todo)))))

;;; --- Project Retrieval for Areas ---

(defun my/dashboard-get-archived-projects (area-name)
  "Return list of (Name . ID) for archived projects in AREA-NAME."
  (let ((archive-file (expand-file-name "projects.org_archive" my/archive-dir))
        (projects '()))
    (when (file-exists-p archive-file)
      (with-current-buffer (find-file-noselect archive-file)
        (org-map-entries
         (lambda ()
           (let* ((heading (org-get-heading t t))
                  (id (org-id-get))
                  (area (org-entry-get nil "AREA")))
             (when (and (string= area area-name) id)
               (push (cons heading id) projects))))
         "LEVEL=1" 'file)))
    (nreverse projects)))

;;; --- Rendering ---

(defun my/dashboard-insert-section (title items)
  "Insert a section TITLE with ITEMS into the current buffer."
  (insert (propertize (concat "\n* " title "\n") 'face 'org-level-1))
  (if (null items)
      (insert "  (No items found)\n")
    (dolist (item items)
      (let ((headline (plist-get item :headline))
            (date (plist-get item :date-str))
            (type (plist-get item :type))
            (todo (plist-get item :todo))
            (id (plist-get item :id)))

        (insert "  - ")
        ;; Show TODO keyword if present
        (when (and todo (not (string-empty-p todo)))
          (insert (propertize (format "%s " todo)
                              'face (if (member todo '("DONE" "CANCELED")) 'org-done 'org-todo))))

        (when (and date (not (string-empty-p date)))
          (insert (format "%s " (propertize date 'face 'org-date))))

        (insert-text-button headline
                            'action (lambda (_) (org-id-goto id))
                            'help-echo "Jump to Item"
                            'face 'link)
        (insert "\n")))))

(defun my/dashboard-insert-project-list (title projects)
  "Insert a list of PROJECTS (Name . ID) under TITLE."
  (insert (propertize (concat "\n* " title "\n") 'face 'org-level-1))
  (if (null projects)
      (insert "  (No projects found)\n")
    (dolist (proj projects)
      (let ((name (car proj))
            (id (cdr proj)))
        (insert "  - ")
        (insert-text-button name
                            'action (lambda (_) (org-id-goto id))
                            'help-echo "Jump to Project"
                            'face 'link)
        (insert "\n")))))

;;; --- Main Dashboards ---

(defun my/project-dashboard ()
  "Open a dashboard for a selected project with 4 sections."
  (interactive)
  (let* ((project-cons (my/org-select-project-allow-empty))
         (project-name (car project-cons))
         (project-id (cdr project-cons)))

    (when (or (null project-id) (string-empty-p project-id))
      (user-error "Selected project '%s' has no ID. Cannot fetch dashboard." project-name))

    (let ((buffer-name (format "*Dashboard: %s*" project-name)))
      (pop-to-buffer (get-buffer-create buffer-name))
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)

      (insert (propertize (format "#+TITLE: Project Dashboard: %s\n" project-name) 'face 'org-document-title))
      (insert (format "Generated: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (format "Project ID: [[id:%s][%s]]\n\n" project-id project-id))

      (message "Gathering items...")
      (let* ((ids (my/dashboard-get-all-links project-id))
             (items (delq nil (mapcar #'my/dashboard-classify-node ids)))

             (active-tasks (seq-filter (lambda (it) (string= (plist-get it :type) "TASK-ACTIVE")) items))
             (done-tasks   (seq-filter (lambda (it) (string= (plist-get it :type) "TASK-DONE")) items))
             (notes        (seq-filter (lambda (it) (string= (plist-get it :type) "NOTE")) items))
             (resources    (seq-filter (lambda (it) (string= (plist-get it :type) "RESOURCE")) items)))

        (my/dashboard-insert-section "Active Tasks"
                                     (sort active-tasks (lambda (a b) (> (plist-get a :date) (plist-get b :date)))))
        (my/dashboard-insert-section "Completed Tasks"
                                     (sort done-tasks (lambda (a b) (> (plist-get a :date) (plist-get b :date)))))
        (my/dashboard-insert-section "Notes & Ideas"
                                     (sort notes (lambda (a b) (> (plist-get a :date) (plist-get b :date)))))
        (my/dashboard-insert-section "Resources & Context"
                                     (sort resources (lambda (a b) (> (plist-get a :date) (plist-get b :date))))))

      (read-only-mode 1)
      (local-set-key (kbd "q") 'bury-buffer)
      (message "Dashboard ready."))))

(defun my/area-dashboard ()
  "Open a dashboard for a selected Area."
  (interactive)
  (let* ((area-name (my/select-area-default-misc))
         (area-id (my/get-area-id-by-name area-name)))

    (let ((buffer-name (format "*Dashboard: Area %s*" area-name)))
      (pop-to-buffer (get-buffer-create buffer-name))
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)

      (insert (propertize (format "#+TITLE: Area Dashboard: %s\n" area-name) 'face 'org-document-title))
      (insert (format "Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))

      (message "Gathering Area data...")

      ;; 1. Projects
      (let ((active-projects (my/org-get-project-headings area-name))
            (archived-projects (my/dashboard-get-archived-projects area-name)))
        (my/dashboard-insert-project-list "Active Projects" active-projects)
        (my/dashboard-insert-project-list "Archived Projects" archived-projects))

      ;; 2. Direct Tasks/Notes (linked to Area ID)
      (when area-id
        (let* ((ids (my/dashboard-get-all-links area-id))
               (items (delq nil (mapcar #'my/dashboard-classify-node ids)))

               (active-tasks (seq-filter (lambda (it) (string= (plist-get it :type) "TASK-ACTIVE")) items))
               (notes        (seq-filter (lambda (it) (string= (plist-get it :type) "NOTE")) items)))

          (my/dashboard-insert-section "Area Tasks (Directly Linked)"
                                       (sort active-tasks (lambda (a b) (> (plist-get a :date) (plist-get b :date)))))
          (my/dashboard-insert-section "Area Notes (Directly Linked)"
                                       (sort notes (lambda (a b) (> (plist-get a :date) (plist-get b :date)))))))

      (read-only-mode 1)
    (local-set-key (kbd "q") 'bury-buffer)
      (message "Area Dashboard ready."))))

;;; --- Keybindings ---

;; Define a prefix command for "View"
(define-prefix-command 'my/view-map)
(global-set-key (kbd "C-c v") 'my/view-map)

(define-key my/view-map (kbd "p") 'my/project-dashboard)
(define-key my/view-map (kbd "a") 'my/area-dashboard)


(provide 'dashboard)
;;; dashboard.el ends here
