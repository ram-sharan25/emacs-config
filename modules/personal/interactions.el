;;; interactions.el --- Meetings, private conversations and people -*- lexical-binding: t; -*-

;;; Commentary:
;; Internal meetings live in the exact Area/project selected at capture time.
;; Conversations and person records stay GPG-encrypted under Private/.  Person
;; links use a custom `person:' scheme rather than org-roam, because Private/
;; is deliberately absent from org-roam's plaintext database.

;;; Code:

(require 'cl-lib)
(require 'epa-file)
(epa-file-enable)
(require 'org)
(require 'org-capture)
(require 'org-id)
(require 'paths)
(require 'gtd-config)

(defvar my/interaction-capture-detailed nil
  "Non-nil while an interaction capture should prompt for optional details.")

(defvar my/interaction--capture-destination nil)
(defvar my/interaction--capture-people nil)
(defvar my/person--cache nil)
(defvar my/person--cache-mtime nil)

(defun my/interaction--file-metadata (file)
  "Return destination metadata for an Area or project FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((case-fold-search t)
           (title (and (re-search-forward "^#\\+TITLE:[ \t]*\\(.+\\)$" nil t)
                       (string-trim (match-string-no-properties 1))))
           (area (progn
                   (goto-char (point-min))
                   (and (re-search-forward "^#\\+PROPERTY:[ \t]+AREA[ \t]+\\(.+\\)$" nil t)
                        (string-trim (match-string-no-properties 1)))))
           (id (progn
                 (goto-char (point-min))
                 (and (re-search-forward "^[ \t]*:ID:[ \t]+\\([^ \t\n]+\\)" nil t)
                      (match-string-no-properties 1))))
           (hub (member file (my/get-area-hub-files))))
      (list :file file :title (or title (file-name-base file))
            :area area :id id :hub hub))))

(defun my/interaction-destinations ()
  "Return completion choices for all Area hubs and projects."
  (mapcar
   (lambda (metadata)
     (let ((label (if (plist-get metadata :hub)
                      (format "%s / General" (plist-get metadata :title))
                    (format "%s / %s" (plist-get metadata :area)
                            (plist-get metadata :title)))))
       (cons label metadata)))
   (mapcar #'my/interaction--file-metadata (my/get-area-files))))

(defun my/interaction-read-destination ()
  "Ask for an exact project, or an Area's General destination."
  (let* ((choices (my/interaction-destinations))
         (choice (completing-read "Project or general Area: " choices nil t)))
    (cdr (assoc choice choices))))

(defun my/interaction--goto-tasks (file)
  "Make FILE current and place point on its Tasks heading.
An Org capture `entry' target inserts beneath the heading and after its existing
subtree, preserving the project file's required Tasks hierarchy."
  (set-buffer (org-capture-target-buffer file))
  (widen)
  (goto-char (point-min))
  (unless (re-search-forward "^\\* Tasks\\(?:[ \t].*\\)?$" nil t)
    (user-error "No top-level Tasks heading in %s" file))
  (beginning-of-line))

(defun my/interaction--meeting-target ()
  "Select and prepare the destination for a meeting capture."
  (setq my/interaction--capture-destination (my/interaction-read-destination))
  (my/interaction--goto-tasks
   (plist-get my/interaction--capture-destination :file)))

(defun my/person--file-mtime ()
  "Modification time of `my/people-file', or nil when it does not exist."
  (when (file-exists-p my/people-file)
    (file-attribute-modification-time (file-attributes my/people-file))))

(defun my/person--read-records ()
  "Read person records from the encrypted people file."
  (when (file-exists-p my/people-file)
    (with-current-buffer (find-file-noselect my/people-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let (records)
         (while (re-search-forward "^\\* \\(.+\\)$" nil t)
           (beginning-of-line)
           (let ((id (org-entry-get nil "PERSON_ID"))
                 (aliases (org-entry-get nil "ALIASES"))
                 (relationship (org-entry-get nil "RELATIONSHIP"))
                 (name (org-get-heading t t t t)))
             (when id
               (push (list :id id :name name
                           :aliases (split-string (or aliases "") "[ \t]*,[ \t]*" t)
                           :relationship relationship)
                     records)))
           (outline-next-heading))
         (nreverse records))))))

(defun my/person-records (&optional refresh)
  "Return cached person records, refreshing when the encrypted file changed."
  (let ((mtime (my/person--file-mtime)))
    (when (or refresh (null my/person--cache)
              (not (equal mtime my/person--cache-mtime)))
      (setq my/person--cache (or (my/person--read-records) nil)
            my/person--cache-mtime mtime))
    my/person--cache))

(defun my/person--ensure-file-buffer ()
  "Return the people file buffer, creating its encrypted scaffold if needed."
  (make-directory (file-name-directory my/people-file) t)
  (let ((buffer (find-file-noselect my/people-file)))
    (with-current-buffer buffer
      (when (= (buffer-size) 0)
        (insert "#+TITLE: People\n#+STARTUP: overview\n\n")))
    buffer))

(defun my/person--append-record (name &optional relationship aliases)
  "Create a person named NAME and return the new record."
  (let ((id (org-id-new)))
    (with-current-buffer (my/person--ensure-file-buffer)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n:PROPERTIES:\n:PERSON_ID: %s\n" name id))
      (when relationship
        (insert (format ":RELATIONSHIP: %s\n" relationship)))
      (when aliases
        (insert (format ":ALIASES: %s\n" aliases)))
      (insert ":END:\n\n** Context\n\n** Things to remember\n\n** Open loops\n")
      (save-buffer))
    (my/person-records t)
    (seq-find (lambda (record) (equal id (plist-get record :id)))
              my/person--cache)))

(defun my/person-create (name relationship aliases)
  "Create a private person record with NAME, RELATIONSHIP and ALIASES."
  (interactive
   (list (read-string "Name: ")
         (read-string "Relationship (optional): ")
         (read-string "Aliases, comma-separated (optional): ")))
  (when (string-empty-p (string-trim name))
    (user-error "A person needs a name"))
  (let ((record (my/person--append-record
                 (string-trim name)
                 (unless (string-empty-p (string-trim relationship))
                   (string-trim relationship))
                 (unless (string-empty-p (string-trim aliases))
                   (string-trim aliases)))))
    (message "Created private person record for %s" (plist-get record :name))
    record))

(defun my/person--matching-record (name)
  "Return the record matching NAME or one of its aliases."
  (seq-find
   (lambda (record)
     (or (string-equal-ignore-case name (plist-get record :name))
         (seq-some (lambda (alias) (string-equal-ignore-case name alias))
                   (plist-get record :aliases))))
   (my/person-records)))

(defun my/person--ensure-record (name)
  "Return NAME's record, creating a minimal private record when absent."
  (or (my/person--matching-record name)
      (my/person--append-record name)))

(defun my/person-link (record)
  "Return an Org link for person RECORD, displayed as an @ mention."
  (format "[[person:%s][@%s]]"
          (plist-get record :id) (plist-get record :name)))

(defun my/person-read-multiple (&optional prompt)
  "Read zero or more people, creating minimal records for unknown names."
  (let* ((records (my/person-records))
         (names (delete-dups
                 (apply #'append
                        (mapcar (lambda (record)
                                  (cons (plist-get record :name)
                                        (plist-get record :aliases)))
                                records))))
         (selected (completing-read-multiple
                    (or prompt "People: ") names nil nil)))
    (mapcar (lambda (name) (my/person--ensure-record (string-trim name)))
            (seq-remove #'string-empty-p selected))))

(defun my/person--find-by-id (id)
  "Return the person record identified by ID."
  (seq-find (lambda (record) (equal id (plist-get record :id)))
            (my/person-records t)))

(defun my/person-open (id _)
  "Open the private person identified by ID."
  (let ((record (my/person--find-by-id id)))
    (unless record (user-error "Unknown person id: %s" id))
    (find-file my/people-file)
    (goto-char (point-min))
    (unless (re-search-forward
             (format "^[ \t]*:PERSON_ID:[ \t]+%s[ \t]*$" (regexp-quote id)) nil t)
      (user-error "Person record disappeared: %s" id))
    (org-back-to-heading t)
    (org-fold-show-context 'link-search)))

(org-link-set-parameters "person" :follow #'my/person-open
                         :export (lambda (_path description _backend _info)
                                   (or description "person")))

(defun my/person-read-one (&optional prompt)
  "Read one existing private person record."
  (let* ((records (my/person-records))
         (choices (mapcar (lambda (record)
                            (cons (plist-get record :name) record))
                          records)))
    (unless choices (user-error "No private person records exist yet"))
    (cdr (assoc (completing-read (or prompt "Person: ") choices nil t)
                choices))))

(defun my/person--interaction-files ()
  "Plain Brain files and encrypted conversation files that may mention people."
  (append
   (directory-files-recursively my/brain-dir "\\.org\\'")
   (when (file-directory-p my/conversations-dir)
     (directory-files-recursively my/conversations-dir "\\.org\\.gpg\\'"))))

(defun my/person--interaction-headings (record)
  "Return headings linking to person RECORD, decrypted only for this search."
  (let ((needle (concat "person:" (plist-get record :id)))
        seen results)
    (dolist (file (my/person--interaction-files))
      (unless (equal (expand-file-name file) (expand-file-name my/people-file))
        ;; A temporary buffer keeps a cross-reference search from opening every
        ;; Brain file.  EasyPG still decrypts .org.gpg input on demand, and the
        ;; plaintext disappears when this form exits.
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (goto-char (point-min))
          (while (search-forward needle nil t)
            (save-excursion
              (when (ignore-errors (org-back-to-heading t) t)
                (let ((key (cons file (point))))
                  (unless (member key seen)
                    (push key seen)
                    (push (list :file file
                                :heading (org-get-heading t t t t))
                          results)))))))))
    (nreverse results)))

(defun my/person-show-interactions (&optional record)
  "Show meetings and conversations mentioning RECORD in a temporary Org buffer."
  (interactive)
  (let* ((record (or record (my/person-read-one "Interactions with: ")))
         (name (plist-get record :name))
         (entries (my/person--interaction-headings record))
         (buffer (get-buffer-create (format "*Interactions with %s*" name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: Interactions with %s\n\n" name))
        (if entries
            (dolist (entry entries)
              (let* ((file (plist-get entry :file))
                     (heading (plist-get entry :heading))
                     (target (concat "file:" file "::*" heading)))
                (insert "- " (org-link-make-string target heading) "\n")))
          (insert "No interactions found.\n"))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun my/person-completion-at-point ()
  "Complete @ mentions as clickable links to private person records."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let ((end (point)))
        (when (re-search-backward
               "\\(?:^\\|[[:space:]([]\\)@\\([[:alnum:]_. -]*\\)\\="
               (line-beginning-position) t)
          (let* ((at (copy-marker (1- (match-beginning 1))))
                 (beg (match-beginning 1))
                 (records (my/person-records))
                 (candidates
                  (delete-dups
                   (apply #'append
                          (mapcar (lambda (record)
                                    (cons (plist-get record :name)
                                          (plist-get record :aliases)))
                                  records)))))
            (when candidates
              (list beg end candidates
                    :exclusive 'no
                    :exit-function
                    (lambda (candidate status)
                      (when (memq status '(finished sole exact))
                        (let ((record (my/person--matching-record candidate)))
                          (when record
                            (delete-region at (point))
                            (insert (my/person-link record))))))))))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      #'my/person-completion-at-point nil t)))

(defun my/interaction--people-lines (people)
  "Format PEOPLE for an Attendees or People section."
  (if people
      (concat "- " (mapconcat #'my/person-link people ", "))
    "- "))

(defun my/interaction--created ()
  "Current inactive Org timestamp."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun my/interaction--meeting-template ()
  "Template for an immediate internal meeting."
  (let* ((title (string-trim (read-string "Meeting: ")))
         (people (when my/interaction-capture-detailed
                   (my/person-read-multiple "Attendees: ")))
         (purpose (when my/interaction-capture-detailed
                    (string-trim (read-string "Purpose: ")))))
    (format "* IN-PROGRESS %s :people:\n:PROPERTIES:\n:ID: %s\n:CREATED: %s\n:INTERACTION_TYPE: meeting\n:END:\n\n** Attendees\n%s\n\n** Purpose\n- %s\n\n** Notes\n- %%?\n\n** Decisions\n\n** Outcome\n\n** Follow-ups\n"
            title (org-id-new) (my/interaction--created)
            (my/interaction--people-lines people) (or purpose ""))))

(defun my/interaction--scheduled-range ()
  "Prompt for a meeting start and duration and return an Org timestamp."
  (let* ((start (org-read-date nil t nil "Meeting start: "))
         (duration (org-duration-to-minutes
                    (read-string "Duration (H:MM): " nil nil "0:30")))
         (end (time-add start (* 60 duration))))
    (format "<%s %s-%s>"
            (format-time-string "%Y-%m-%d %a" start)
            (format-time-string "%H:%M" start)
            (format-time-string "%H:%M" end))))

(defun my/interaction--planned-meeting-template ()
  "Template for a planned local meeting that does not start a clock."
  (let* ((title (string-trim (read-string "Meeting: ")))
         (people (my/person-read-multiple "Attendees: "))
         (purpose (string-trim (read-string "Purpose: ")))
         (scheduled (my/interaction--scheduled-range)))
    (format "* TODO %s :people:\nSCHEDULED: %s\n:PROPERTIES:\n:ID: %s\n:CREATED: %s\n:INTERACTION_TYPE: meeting\n:END:\n\n** Attendees\n%s\n\n** Purpose\n- %s\n\n** Notes\n- %%?\n\n** Decisions\n\n** Outcome\n\n** Follow-ups\n"
            title scheduled (org-id-new) (my/interaction--created)
            (my/interaction--people-lines people) purpose)))

(defun my/interaction--conversation-file ()
  "Encrypted conversation file for the current year."
  (expand-file-name (format-time-string "%Y.org.gpg") my/conversations-dir))

(defun my/interaction--conversation-target ()
  "Select context and prepare this year's encrypted conversation file."
  (setq my/interaction--capture-destination (my/interaction-read-destination)
        my/interaction--capture-people (my/person-read-multiple "People: "))
  (let ((file (my/interaction--conversation-file)))
    (make-directory (file-name-directory file) t)
    (set-buffer (org-capture-target-buffer file))
    (when (= (buffer-size) 0)
      (insert (format "#+TITLE: Conversations %s\n#+STARTUP: overview\n\n"
                      (format-time-string "%Y"))))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))))

(defun my/interaction--destination-link (destination)
  "Return an Org link naming DESTINATION."
  (if-let ((id (plist-get destination :id)))
      (format "[[id:%s][%s]]" id (plist-get destination :title))
    (format "[[file:%s][%s]]" (plist-get destination :file)
            (plist-get destination :title))))

(defun my/interaction--conversation-template ()
  "Template for an immediate encrypted conversation."
  (let* ((names (mapconcat (lambda (record) (plist-get record :name))
                           my/interaction--capture-people ", "))
         (topic (when my/interaction-capture-detailed
                  (string-trim (read-string "Topic (optional): "))))
         (title (concat "Conversation"
                        (unless (string-empty-p names) (concat " with " names))
                        (unless (or (null topic) (string-empty-p topic))
                          (concat ": " topic)))))
    (format "* IN-PROGRESS %s :people:\n:PROPERTIES:\n:ID: %s\n:CREATED: %s\n:INTERACTION_TYPE: conversation\n:AREA: %s\n:END:\n\n- People: %s\n- Context: %s\n\n** Notes\n- %%?\n\n** Things decided\n\n** Things to remember\n\n** Follow-ups\n"
            title (org-id-new) (my/interaction--created)
            (or (plist-get my/interaction--capture-destination :area) "Personal")
            (if my/interaction--capture-people
                (mapconcat #'my/person-link my/interaction--capture-people ", ")
              "")
            (my/interaction--destination-link
             my/interaction--capture-destination))))

(defun my/capture-meeting (&optional detailed)
  "Capture an internal meeting starting now.
With prefix DETAILED, ask for attendees and purpose before opening the notes."
  (interactive "P")
  (let ((my/interaction-capture-detailed (and detailed t)))
    (org-capture nil "M")))

(defun my/capture-planned-meeting ()
  "Capture a scheduled internal meeting without starting a clock."
  (interactive)
  (org-capture nil "P"))

(defun my/capture-conversation (&optional detailed)
  "Capture a private conversation starting now.
With prefix DETAILED, also ask for a topic."
  (interactive "P")
  (let ((my/interaction-capture-detailed (and detailed t)))
    (org-capture nil "C")))

(add-to-list 'org-capture-templates
             '("M" "Internal meeting now" entry
               (function my/interaction--meeting-target)
               (function my/interaction--meeting-template)
               :clock-in t :clock-keep t :empty-lines 1)
             t)
(add-to-list 'org-capture-templates
             '("P" "Plan internal meeting" entry
               (function my/interaction--meeting-target)
               (function my/interaction--planned-meeting-template)
               :empty-lines 1)
             t)
(add-to-list 'org-capture-templates
             '("C" "Private conversation now" entry
               (function my/interaction--conversation-target)
               (function my/interaction--conversation-template)
               :clock-in t :clock-keep t :empty-lines 1)
             t)

(provide 'interactions)
;;; interactions.el ends here
