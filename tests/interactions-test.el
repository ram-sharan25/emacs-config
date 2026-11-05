;;; interactions-test.el --- Tests for interactions.el -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -L modules/personal -l tests/interactions-test.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(require 'interactions)

(defmacro interaction-test--with-temp (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "interactions-" t)))
          (my/brain-dir root)
          (my/areas-dir (expand-file-name "Areas/" root))
          (my/private-dir (expand-file-name "Private/" root))
          (my/people-file (expand-file-name "People/people.org" my/private-dir))
          (my/conversations-dir (expand-file-name "Conversations/" my/private-dir))
          (my/area-names '("Impact" "Personal" "Craft"))
          (my/person--cache nil)
          (my/person--cache-mtime nil))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file root)
             (kill-buffer buffer))))
       (delete-directory root t))))

(defun interaction-test--area-file (area title filename &optional hub)
  (let ((file (expand-file-name (format "%s/%s" area filename) my/areas-dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: %s\n#+PROPERTY: AREA %s\n#+FILETAGS: :%s:\n\n* Tasks\n"
                      (org-id-new) title area (if hub "hub" "people"))))
    file))

(ert-deftest interaction-destinations-include-projects-and-general-areas ()
  (interaction-test--with-temp
    (dolist (area my/area-names)
      (interaction-test--area-file area area (concat area ".org") t))
    (interaction-test--area-file "Impact" "MMGIS" "mmgis.org")
    (let ((labels (mapcar #'car (my/interaction-destinations))))
      (should (member "Personal / General" labels))
      (should (member "Impact / MMGIS" labels)))))

(ert-deftest interaction-person-records-and-links ()
  (interaction-test--with-temp
    (let ((record (my/person--append-record "Carson" "coworker" "CJ")))
      (should (string-match-p "\\`\\[\\[person:[^]]+\\]\\[@Carson\\]\\]\\'"
                              (my/person-link record)))
      (should (equal "Carson" (plist-get (my/person--matching-record "CJ") :name)))
      (should (string-match-p "RELATIONSHIP: coworker"
                              (with-temp-buffer
                                (insert-file-contents my/people-file)
                                (buffer-string)))))))

(ert-deftest interaction-person-cross-reference-finds-plain-meetings ()
  (interaction-test--with-temp
    (let* ((record (my/person--append-record "Carson" "coworker"))
           (file (expand-file-name "Areas/Impact/meeting.org" root)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (format "* DONE Architecture meeting\n- Attendees: %s\n"
                        (my/person-link record))))
      (let ((matches (my/person--interaction-headings record)))
        (should (= 1 (length matches)))
        (should (equal "Architecture meeting"
                       (plist-get (car matches) :heading)))))))

(ert-deftest interaction-at-completion-inserts-person-link ()
  (interaction-test--with-temp
    (my/person--append-record "Carson" "coworker")
    (with-temp-buffer
      (org-mode)
      (insert "Ask @Car")
      (let* ((capf (my/person-completion-at-point))
             (beg (nth 0 capf))
             (end (nth 1 capf))
             (exit (plist-get (nthcdr 3 capf) :exit-function)))
        (delete-region beg end)
        (goto-char beg)
        (insert "Carson")
        (funcall exit "Carson" 'finished)
        (should (string-match-p "Ask \\[\\[person:[^]]+\\]\\[@Carson\\]\\]"
                                (buffer-string)))))))

(ert-deftest interaction-meeting-capture-is-filed-under-project-tasks ()
  (interaction-test--with-temp
    (dolist (area my/area-names)
      (interaction-test--area-file area area (concat area ".org") t))
    (let* ((file (interaction-test--area-file "Impact" "MMGIS" "mmgis.org"))
           (destination (my/interaction--file-metadata file))
           (org-capture-templates
            '(("X" "Test meeting" entry
               (function my/interaction--meeting-target)
               "* TODO Captured meeting\n" :immediate-finish t))))
      (cl-letf (((symbol-function 'my/interaction-read-destination)
                 (lambda () destination)))
        (org-capture nil "X"))
      (with-temp-buffer
        (insert-file-contents file)
        (should (re-search-forward "^\\* Tasks\n\\*\\* TODO Captured meeting$" nil t))))))

(ert-deftest interaction-quick-meeting-leaves-details-for-backfill ()
  (interaction-test--with-temp
    (let ((my/interaction-capture-detailed nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "Architecture sync")))
        (let ((text (my/interaction--meeting-template)))
          (should (string-match-p "IN-PROGRESS Architecture sync :people:" text))
          (should (string-match-p "\\*\\* Attendees\n- " text))
          (should (string-match-p "\\*\\* Notes\n- %?" text))
          (should (string-match-p "\\*\\* Follow-ups" text)))))))

(ert-deftest interaction-detailed-meeting-includes-people-and-purpose ()
  (interaction-test--with-temp
    (let* ((person (my/person--append-record "Carson" "coworker"))
           (my/interaction-capture-detailed t))
      (cl-letf (((symbol-function 'my/person-read-multiple) (lambda (&rest _) (list person)))
                ((symbol-function 'read-string)
                 (let ((answers '("Architecture sync" "Choose the implementation")))
                   (lambda (&rest _) (pop answers)))))
        (let ((text (my/interaction--meeting-template)))
          (should (string-match-p "@Carson" text))
          (should (string-match-p "Choose the implementation" text)))))))

(ert-deftest interaction-conversation-template-stays-private-and-keeps-context ()
  (interaction-test--with-temp
    (let* ((person (my/person--append-record "Aama" "family"))
           (my/interaction--capture-people (list person))
           (my/interaction--capture-destination
            (list :title "Personal" :area "Personal" :id "personal-id")))
      (let ((text (my/interaction--conversation-template)))
        (should (string-match-p "Conversation with Aama" text))
        (should (string-match-p "@Aama" text))
        (should (string-match-p "\\[\\[id:personal-id\\]\\[Personal\\]\\]" text))
        (should (string-suffix-p ".org.gpg" (my/interaction--conversation-file)))))))

(provide 'interactions-test)
;;; interactions-test.el ends here
