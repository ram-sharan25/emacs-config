;;; paths.el --- Central file and directory paths -*- lexical-binding: t; -*-

;;; Commentary:
;; Single source of truth for all paths in the config.
;; All paths follow ~/Stillness/Brain/{Areas/<Area>/,gtd,Private,Trackers}
;; Use (require 'paths) in other modules to reference these variables.

;;; Code:

;;; Root Directories

(defconst my/root-dir "~/Stillness/"
  "Root directory of the Stillness system.")

(defconst my/brain-dir (expand-file-name "Brain/" my/root-dir)
  "Brain directory containing all org-roam notes.")

(defconst my/private-dir (expand-file-name "Private/" my/brain-dir)
  "Private notes directory; only GPG-encrypted .gpg files are tracked in git.")

(defconst my/trackers-dir (expand-file-name "Trackers/" my/brain-dir)
  "Tool-written plaintext Org tables and logs; git-crypt in git (D11).")

(defconst my/gtd-dir (expand-file-name "gtd/" my/brain-dir)
  "GTD files directory.")

(defconst my/areas-dir (expand-file-name "Areas/" my/brain-dir)
  "Areas/<Area>/<Area>.org hubs and Areas/<Area>/<project>.org project files (D3).")

(defconst my/area-names '("Impact" "Personal" "Craft")
  "The three live areas (D3). Each is a folder under `my/areas-dir' and a Toggl project.")

(defconst my/project-kinds
  '("build" "write" "learn" "practice" "tool" "admin" "health" "people" "reflect" "play")
  "Closed kind vocabulary (D4): one or two per project file, loose task or habit.")

(defconst my/resources-dir (expand-file-name "Resources/" my/brain-dir)
  "Resource hubs — topics, books, references.")

(defconst my/data-dir (expand-file-name "data/" my/brain-dir)
  "Central directory for all media assets (images, PDFs, etc).")

(defconst my/archive-dir (expand-file-name "Archives/" my/brain-dir)
  "Directory for archived tasks and projects.")

(defconst my/library-dir (expand-file-name "Library/" my/root-dir)
  "Static resources directory (Books, Music, PDFs).")

(defconst my/ai-engineering-dir (expand-file-name "Resources/" my/library-dir)
  "Directory for my AI engineering resources (video courses, one folder each).")



(defconst my/development-dir (expand-file-name "Development/" my/root-dir)
  "Development projects and code.")

(defconst my/export-output-dir (expand-file-name "output/" my/brain-dir)
  "Centralized directory for all Org exports.")

;;; GTD Files

(defconst my/inbox-file (expand-file-name "inbox.org" my/gtd-dir)
  "Inbox — all new captures land here.")

(defconst my/someday-file (expand-file-name "someday.org" my/gtd-dir)
  "Someday/Maybe list.")

(defconst my/waiting-file (expand-file-name "waiting.org" my/gtd-dir)
  "Tasks waiting on someone else.")

(defconst my/rituals-file (expand-file-name "rituals.org" my/gtd-dir)
  "Recurring habits and rituals.")

(defconst my/discarded-file (expand-file-name "discarded.org" my/gtd-dir)
  "Discarded/cancelled tasks.")

(defconst my/gcal-calendars-dir (expand-file-name "calendars/" my/gtd-dir)
  "One Org file per synced Google calendar.")

(defconst my/gcal-calendars-file (expand-file-name "gcal-calendars.el" user-emacs-directory)
  "Git-ignored file that sets `my/gcal-calendars' (keeps account names out of this repo).")

(defvar my/gcal-calendars nil
  "Google calendars to sync, set in `my/gcal-calendars-file'.
Each entry is a plist: :label for prompts, :account whose OAuth token
is used, :id the calendar ID, and :file the Org file events go to.")

(load my/gcal-calendars-file t t)

(defconst my/gcal-default-calendar "Personal"
  "Label of the calendar preselected when pushing.")

;;; GTD Files

(defconst my/rough-notes-file (expand-file-name "fleeting_notes.org" my/gtd-dir)
  "Fleeting notes: quick capture of unprocessed thoughts (D10).")

;;; Trackers Files

(defconst my/video-history-file (expand-file-name "video-history.org" my/trackers-dir)
  "Watched inspiration videos, appended by `my/video-log-entry' in tools.el.")

(defconst my/linear-org-file (expand-file-name "linear.org" my/export-output-dir)
  "Linear issues mirror written by linear-emacs; regenerated, not in git.")

;;; Private Files

;; Files under Private/ are GPG-encrypted (*.gpg); EasyPG decrypts them on
;; open and re-encrypts to this key on save without asking for recipients.
(setq-default epa-file-encrypt-to '("908F2A3087D3DFFCD986F8616BEE6200A001282B"))

(defconst my/journal-file (expand-file-name "Journal/Journal.org.gpg" my/private-dir)
  "Daily journal (private, GPG-encrypted).")

(defconst my/monthly-reviews-file
  (expand-file-name "Journal/monthly-reviews.org.gpg" my/private-dir)
  "Dated monthly reviews (private, GPG-encrypted).")

(defconst my/weekly-reviews-file
  (expand-file-name "Journal/weekly-reviews.org.gpg" my/private-dir)
  "Dated weekly reviews (private, GPG-encrypted).")

(defconst my/monthly-review-template-file
  (expand-file-name "templates/monthly-review.org" user-emacs-directory)
  "Org template used to create monthly reviews.")

(defconst my/weekly-review-template-file
  (expand-file-name "templates/weekly-review.org" user-emacs-directory)
  "Org template used to create weekly reviews.")

;;; Personal

(defconst my/job-applications-dir "~/Stillness/Personal/Applications/"
  "Job applications directory.")

(defconst my/job-applications-file
  (expand-file-name "Personal/job-applications.org" my/areas-dir)
  "Job applications tracker: a Personal project file (one heading per job under * Tasks).")

(defconst my/cv-library-dir (expand-file-name "automate-resume/outputs/"   my/job-applications-dir)
  "Job applications CV library directory.")
;;; Development

(defconst my/project-search-dirs
  `((,my/development-dir . 2)
    (,(expand-file-name "IMPACT_WORK/Projects/" my/root-dir) . 3))
  "Where repositories live, each with how many levels down to look.
Used as `magit-repository-directories' and `projectile-project-search-path',
so a repository created anywhere under these is found without registering it.")

(defconst my/leetcode-dir (expand-file-name "NeetCode/" my/development-dir)
  "LeetCode/NeetCode solutions and notes.")

;;; Classes

(defconst my/classes-dir (expand-file-name "Classes/" my/root-dir)
  "University classes directory.")

(defconst my/third-sem-dir (expand-file-name "Third Sems (2025 Fall)/" my/classes-dir)
  "Third semester (Fall 2025) course files.")

(defconst my/fourth-sem-dir (expand-file-name "Fourth Sem(Spring 2026)/" my/classes-dir)
  "Fourth semester (Spring 2026) course files.")

(defconst my/coa-lectures (expand-file-name "Advanced Architecture/Lectures/" my/fourth-sem-dir)
  "Computer Architecture lecture files.")

(defconst my/dsa-lectures (expand-file-name "Data Structures and Algorithms/Class Lectures/" my/third-sem-dir)
  "DSA lecture files.")

;;; External

(defconst my/zotero-storage "~/Zotero/storage/"
  "Zotero PDF storage directory.")

;;; Secrets

(defconst my/secrets-file (expand-file-name "secrets.el.gpg" user-emacs-directory)
  "Encrypted elisp holding API keys and tokens (see docs/secrets.org).")

(defvar my/secrets-loaded-p nil
  "Non-nil once `my/load-secrets' has loaded `my/secrets-file'.")

(defun my/load-secrets ()
  "Load `my/secrets-file' once; return non-nil if it is loaded.
Decrypting prompts for the passphrase, so callers load it on first need
(gcal.el at startup, Linear and Forge on their first request).  A failure
is reported in the echo area rather than signalled."
  (or my/secrets-loaded-p
      (condition-case err
          (progn
            (require 'epa-file)         ; pinentry mode: gcal.el, per OS
            (load my/secrets-file)
            (setq my/secrets-loaded-p t))
        (error (message "Failed to load %s: %s" my/secrets-file
                        (error-message-string err))
               nil))))

(provide 'paths)
;;; paths.el ends here
