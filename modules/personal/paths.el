;;; paths.el --- Central file and directory paths -*- lexical-binding: t; -*-

;;; Commentary:
;; Single source of truth for all paths in the config.
;; All paths follow ~/Stillness/Brain/{Areas,Dashboard,Private,gtd}
;; Use (require 'paths) in other modules to reference these variables.

;;; Code:

;;; Root Directories

(defconst my/root-dir "~/Stillness/"
  "Root directory of the Stillness system.")

(defconst my/brain-dir (expand-file-name "Brain/" my/root-dir)
  "Brain directory containing all org-roam notes.")

(defconst my/public-dir (expand-file-name "Public/" my/brain-dir)
  "Public knowledge notes directory (Git-synced).")

(defconst my/private-dir (expand-file-name "Private/" my/brain-dir)
  "Private notes directory (Git-ignored, sensitive content).")

(defconst my/dashboard-dir (expand-file-name "Dashboard/" my/brain-dir)
  "Dashboard directory for workflow files.")

(defconst my/gtd-dir (expand-file-name "gtd/" my/brain-dir)
  "GTD files directory.")

(defconst my/areas-dir (expand-file-name "Areas/" my/brain-dir)
  "Area hubs — ongoing responsibilities.")

(defconst my/resources-dir (expand-file-name "Resources/" my/brain-dir)
  "Resource hubs — topics, books, references.")

(defconst my/data-dir (expand-file-name "data/" my/brain-dir)
  "Central directory for all media assets (images, PDFs, etc).")

(defconst my/archive-dir (expand-file-name "Archives/" my/brain-dir)
  "Directory for archived tasks and projects.")

(defconst my/library-dir (expand-file-name "Library/" my/root-dir)
  "Static resources directory (Books, Music, PDFs).")

(defconst my/ai-engineering-dir (expand-file-name "Resources/AI-engineering/" my/library-dir)
  "Directory for my AI engineering resources")



(defconst my/development-dir (expand-file-name "Development/" my/root-dir)
  "Development projects and code.")

(defconst my/export-output-dir (expand-file-name "output/" my/brain-dir)
  "Centralized directory for all Org exports.")

;;; GTD Files

(defconst my/inbox-file (expand-file-name "inbox.org" my/gtd-dir)
  "Inbox — all new captures land here.")

(defconst my/next-file (expand-file-name "next.org" my/gtd-dir)
  "Standalone next actions not tied to a project.")

(defconst my/gtd-projects-file (expand-file-name "projects.org" my/gtd-dir)
  "Active projects.")

(defconst my/someday-file (expand-file-name "someday.org" my/gtd-dir)
  "Someday/Maybe list.")

(defconst my/waiting-file (expand-file-name "waiting.org" my/gtd-dir)
  "Tasks waiting on someone else.")

(defconst my/rituals-file (expand-file-name "rituals.org" my/gtd-dir)
  "Recurring habits and rituals.")

(defconst my/discarded-file (expand-file-name "discarded.org" my/gtd-dir)
  "Discarded/cancelled tasks.")

(defconst my/gcal-file (expand-file-name "gcal.org" my/gtd-dir)
  "Google Calendar sync file.")

;;; Dashboard Files

(defconst my/tasks-file (expand-file-name "tasks.org" my/dashboard-dir)
  "Actionable tasks file (used by org-timeblock).")

(defconst my/rough-notes-file (expand-file-name "fleeting_notes.org" my/dashboard-dir)
  "Quick capture scratchpad for unprocessed thoughts.")

(defconst my/logbook-file (expand-file-name "log-book.org" my/dashboard-dir)
  "Time tracking and clock-in log.")

(defconst my/shortcuts-file (expand-file-name "shortcuts_in_emacs.org" my/dashboard-dir)
  "Emacs shortcuts and keybinding reference.")

(defconst my/focus-file (expand-file-name "focus-blocks.org" my/dashboard-dir)
  "Log of completed focus-timer blocks (~/focus-timer), one CLOCK entry each.
Written by the focus-timer server, not Emacs; included in `org-agenda-files'
so total focus hours show up via the agenda's clock report (`R').")

;;; Private Files

(defconst my/journal-file (expand-file-name "Journal.org" my/private-dir)
  "Daily journal (private, Git-ignored).")

;;; Phone Inbox

(defconst my/phone-inbox-dir (expand-file-name "phone_inbox" my/brain-dir)
  "Phone inbox directory for captures.")

(defconst my/dictation-dir (expand-file-name "dictations/" my/phone-inbox-dir)
  "Dictations directory from phone.")

(defconst my/phone-inbox (expand-file-name "inbox/inbox.org" my/phone-inbox-dir)
  "Inbox file from phone captures.")

;;; Personal

(defconst my/job-applications-dir "~/Stillness/Personal/Applications/"
  "Job applications directory.")

(defconst my/job-applications-file (expand-file-name "applications.org" my/job-applications-dir)
  "Job applications tracking file.")

(defconst my/cv-library-dir (expand-file-name "automate-resume/outputs/"   my/job-applications-dir)
  "Job applications CV library directory.")
;;; Development

(defconst my/leetcode-dir (expand-file-name "NeetCode/" my/development-dir)
  "LeetCode/NeetCode solutions and notes.")

(defconst my/leetcode-index-file (expand-file-name "Index.org" my/leetcode-dir)
  "Auto-generated index of LeetCode problems.")

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

(provide 'paths)
;;; paths.el ends here
