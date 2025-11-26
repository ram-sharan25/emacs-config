;;; paths.el --- Central location for all file and directory paths -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines the Single Source of Truth for all paths in the Emacs config.
;; All paths use the Brain structure: ~/Stillness/Brain/{Public,Dashboard,Private}
;; Use standardized `my/` prefix for all variables.
;; Usage: (require 'paths) in other modules, then reference these variables.

;;; Code:

;;; ============================================================================
;;; ROOT DIRECTORIES
;;; ============================================================================

(defconst my/root-dir "~/Stillness/"
  "Root directory of the Stillness system.")

(defconst my/brain-dir (expand-file-name "Brain/" my/root-dir)
  "Brain directory containing all org-roam notes (Public + Dashboard + Private).")

(defconst my/public-dir (expand-file-name "Public/" my/brain-dir)
  "Public knowledge notes directory (Git-synced).")

(defconst my/data-dir (expand-file-name "data/" my/brain-dir)
  "Central directory for all media assets (images, PDFs, etc).")

(defconst my/dashboard-dir (expand-file-name "Dashboard/" my/brain-dir)
  "Dashboard directory for PARA workflow (Projects, Tasks, etc).")

(defconst my/private-dir (expand-file-name "Private/" my/brain-dir)
  "Private notes directory (Git-ignored, sensitive content).")

(defconst my/library-dir (expand-file-name "Library/" my/root-dir)
  "Static resources directory (Books, Music, PDFs).")

(defconst my/development-dir (expand-file-name "Development/" my/root-dir)
  "Development projects and code.")

;;; ============================================================================
;;; DASHBOARD FILES - PARA System & Workflow
;;; ============================================================================

(defconst my/tasks-file (expand-file-name "tasks.org" my/dashboard-dir)
  "Actionable tasks file.")

(defconst my/areas-dir (expand-file-name "Areas/" my/brain-dir)
  "Directory for Area Hubs (Ongoing Responsibilities).")

(defconst my/resources-dir (expand-file-name "Resources/" my/brain-dir)
  "Directory for Resource Hubs (Topics, Books, Interests).")

(defconst my/projects-file (expand-file-name "projects.org" my/dashboard-dir)
  "High-level project definitions file.")

(defconst my/rough-notes-file (expand-file-name "rough_notes.org" my/dashboard-dir)
  "Quick capture scratchpad for unprocessed thoughts.")

(defconst my/notes-index-file (expand-file-name "notes_index.org" my/dashboard-dir)
  "Auto-generated index of notes.")

(defconst my/logbook-file (expand-file-name "log-book.org" my/dashboard-dir)
  "Time tracking and clock-in log.")

(defconst my/shortcuts-file (expand-file-name "shortcuts_in_emacs.org" my/dashboard-dir)
  "Emacs shortcuts and keybinding reference.")

;;; ============================================================================
;;; GOOGLE INTEGRATIONS - Calendar & Tasks
;;; ============================================================================

(defconst my/gcal-file (expand-file-name "gcal.org" my/dashboard-dir)
  "Google Calendar sync file.")

(defconst my/gtasks-dir (expand-file-name "gtasks/" my/dashboard-dir)
  "Google Tasks sync directory.")

(defconst my/gtasks-file (expand-file-name "GoogleTasks.org" my/gtasks-dir)
  "Google Tasks sync file.")

;;; ============================================================================
;;; PRIVATE FILES - Journal & Personal
;;; ============================================================================

(defconst my/journal-file (expand-file-name "Journal.org" my/private-dir)
  "Daily journal with structured entries (private, Git-ignored).")

(defconst my/diary-file (expand-file-name "Diary.org" my/private-dir)
  "Diary file for personal reflections.")

;;; ============================================================================
;;; DEVELOPMENT DIRECTORIES - Coding Projects
;;; ============================================================================

(defconst my/leetcode-dir (expand-file-name "NeetCode/" my/development-dir)
  "LeetCode/NeetCode problem solutions and notes.")

(defconst my/leetcode-index-file (expand-file-name "Index.org" my/leetcode-dir)
  "Auto-generated index of LeetCode problems.")

;;; ============================================================================
;;; ORG-ROAM CONFIGURATION
;;; ============================================================================

(defconst my/org-roam-directory my/brain-dir
  "Org-roam root directory (points to Brain for all notes).")

;;; ============================================================================
;;; ORG AGENDA CONFIGURATION
;;; ============================================================================

(defconst my/org-agenda-files
  (list my/tasks-file
        my/rough-notes-file
        my/gcal-file
        my/gtasks-file)
  "List of files for org-agenda.")

;;; ============================================================================
;;; BACKUP/SYNC CONFIGURATION
;;; ============================================================================

(defconst my/rclone-sync-dir my/brain-dir
  "Directory to sync via Rclone (entire Brain directory).")

(defconst my/archive-dir (expand-file-name "Archives/" my/brain-dir)
  "Directory for archived tasks and projects.")

(defconst my/archive-file-template (expand-file-name "%s_archive.org" my/archive-dir)
  "Template for archive filenames (e.g. tasks_archive.org).")

(defconst my/export-output-dir (expand-file-name "output/" my/brain-dir)
  "Centralized directory for all Org exports.")

(provide 'paths)
;;; paths.el ends here
