# Cognitive Scaffolding: The Design Document

## 1. The User Profile & Problem Statement

### The User
*   **Context**: A graduate student/researcher working on complex technical topics (Thesis, Cloud, NASA Data).
*   **Cognitive Profile**: Identifies with ADHD traits.
	*   **Strengths**: "Thinking in Writing" (Walkthroughs), deep focus when engaged.
	*   **Challenges**: Jumping to conclusions, forgetting tasks, overlooking details, getting scattered, "Knowledge Hoarding" (notes rotting in folders).
*   **Current Workflow**:
	*   **Timeblocking**: Picks tasks in the morning.
	*   **Walkthroughs**: Writes detailed "stream of consciousness" logs inside tasks (`Tasks.org`).
	*   **The Friction**: "Tasks and Notes get tangled." Valuable insights ("The Gold") are buried inside the ephemeral task logs ("The Dirt"). When the task is closed, the knowledge is lost.

### The Goal
To build a **"Cognitive Scaffolding"** (an external brain) that:
1.  **Enforces Discipline**: Prevents jumping to conclusions (Start Protocol).
2.  **Untangles Knowledge**: Extracts permanent insights from daily work without breaking flow.
3.  **Links Everything**: Ensures no note is ever lost or orphaned.
4.  **Respects the Current Flow**: Is an incremental upgrade, not a total rewrite.

---

## 2. The Conceptual Evolution

We moved through several models to arrive at the final design:

1.  **The "Logbook" Misunderstanding**: Initially thought `LogBook.org` was for journaling.
	*   *Correction*: User logs thoughts *directly inside the Task* in `Tasks.org`. This is "Thinking in Writing."
2.  **The "Project" Confusion**: User had broad categories like "Thesis" marked as Projects.
	*   *Refinement*: Adopted **PARA**. "Thesis" is an **Area** (Ongoing). "Write Proposal" is a **Project** (Finite).
3.  **The "Tangle" Solution**: Instead of forcing the user to stop and switch contexts, we introduced the **Extraction Protocol**.
	*   *Mechanism*: Write in the task -> Highlight Insight -> Capture to Note -> Link back automatically.

---

## 3. The Proposed System: PARA + Zettelkasten + BuJo

We are combining three standard methodologies to fit the user's specific needs.

### A. Structure (PARA)
*   **Areas (The Hubs)**: Ongoing responsibilities (e.g., `Impact`, `Thesis`, `Emacs`).
	*   *Implementation*: `Notes/Areas/Impact.org`.
	*   *Function*: A dashboard showing all active Projects and Notes for that Area.
*   **Projects (The Drivers)**: Finite goals with deadlines (e.g., `Fix Deployment`, `Write Chapter 1`).
	*   *Implementation*: Headings in `Tasks.org` with `:PROJECT: Impact` property.
*   **Resources (The Library)**: Topics of interest (e.g., `Docker`, `STAC`, `Zettelkasten`).
	*   *Implementation*: Atomic notes in `Notes/`.

### B. Capture (The Templates)
1.  **Project Task (`p`)**:
	*   *Prompt*: "Which Area/Project?"
	*   *Features*: Auto-inserts the **Start Protocol** checklist.
2.  **Reference (`r`)**:
	*   *Purpose*: Capturing external inputs (Papers, Articles, Meetings).
	*   *Location*: `Notes/References/`.
3.  **Zettel (`z`)**:
	*   *Purpose*: Capturing internal insights (The "Spark").
	*   *Location*: `Notes/`.
	*   *Features*: **The Umbilical Cord** (`%a`) - automatically links back to the Task/Context it was captured from.

### C. The Protocols (The Scaffolding)
1.  **Start Protocol**: Before starting a task, define Outcome, First Step, and Assumptions.
2.  **Extraction Protocol**: While working, if you write a "Keeper," extract it immediately to a Zettel.
3.  **Shutdown Protocol**: Review the day, refile loose notes, ensure "Gold" is extracted.

---

## 4. The "Day in the Life" (Detailed Example)

**Scenario**: You are starting to think about a new topic: **"Using AI for STAC Data Ingestion."**

### Step 1: The Setup (Defining the Work)
*   **Thought**: "I need to research how AI can help with STAC."
*   **Action**: `org-capture` -> `p` (Project Task).
*   **The Template Asks**:
	*   *Task Name*: "Research AI for STAC".
	*   *Project/Area*: Select `Thesis` (Area).
*   **The Result**: A new entry in `Tasks.org`.
	```org
	* TODO Research AI for STAC
	  :PROPERTIES:
	  :PROJECT: Thesis
	  :END:
	  ** Protocol
	  - [ ] Outcome: A list of 3 viable AI models for STAC.
	  - [ ] First Step: Search Google Scholar for "LLM STAC".
	  - [ ] Assumptions: That LLMs can understand JSON schemas.
	```

### Step 2: The Work (Thinking in Writing)
*   **Action**: You Clock In (`C-c C-x C-i`).
*   **Action**: You start your **Walkthrough** (your "Thinking in Writing") directly under the task.
	```org
	** Walkthrough
	- 09:00: Starting search. Found a paper by Smith et al.
	- Reading the abstract... it says LLMs struggle with large JSONs.
	- Wait, they suggest using "RAG" to split the schema.
	- This is important! RAG is the key here.
	```

### Step 3: The Extraction (Untangling the Gold)
*   **The Spark**: You realize "RAG is the key for JSON schemas" is a permanent insight. It shouldn't die in this task.
*   **Action**: You highlight that sentence.
*   **Command**: `M-x org-roam-capture` -> `z` (Zettel).
*   **The Template Asks**:
	*   *Title*: "RAG for Large JSON Schemas".
	*   *Tags*: `:AI:STAC:`.
*   **The Template Inserts**:
	```org
	#+title: RAG for Large JSON Schemas
	#+filetags: :AI:STAC:

	* Refers to: [[id:task-id][Research AI for STAC]]  <-- The Umbilical Cord
	```
*   **You Add Context**: You type `[[` and link it to your existing `[[id:stac][STAC]]` note.
*   **Result**: You have a permanent note. You return to your Task and keep working.

### Step 4: The Conclusion (Finishing)
*   **Action**: You finish the research.
*   **Review**: You look at your Walkthrough. Did you miss any other insights?
	*   "Oh, I also found that Gemini 1.5 has a large context window." -> Capture another Zettel (`z`).
*   **Action**: Mark Task as `DONE`.
*   **Result**:
	*   The **Task** is closed (History).
	*   The **Notes** ("RAG for JSON", "Gemini Context") are safe in `Notes/` (Asset).
	*   The **Area Map** (`Thesis.org`) now shows these new notes in its backlinks.

### Step 5: The Future (Retrieval)
*   Two weeks later, you are writing your Thesis Proposal.
*   You open `Notes/Areas/Thesis.org`.
*   You see a list of "Recent Insights":
	*   `[[RAG for Large JSON Schemas]]`
	*   `[[Gemini Context Window]]`
*   You have exactly what you need to write.

---

## 5. The Physical Architecture (Where things sit)

### The Folder Structure (PARA)
```text
~/Stillness/Personal/
├── Writings/
│   ├── Tasks.org          <-- PROJECTS & TASKS live here.
│   │                          (This is where you "Think in Writing" / Walkthroughs)
│   └── RoughNotes.org     <-- INBOX (Fleeting thoughts)
│
└── Notes/                 <-- RESOURCES (Your Knowledge Base)
	├── Areas/             <-- AREAS (The Hubs)
	│   ├── Thesis.org     (Dashboard for Thesis)
	│   └── Impact.org     (Dashboard for Work)
	│
	├── References/        <-- INPUTS (External Knowledge)
	│   ├── Paper_X.org    (Notes on a PDF)
	│   └── Meeting_Y.org  (Notes from a meeting)
	│
	├── Docker.org         <-- ZETTELS (Atomic Insights)
	├── STAC.org
	└── ...
```

### Q&A: Where does it go?

1.  **Where do Projects live?**
	*   Inside `Writings/Tasks.org`.
	*   Example: `* TODO Fix Deployment :PROJECT:Impact:`

2.  **Where does "Thinking in Writing" happen?**
	*   **Inside the Task** in `Tasks.org`.
	*   You do your "Walkthrough" there. It is the "Workbench."

3.  **Where do Actual Notes sit?**
	*   In `Notes/`.
	*   When you extract a "Gold" thought from your Workbench, it moves to `Notes/STAC.org`.

4.  **Where do Areas sit?**
	*   In `Notes/Areas/`.
	*   These are just "Maps" that link to your Tasks and Notes.
---

## 6. The Linking Logic (The Glue)

You asked: *"How will they be linked?"*

We use 3 types of glue:

### 1. Properties (Task -> Project/Area)
In `Tasks.org`, every task knows where it belongs using **Properties**.
```org
* TODO Research AI for STAC
  :PROPERTIES:
  :AREA: Thesis        <-- Belongs to Thesis Area
  :PROJECT: AI Research <-- Belongs to AI Research Project
  :ID: 12345           <-- Unique ID for linking
  :END:
```

### 2. The Umbilical Cord (Note -> Task)
When you extract a Note (`z`), it automatically links back to the Task's ID.
```org
#+title: RAG for JSON
* Refers to: [[id:12345][Research AI for STAC]]
```
*   *Result*: If you are in the Note, you can click back to the Task.

### 3. The Dashboard (Area -> Everything)
In `Notes/Areas/Thesis.org`, we use **Dynamic Blocks** to pull everything together.
```org
#+title: Thesis Area

* Active Projects
#+BEGIN: columnview :id global :match "AREA=\"Thesis\""
(List of all Tasks with :AREA: Thesis)
#+END:

* Knowledge Base
(List of all Notes that link to [[id:thesis][Thesis]])
```

**Summary**:
-   **Tasks** point Up (to Areas).
-   **Notes** point Back (to Tasks).
-   **Areas** look Down (at everything).

---

## 7. The Safety Net (No Orphans Allowed)

You asked: *"So there won't be any dangling tasks? And there won't be any note without a task?"*

**The System Rules:**

1.  **Rule 1: No Task Left Behind**
	*   The **Capture Template (`p`)** *forces* you to pick an Area/Project.
	*   You physically cannot create a task without assigning it a home.
	*   *Safety*: If you try, it defaults to a "Dump" project (which you must clear weekly).

2.  **Rule 2: No Note Left Alone**
	*   **Scenario A (Working)**: You extract a note from a Task. The template links it to the Task. (Secure).
	*   **Scenario B (Random Idea)**: You have a random idea in the shower. You capture it to `RoughNotes` (Inbox).
		*   *The Protocol*: You **MUST** process your Inbox.
		*   You must either link it to an **Area** (e.g., `[[id:thesis][Thesis]]`) OR delete it.
	*   *Result*: A note is either linked to a Task (Origin) or an Area (Topic). It never floats.

**The Guarantee**:
If you follow the **Capture Templates** and the **Inbox Protocol**, it is mathematically impossible to have an orphan.

---

## 8. The Project Anatomy (Where Projects sit)

You asked: *"How does a project structure work and where does it sit?"*

Projects are **Virtual Containers** that live inside your `Tasks.org` (or `Projects.org`). They are not separate files (usually).

### The Physical Location
Inside `~/Stillness/Personal/Writings/Tasks.org`:

```org
* Active Projects                    <-- Top Level Category
  * Impact                           <-- Area (Group)
	* Fix Deployment                 <-- PROJECT (The Container)
	  :PROPERTIES:
	  :ID: proj-123
	  :END:
	  ** TODO Check Logs             <-- Task 1
	  ** TODO Restart Server         <-- Task 2
	  ** Walkthrough                 <-- Your Thinking Log
		 - "Found a bug..."
```

### How it Links
1.  **The Project** is just a Heading.
2.  **The Tasks** sit inside it.
3.  **The Notes** (Zettels) link back to the Project ID (`proj-123`).

### Why not a file?
If we made a file for every small project (`Fix Deployment.org`), you would have 1000s of files.
By keeping Projects as **Headings in Tasks.org**, you can archive the whole tree when done.

**Exception**: If a Project is HUGE (e.g., "Write Thesis"), it acts like an Area and gets its own Hub File in `Notes/Areas/Thesis.org`.

---

## 9. The Creation Workflow (How to birth a Project)

You asked: *"How and when is a project created? And how is it linked?"*

We will introduce a **New Template (`P`)** specifically for Projects.

### 1. Creating an Area (Rare)
*   **Action**: Manually add a top-level heading in `Projects.org`.
*   **Example**: `* Thesis`, `* Impact`, `* Personal`.
*   *Frequency*: Once a month/year.

### 2. Creating a Project (Occasional)
*   **Action**: `org-capture` -> `P` (New Project).
*   **Prompt**: "Project Name?" -> "Fix Deployment".
*   **Prompt**: "Which Area?" -> Select `Impact` (from `Projects.org`).
*   **Result**:
	*   It creates a new heading in `Tasks.org`:
		```org
		* PROJ Fix Deployment
		  :PROPERTIES:
		  :AREA: Impact
		  :ID: proj-123
		  :END:
		```
*   *Frequency*: When you start a new goal.

### 3. Creating a Task (Daily)
*   **Action**: `org-capture` -> `p` (Task).
*   **Prompt**: "Task Name?" -> "Check Logs".
*   **Prompt**: "Which Project?" -> Select `Fix Deployment` (from `Tasks.org`).
*   **Result**:
	*   It nests the task *under* the Project in `Tasks.org`:
		```org
		* PROJ Fix Deployment
		  ** TODO Check Logs
		```

**The Link**:
*   The **Project** links to the **Area** via the `:AREA:` property.
*   The **Task** links to the **Project** by being physically nested inside it.
--

## 9. The Creation Workflow (How to birth a Project)

You asked: *"How and when is a project created? And how is it linked?"*

We will introduce a **New Template (`P`)** specifically for Projects.

### 1. Creating an Area (Rare)
*   **Action**: Add a heading in `Writings/Areas.org`.
*   **Example**: `* Thesis`, `* Impact`.
*   **Action**: Create the dashboard in `Notes/Areas/Thesis.org`.
*   *Frequency*: Once a month/year.

### 2. Creating a Project (Occasional)
*   **Action**: `org-capture` -> `P` (New Project).
*   **Prompt**: "Project Name?" -> "Fix Deployment".
*   **Prompt**: "Which Area?" -> Select `Impact` (from `Areas.org`).
*   **Result**: Creates a heading in `Projects.org`:
		```org
		* PROJ Fix Deployment
		  :PROPERTIES:
		  :AREA: Impact
		  :ID: proj-123
		  :END:
		```
*   *Frequency*: When you start a new goal.

### 3. Creating a Task (Daily)
*   **Action**: `org-capture` -> `p` (Task).
*   **Prompt**: "Task Name?" -> "Check Logs".
*   **Prompt**: "Which Area/Project?" -> Select `Impact` or `Fix Deployment`.
*   **Result**: Creates a heading in `Tasks.org`:
		```org
		* TODO Check Logs
		  :PROPERTIES:
		  :AREA: Impact
		  :PROJECT: Fix Deployment
		  :ID: task-456
		  :END:
		```

**The Links**:
*   **Project** -> **Area**: Via `:AREA:` property.
*   **Task** -> **Project/Area**: Via `:PROJECT:` and `:AREA:` properties.
*   **Note** -> **Task**: Via the Umbilical Cord (`%a`).

---

## 10. The Daily Routine (When to do what)

### Morning (Planning)
1. **Open**: `Tasks.org`
2. **Review**: Look at tasks with `:AREA:` tags
3. **Timeblock**: Pick 3-5 tasks for the day
4. **Start Protocol**: For each major task, ensure the Protocol is filled:
   - Outcome defined?
   - First step clear?
   - Assumptions checked?

### During Work (Execution)
1. **Clock In**: `C-c C-x C-i` on the task
2. **Walkthrough**: Write your thoughts under `** Walkthrough`
3. **Extraction**: When you find "Gold":
   - Highlight the sentence
   - `M-x org-roam-capture` -> `z`
   - Link it back (automatic via `%a`)
   - Add concept links (manual via `org-roam-node-insert`)
4. **New Task**: If you discover a new todo:
   - `M-x org-capture` -> `p`
   - Assign to Area/Project
   - Continue current task

### End of Day (Shutdown Protocol)
1. **Clock Out**: `C-c C-x C-o`
2. **Review Walkthroughs**: Scan today's tasks
   - Did I learn something reusable?
   - If yes: Extract to Zettel (`z`)
3. **Process Inbox**: Open `RoughNotes.org`
   - Refile to Tasks (`p`)
   - Refile to Notes (`z`)
   - Delete junk
4. **Update Area Dashboards**: (Optional) Open `Notes/Areas/Thesis.org` to see your progress

---

## 11. Essential Org-roam Commands

| Command | Function | Purpose |
|---------|----------|---------|
| `org-roam-node-find` | Find and open a note | Navigate your knowledge base |
| `org-roam-node-insert` | Insert link to existing note | Create concept links (the "Add Context" step) |
| `org-roam-buffer-toggle` | Show backlinks sidebar | See what links TO the current note |
| `org-roam-capture` | Create a new note | Capture Zettels (`z`) or References (`r`) |
| `org-id-get-create` | Generate unique ID | Ensure every heading can be linked |

**Recommended Keybindings** (add to your config):
```elisp
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n b") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
```

---

## 12. The Reference Workflow (Processing External Inputs)

**Scenario**: You're reading a research paper: "Autonomous Agents for Data Pipelines" by Smith et al.

### Step 1: Capture the Reference
*   **Action**: `org-roam-capture` -> `r` (Reference)
*   **Template Asks**:
	*   *Title*: "Autonomous Agents for Data Pipelines"
	*   *Author*: "Smith et al."
	*   *URL*: "https://arxiv.org/..."
	*   *Date*: "2024-03-15"
*   **Result**: New file in `Notes/References/autonomous-agents-data-pipelines.org`:
	```org
	#+title: Autonomous Agents for Data Pipelines
	#+filetags: :reference:paper:

	* Metadata
	- Author: Smith et al.
	- Source: https://arxiv.org/...
	- Date: 2024-03-15
	- Related Area: [[id:thesis][Thesis]]

	* Summary
	(Your notes on the paper go here)

	* Key Insights
	- They use RAG for schema understanding
	- Performance improves 40% vs baseline
	```

### Step 2: Extract Zettels from the Reference
While reading the paper, you find insights:
*   **Action**: Highlight "They use RAG for schema understanding"
*   **Action**: `org-roam-capture` -> `z`
*   **Title**: "RAG for Schema Understanding"
*   **Result**: The Zettel links back to the Reference (via `%a`)

### Step 3: Link to Your Work
*   In the Reference note, add: `Related to: [[id:proj-ai-stac][AI for STAC Research]]`
*   Now when you view the Project, you'll see this paper in the backlinks

**The Flow**:
`Paper` -> `Reference Note` -> `Zettel` -> `Task` -> `Project` -> `Area`

---

## 13. The Area Dashboard (What it looks like)

**Example: `Notes/Areas/Impact.org`** (Your Work Area)

```org
#+title: Impact Area Dashboard
#+filetags: :area:

* Active Projects
- [[id:proj-mangrove][PROJ Mangrove Basemap Updates]]
- [[id:proj-emit][PROJ EMIT Layer Fixes]]
- [[id:proj-deployment][PROJ Fix Deployment Pipeline]]

* Recent Tasks (This Week)
#+BEGIN: columnview :id global :match "AREA=\"Impact\"" :maxlevel 2
| Task | Status | Project |
|------|--------|---------|
| Check Logs | TODO | Fix Deployment |
| Update Basemap | DONE | Mangrove |
#+END:

* Knowledge Base (Backlinks)
All notes that link to [[id:impact][Impact]]:
- [[id:docker-compose][Docker Compose Setup]]
- [[id:github-actions][GitHub Actions CI/CD]]
- [[id:mapbox-styles][Mapbox Custom Styles]]

* References
Papers/Meetings related to this area:
- [[id:ref-veda-architecture][VEDA Architecture Meeting Notes]]
```

**How it works:**
1. **Manual**: You add Projects to the list when you create them
2. **Automatic**: Tasks appear via the `columnview` dynamic block
3. **Automatic**: Notes appear in backlinks because they contain `[[id:impact][Impact]]`

---

## 14. Writing from Zettels (Composition Workflow)

You asked: *"What if I need to combine 2-3 ideas to write an article?"*

### The Concept: Structure Notes (Maps of Content)

When you want to write something (article, thesis chapter, presentation), you create a **Structure Note** that assembles your atomic Zettels.

**Example: Writing an article "How to Deploy Data Tools on AWS"**

### Step 1: Create the Structure Note
*   **Action**: `M-x org-roam-node-find` -> Create new: "Deploy Data Tools AWS (Article)"
*   **Location**: `Notes/deploy-data-tools-aws.org`
*   **Template**:
	```org
	#+title: Deploy Data Tools on AWS (Article)
	#+filetags: :structure:article:

	* Outline
	** Introduction
	- [[id:cloud-benefits][Why Cloud for Data Tools]]

	** Core Concepts
	- [[id:docker-compose][Docker Compose Setup]]
	- [[id:github-actions][GitHub Actions CI/CD]]
	- [[id:aws-ecr][AWS ECR for Container Storage]]

	** Implementation
	- [[id:terraform-aws][Terraform AWS Setup]]
	- [[id:lambda-deployment][Lambda for Event Triggers]]

	** Conclusion
	- [[id:cost-optimization][Cost Optimization Tips]]

	* Draft
	(You write the actual article here, pulling content from the linked Zettels above)
	```

### Step 2: Write the Article
You open each Zettel, copy the relevant parts, and weave them together in the "Draft" section.

### Step 3: The Result
*   The **Zettels** remain atomic (reusable).
*   The **Structure Note** is your composition (one-time use for this article).

**The Analogy**:
- Zettels = LEGO bricks (atomic, reusable)
- Structure Note = The castle you build (composed from bricks)

You don't change the bricks. You assemble them into new structures.

---

## 15. The Templates (Emacs Lisp Code)

### Template 1: Project (`P`)
```elisp
("P" "New Project" entry
 (file "~/Stillness/Personal/Writings/Projects.org")
 "* PROJ %^{Project Name}\n:PROPERTIES:\n:AREA: %^{Area|Thesis|Impact|Personal|SelfStudy}\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n\n** Goals\n- [ ] %?\n"
 :empty-lines 1)
```

### Template 2: Task (`p`)
```elisp
("p" "Task" entry
 (file "~/Stillness/Personal/Writings/Tasks.org")
 "* TODO %^{Task Name}\n:PROPERTIES:\n:AREA: %^{Area|Thesis|Impact|Personal|SelfStudy}\n:PROJECT: %^{Project}\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n\n** Protocol\n- [ ] Outcome: %^{What does 'done' look like?}\n- [ ] First Step: %^{What is the immediate next action?}\n- [ ] Assumptions: %^{What am I assuming?}\n\n** Walkthrough\n- %?\n"
 :empty-lines 1)
```

### Template 3: Zettel (`z`)
```elisp
("z" "Zettel" plain
 "%?"
 :if-new (file+head "~/Stillness/Personal/Notes/${slug}.org"
					"#+title: ${title}\n#+filetags: :%^{Tags}:\n\n* Refers to: %a\n\n")
 :unnarrowed t)
```

### Template 4: Reference (`r`)
```elisp
("r" "Reference" plain
 "%?"
 :if-new (file+head "~/Stillness/Personal/Notes/References/${slug}.org"
					"#+title: ${title}\n#+filetags: :reference:\n\n* Metadata\n- Author: %^{Author}\n- Source: %^{URL/Source}\n- Date: %^{Date}\n- Related Area: %^{Area}\n\n* Summary\n\n* Key Insights\n")
 :unnarrowed t)
```

### Template 5: Quick Capture (`i` - existing, keep as-is)
```elisp
("i" "Idea" entry
 (file "~/Stillness/Personal/Writings/RoughNotes.org")
 "* %^{Title} :IDEA:\n:PROPERTIES:\n:ID: %(org-id-new)\n:TIME: %U\n:END:\n- %?"
 :empty-lines 1)
```

---

## 16. Life Admin & Habits (The "Personal" Area)

You asked: *"How can I include daily chores, cooking, call friend, track habits?"*

### Solution: The "Personal" Area

Create a **Personal** area in `Areas.org`:
```org
* Personal
  :PROPERTIES:
  :ID: area-personal-123
  :END:
```

### For Daily Chores (Non-Project Tasks)
Use the **Task template (`p`)** but select "Personal" as the Area:
```org
* TODO Call Mom
  :PROPERTIES:
  :AREA: Personal
  :PROJECT: Relationships  (or leave empty)
  :ID: task-456
  :END:
```

### For Habits (Recurring)
Create a **Habits** section in `Tasks.org`:
```org
* Habits
  ** TODO Morning Exercise
	 SCHEDULED: <2025-11-24 Mon .+1d>
	 :PROPERTIES:
	 :AREA: Personal
	 :STYLE: habit
	 :END:

  ** TODO Call a Friend
	 SCHEDULED: <2025-11-24 Mon .+1w>
	 :PROPERTIES:
	 :AREA: Personal
	 :STYLE: habit
	 :END:
```

### The Rule:
- **Work/Study**: Go to `Impact`, `Thesis`, `SelfStudy` areas
- **Life Admin**: Go to `Personal` area
- **Habits**: Recurring tasks in `Tasks.org` with `:STYLE: habit`

This keeps your "Thinking Work" (Thesis, Impact) separate from "Life Admin" (cooking, calls).
