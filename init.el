
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize) ; Note that using Package.el as main package manager

(load-theme 'zenburn t) ; Loading theme

(require 'epa-file)
(epa-file-enable) ; Enable GPG transparent encryption

(require 'cl) ; Import common lisp dialect
(require 'org) ; Initialize Org along side its modules see custom-set-variables for org-modules below the file
(require 'org-clock)

(setq org-link-frame-setup '((file . find-file))) ; Open link in the same window

(setq org-directory "D:/Desktop/personal-resources-planning") ; For relative path references

; Disable code evaluation security !
(setq org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil
      org-confirm-babel-evaluate nil)

;; BEGIN of Time management module

(defun org-dblock-write:get-latest-mission-statement(params)
  "Get the latest mission statement. Use with block view in a time management's index page."
  (let ((MATCH t)
	(SCOPE '("./mission-statements.org.gpg"))
	(SKIP nil)
	(latest-timestamp nil)
	(latest-quotes nil))
    (progn
      (org-map-entries
       (lambda()
	 (cond ((eq latest-timestamp nil)
		(setq latest-timestamp (parse-time-string (org-entry-get nil "Created-at")))
		(setq latest-quotes (org-entry-get nil "ITEM")))
	       ((time-less-p (parse-time-string (org-entry-get nil "Created-at")) latest-timestamp)
		(setq latest-timestamp (parse-time-string (org-entry-get nil "Created-at")))
		(setq latest-quotes (org-entry-get nil "ITEM")))
	       )
	 ) MATCH SCOPE SKIP)
      (insert "*" latest-quotes "*"))))

(defun ref-completion(ref-file &optional allow-multi-value)
  "Emulate foreign key behavior. Allow user to enter referenced properties from a parent or referenced file."
  (lexical-let ((possible-values nil))
    (setq available-values (let ((MATCH t)
				 (SCOPE (list (concat org-directory ref-file)))
				 (SKIP nil))
			     (org-map-entries
			      (lambda()
				(org-entry-get nil "ITEM")
				) MATCH SCOPE SKIP)))
    
    (if (not allow-multi-value)
	(symbol-value 'available-values)

      (lexical-let ((possible-values '("\""))
		    (all-values ""))
	(lambda (input predicate action)

	  (cond ((string-match "^$" input)
		 (setq possible-values '("\"")))
		((string-match "\\(.*\" \"\\|^\"\\)\\([^\"]*\\)\\(\"? ?\"?\\)$" input)
		 (if (test-completion (match-string 2 input) available-values)
		     (progn
		       (setq possible-values '("\" \""))
		       (setq all-values (concat (match-string 1 input) (match-string 2 input)))
		       (setq input (match-string 3 input)))
		   (setq possible-values available-values)
		   (setq all-values (match-string 1 input))
		   (setq input (match-string 2 input)))))
	  (cond ((null action)
		 (if (test-completion input possible-values predicate)
		     (concat all-values input)
		   (concat all-values (try-completion input possible-values predicate))))
		((eq action t)
		 (all-completions input possible-values predicate)))

	  )))
    ))

; Set templates for org-capture
(setq org-capture-templates nil)

(add-to-list 'org-capture-templates
	     `("m" "Add a new mission statement." entry
	       (file "./time_management/mission-statements.org.gpg")
	       ,(concat
		 "* %^{Statement}\n"
		 "  :PROPERTIES:\n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1) t)

(add-to-list 'org-capture-templates
	     `("l" "Add a new life area." entry
	       (file "./time_management/life-areas.org.gpg")
	       ,(concat
		 "* %^{Life area}\n"
		 "  :PROPERTIES:\n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1) t)

(add-to-list 'org-capture-templates
	     `("v" "Add a new life value." entry
	       (file "./time_management/values.org.gpg")
	       ,(concat
		 "* %^{Value}\n"
		 "  :PROPERTIES:\n"
		 "  :Description: %^{Description [optional]}\n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1) t)

(add-to-list 'org-capture-templates
	     `("r" "Add a new life role." entry
	       (file "./time_management/roles.org.gpg")
	       ,(concat
		 "* %^{Role}\n"
		 "  :PROPERTIES:\n"
		 "  :Description: %^{Description [optional]}\n"
		 "  :Life-area: %(completing-read \"Life-area [optional]: \" (ref-completion \"/time_management/life-areas.org.gpg\") nil nil \"\")\n"
		 "  :Values: %(completing-read \"Values [optional]: \" (ref-completion \"/time_management/values.org.gpg\" t) nil nil \"\")\n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1) t)

(add-to-list 'org-capture-templates
	     `("e" "Add a new life event" entry
	       (file+headline "./time_management/life-events.org.gpg" "Life events")
	       ,(concat
		 "* %^{Event}\n"
		 "  :PROPERTIES:\n"
		 "  :Role: %(completing-read \"Role [optional]: \" (ref-completion \"/time_management/roles.org.gpg\") nil nil \"\")\n"
		 "  :Description: %^{Description [optional]}\n"
		 "  :Start-date: %^{Start-date}u\n"
		 "  :End-date: %(if (yes-or-no-p \"Does an event finished yet?\") \"%^{End-date}u\" \"Present\") \n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1) t)

(add-to-list 'org-capture-templates
	     `("a" "Add a new ad-hoc task" entry
	       (file+datetree "./time_management/adhoc-tasks.org.gpg")
	       ,(concat
		 "* UNFINISHED %^{Task name} %^{Tags [optional]}G \n"
		 "  SCHEDULED: %^{Schedule}T %(if (yes-or-no-p \"Does a task has a deadline?\") \"DEADLINE: %^{Deadline}T\" \"\") \n"
		 "  :PROPERTIES:\n"
		 "  :Description: %^{Description [optional]}\n"
		 "  :Role:  %(completing-read \"Role [optional]: \" (ref-completion \"/time_management/roles.org.gpg\") nil nil \"\")\n"
		 "  :Effort: %^{Effort|0:10|0:30|1:00|1:30|2:00|2:30|3:00|3:30|4:00|4:30|5:00}\n"
		 "  :Impact: %^{Impact|1|2|3|4|5}\n"
		 "  :Risk: %^{Risk|low|high}\n"
		 "  :Complexity: %^{Complexity|low|high}\n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1) t)

(add-to-list 'org-capture-templates
	     `("i" "Add an interruption task" entry
	       (file+datetree "./time_management/adhoc-tasks.org.gpg")
	       ,(concat
		 "* UNFINISHED %^{Task name} :interruption: %^{Tags [optional]}G \n"
		 "  SCHEDULED: %^{Schedule}T %(if (yes-or-no-p \"Does a task has a deadline?\") \"DEADLINE: %^{Deadline}T\" \"\") \n"
		 "  :PROPERTIES:\n"
		 "  :Description: %^{Description [optional]}\n"
		 "  :Role:  %(completing-read \"Role [optional]: \" (ref-completion \"/time_management/roles.org.gpg\") nil nil \"\")\n"
		 "  :Effort: %^{Effort|0:10|0:30|1:00|1:30|2:00|2:30|3:00|3:30|4:00|4:30|5:00}\n"
		 "  :Impact: %^{Impact|1|2|3|4|5}\n"
		 "  :Risk: %^{Risk|low|high}\n"
		 "  :Complexity: %^{Complexity|low|high}\n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1 :clock-in t) t)

(add-to-list 'org-capture-templates
	     `("p" "Add a new project" entry
	       (function (lambda()
			   (find-file(read-file-name "Project file: " (concat org-directory "/time_management/projects") "new-project.org.gpg")) ))
	       ,(concat
		 "* OPENED %^{Project name} %^{Tags [optional]}G \n"
		 "  SCHEDULED: %^{Schedule}t DEADLINE: %^{Deadline}t \n"
		 "  :PROPERTIES:\n"
		 "  :Description: %^{Description [optional]}\n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1) t)

(add-to-list 'org-capture-templates
	     `("t" "Add a new project associated task" entry
	       (function (lambda()
			   (let ((file-name (read-file-name "Project file: " (concat org-directory "/time_management/projects") "" t))
				 (available-non-todo-subheadlines '()))
			     
			     (let ((MATCH t)
				   (SCOPE (list file-name))
				   (SKIP nil))
			       (org-map-entries
				(lambda()
				  (if (or (eq 1 (nth 0 (org-heading-components))) (null (nth 2 (org-heading-components))))
				      (add-to-list 'available-non-todo-subheadlines (org-entry-get nil "ITEM"))) ;Can only insert into non-todo subheadlines or directly into the project headline
				  ) MATCH SCOPE SKIP))
			     
			     (find-file file-name)
			     (goto-char (point-min))
			     (goto-char (re-search-forward (concat "^\*.*" (completing-read "Non-todo subheadline: " available-non-todo-subheadlines nil t))))
			     )))
	       ,(concat
		 "* UNFINISHED %^{Task name} %^{Tags [optional]}G \n"
		 "  %(if (yes-or-no-p \"Does a task has a schedule?\") \"SCHEDULED: %^{Schedule}T\" \"\") DEADLINE: %^{Deadline}T \n"
		 "  :PROPERTIES:\n"
		 "  :Description: %^{Description [optional]}\n"
		 "  :Role:  %(completing-read \"Role [optional]: \" (ref-completion \"/time_management/roles.org.gpg\") nil nil \"\")\n"
		 "  :Effort: %^{Effort|0:10|0:30|1:00|1:30|2:00|2:30|3:00|3:30|4:00|4:30|5:00}\n"
		 "  :Impact: %^{Impact|1|2|3|4|5}\n"
		 "  :Risk: %^{Risk|low|high}\n"
		 "  :Complexity: %^{Complexity|low|high}\n"
		 "  :Created-at: %U\n"
		 "  :Updated-at: %U\n"
		 "  :END:"
		 )
	       :empty-lines 1) t)


(setq org-agenda-files `(,(concat org-directory "/time_management/adhoc-tasks.org.gpg") ,(concat org-directory "/time_management/projects"))
      org-agenda-file-regexp "\\`[^.].*\\(\\.org\\|\\.org.gpg\\)\\'")

(setq org-agenda-log-mode-items '(closed clock state)
      org-agenda-start-with-log-mode t
      org-agenda-start-with-follow-mode t
      org-agenda-start-with-entry-text-mode nil
      org-agenda-log-mode-add-notes nil)

(setq org-log-state-notes-into-drawer t
      org-log-done nil    ; To avoid confusion, all DONE states don't get logged automatically. Leave them to ! and @ in todo keywords below
      org-log-repeat nil) ; Prevent messing up with Org-habit (Only "FINISHED" state in DONE states should be logged)

; Try to set more generalized keywords as possible. Can be overriden at the buffer or subtree level
(setq org-todo-keywords '((sequence "UNFINISHED(u)" "WAITING(w@)" "|" "FINISHED(f!)" "DELEGATED(d@)" "CANCELED(c@)") ; Available task states
			  (sequence "OPENED(o)" "ONGOING(g)" "|" "CLOSED(c)" "THROWN_AWAY(t@)"))) ; Availabel project states

(defun remap-org-agenda()
  (local-set-key (kbd "C-c C-z") (lambda()
				   (interactive)
				   (setcdr (nth 2 org-log-note-headings) "Comment on %t")
				   (org-agenda-add-note)
				   ))
  (local-set-key (kbd "C-c C-y") (lambda()
				   (interactive)
				   (setcdr (nth 2 org-log-note-headings) "Distraction on %t")
				   (org-agenda-add-note)
				   ))
  (local-set-key (kbd "C-c C-i") (lambda()
				   (interactive)
				   (org-capture nil "i")
				   ))
  )

(add-hook 'org-agenda-mode-hook 'remap-org-agenda)

(defun impact-sorting-strategy(a b)
  "Basic impact sorting strategy function which returns following order 1 < 2 < 3 < 4 < 5.
   Function returns 1 if a>b, -1 when a<b and nil when a=b"
  (let* ((a_pos (get-text-property 0 'org-marker a))
	 (b_pos (get-text-property 0 'org-marker b))
	 (a_i (string-to-number (org-entry-get a_pos "Impact")))
	 (b_i (string-to-number (org-entry-get b_pos "Impact"))))
    (if (not (eql a_i b_i))
	(if (> a_i b_i)
	    1
	  -1))))

(defun roi-sorting-strategy(a b)
  "Basic roi sorting strategy function which sort each entry by its yield value.
   Yield calculation based on this formula (impact / clocksum) / (clocksum) or (impact / estimated effort) / (estimated effort) when task isn't get clocked yet
   Both clocksum and effort are retrived as minutes normalized by 300.00
   Function returns 1 if a>b, -1 when a<b and nil when a=b

   Repeating tasks, only get clock sum for today only (clocksum_t), since the ROI shouldn't be diminished as the dates goes by.
   Time costs only increases within that day only.

   Non-repeating tasks, all clock sum counts. Most tasks are completed within a single occurence anyway. But some clocked task might get the WAITING status
   and need to be rescheduled later. Time costs continue to increase if user continue spend time on that task, until it's finished.

   If ROI goes to low, splitting a task into smaller subtasks is recommended."
  
  (let* ((a_pos (get-text-property 0 'org-marker a))
	 (b_pos (get-text-property 0 'org-marker b))
	 (a_i (string-to-number (org-entry-get a_pos "Impact")))
	 (b_i (string-to-number (org-entry-get b_pos "Impact")))

	 (a_c_minutes (with-current-buffer (marker-buffer a_pos)
			(goto-char (marker-position a_pos))
			(let ((clocksum_t (org-clock-sum-current-item (format-time-string "%Y-%m-%d 00:00")))
			      (clocksum (org-clock-sum-current-item))
			      (effort (parse-time-string (org-entry-get a_pos "Effort"))))
			  
			  (if (null (org-get-repeat)) ; Non-repeating tasks
			      (if (eq clocksum 0)
				  (+ (* (nth 2 effort) 60) (nth 1 effort)) ; Effort, Combine hours and minutes into minutes
				clocksum)
			    (if (eq clocksum_t 0) ; Repeating tasks
				(+ (* (nth 2 effort) 60) (nth 1 effort))
			      clocksum_t)))))

	 (b_c_minutes (with-current-buffer (marker-buffer b_pos)
			(goto-char (marker-position b_pos))
			(let ((clocksum_t (org-clock-sum-current-item (format-time-string "%Y-%m-%d 00:00")))
			      (clocksum (org-clock-sum-current-item))
			      (effort (parse-time-string (org-entry-get b_pos "Effort"))))
			  
			  (if (null (org-get-repeat)) ; Non-repeating tasks
			      (if (eq clocksum 0)
				  (+ (* (nth 2 effort) 60) (nth 1 effort)) ; Effort, Combine hours and minutes into minutes
				clocksum)
			    (if (eq clocksum_t 0) ; Repeating tasks
				(+ (* (nth 2 effort) 60) (nth 1 effort))
			      clocksum_t)))))
    
	 (a_yield (/ (/ a_i (/ a_c_minutes 300.00)) (/ a_c_minutes 300.00)))
	 (b_yield (/ (/ b_i (/ b_c_minutes 300.00)) (/ b_c_minutes 300.00))))
    
    (if (not (eql a_yield b_yield))
	(if (> a_yield b_yield)
	    1
	  -1))

    ))

(setq org-agenda-custom-commands
      '(("d" "Scheduled view - Daily"
	 ((agenda))
	 ((org-agenda-span 1)
	  (org-agenda-sorting-strategy '(time-up priority-down category-keep))))
	("w" "Scheduled view - Weekly"
	 ((agenda))
	 ((org-agenda-sorting-strategy '(time-up priority-down category-keep))))
	("x" "Global view - Deadline"
	 ((todo "UNFINISHED|WAITING"))
	 ((org-agenda-sorting-strategy '(deadline-up))
	  (org-agenda-overriding-columns-format "#+COLUMNS: %CATEGORY(Project) %DEADLINE(Deadline) %ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))
	("y" "Global view - ROI"
	 ((todo "UNFINISHED|WAITING"))
	 ((org-agenda-cmp-user-defined 'roi-sorting-strategy)
	  (org-agenda-sorting-strategy '(user-defined-down))
	  (org-agenda-overriding-columns-format "#+COLUMNS: %CATEGORY(Project) %Impact %Effort %ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))
	("z" "Global view - Impact"
	 ((todo "UNFINISHED|WAITING"))
	 ((org-agenda-cmp-user-defined 'impact-sorting-strategy)
	  (org-agenda-sorting-strategy '(user-defined-down))
	  (org-agenda-overriding-columns-format "#+COLUMNS: %CATEGORY(Project) %Impact %ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))))

(defun project-specific-agenda-block(dispatcher_key)
  (interactive "sAgenda dispatcher key: ")
  (let* ((proj_file (read-file-name "Project file: " (concat org-directory "/time_management/projects") "" t))
	 (org-agenda-files `(,proj_file))
	 (org-agenda-custom-commands '(("x" "Project view - Deadline"
					((todo "UNFINISHDED|WAITING|FINISHED|CANCELED|DELEGATED"))
					((org-agenda-sorting-strategy '(deadline-up))
					 (org-agenda-overriding-columns-format "#+COLUMNS: %DEADLINE(Deadline) %ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))
				       ("y" "Project view - ROI"
					((todo "UNFINISHED|WAITING|FINISHED|CANCELED|DELEGATED"))
					((org-agenda-cmp-user-defined 'roi-sorting-strategy)
					 (org-agenda-sorting-strategy '(user-defined-down))
					 (org-agenda-overriding-columns-format "#+COLUMNS: %Impact %Effort %ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))
				       ("z" "Project view - Impact"
					((todo "UNFINISHED|WAITING|FINISHED|CANCELED|DELEGATED"))
					((org-agenda-cmp-user-defined 'impact-sorting-strategy)
					 (org-agenda-sorting-strategy '(user-defined-down))
					 (org-agenda-overriding-columns-format "#+COLUMNS: %Impact %ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))
				       ("i" "Project view - Eisenhower's Impact/Urgency"
					((tags-todo "Impact>=3+DEADLINE<=\"<+2d>\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Do")))
					 (tags-todo "Impact>=3+DEADLINE>\"<+2d>\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Schedule")))
					 (tags-todo "Impact<3+DEADLINE<=\"<+2d>\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Delegate")))
					 (tags-todo "Impact<3+DEADLINE>\"<+2d>\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Eliminate"))))
					((org-agenda-cmp-user-defined 'roi-sorting-strategy)
					 (org-agenda-sorting-strategy '(user-defined-down))
					 (org-agenda-overriding-columns-format "#+COLUMNS: %Impact %DEADLINE(Deadline) %25ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))
				       ("j" "Project view - Impact/Effort"
					((tags-todo "Impact>=3+Effort<=\"1:30\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Quick wins")))
					 (tags-todo "Impact>=3+Effort>\"1:30\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Big bet")))
					 (tags-todo "Impact<3+Effort<=\"1:30\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Maybe")))
					 (tags-todo "Impact<3+Effort>\"1:30\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Time sinks"))))
					((org-agenda-cmp-user-defined 'roi-sorting-strategy)
					 (org-agenda-sorting-strategy '(user-defined-down))
					 (org-agenda-overriding-columns-format "#+COLUMNS: %Impact %Effort %25ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))
				       ("k" "Project view - Impact/Complexity"
					((tags-todo "Impact>=3+Complexity=\"low\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Necessary & Simple")))
					 (tags-todo "Impact>=3+Complexity=\"high\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Neccessary & Complex")))
					 (tags-todo "Impact<3+Complexity=\"low\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Unnecessary & Simple")))
					 (tags-todo "Impact<3+Complexity=\"high\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Unnecessary & Complex"))))
					((org-agenda-cmp-user-defined 'roi-sorting-strategy)
					 (org-agenda-sorting-strategy '(user-defined-down))
					 (org-agenda-overriding-columns-format "#+COLUMNS: %Impact %Complexity %25ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)")))
				       ("l" "Project view - Impact/Risk"
				        ((tags-todo "Impact>=3+Risk=\"low\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Do second")))
					 (tags-todo "Impact>=3+Risk=\"high\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Do first")))
					 (tags-todo "Impact<3+Risk=\"low\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Do last")))
					 (tags-todo "Impact<3+Risk=\"high\"/UNFINISHED|WAITING|FINISHED|DELEGATED|CANCELED"
						    ((org-agenda-overriding-header "Avoid"))))
					((org-agenda-cmp-user-defined 'roi-sorting-strategy)
					 (org-agenda-sorting-strategy '(user-defined-down))
					 (org-agenda-overriding-columns-format "#+COLUMNS: %Impact %Risk %25ITEM(Task name) %TODO(Status) %Role %Description %TAGS(Tags) %CLOCKSUM_T(Time spent)"))))))
    
    (command-execute (kbd (concat "M-x org-agenda RET " dispatcher_key " M-x org-agenda-columns")))

    ))

(defun org-dblock-write:timespent-by-role-report(params)
  (let ((role_hours '())
	(total_hours 0.0)
	(start_date (let* ((delta_start_date (plist-get params ':delta-start-date))
			   (operator (progn
				       (string-match "^<\\([+-]?[0-9]\\)\\([dmwy]\\)>$" delta_start_date)
				       (match-string 2 delta_start_date)))
			   (delta (string-to-number (match-string 1 delta_start_date)))
			   (current_time (decode-time)))
		      
		      (cond ((string-equal operator "y")
			     (setcar (cdr (cddddr current_time)) (+ (nth 5 current_time) delta)))
			    
			    ((string-equal operator "m")
			     (if (>= delta 0)
				 (setcar (cddddr current_time) (+ (nth 4 current_time) (mod delta 12)))
			       (setcar (cddddr current_time) (+ (nth 4 current_time) (mod delta -12))))
			     (setcar (cdr (cddddr current_time)) (+ (nth 5 current_time) (/ delta 12))))
			    
			    ((string-equal operator "w")
			     (setq delta (* delta 7))
			     (if (>= delta 0)
				 (setcar (cdddr current_time) (+ (nth 3 current_time) (mod delta 31)))
			       (setcar (cdddr current_time) (+ (nth 3 current_time) (mod delta -31))))
			     (setcar (cddddr current_time) (+ (nth 4 current_time) (/ delta 31))))
			    
			    ((string-equal operator "d")
			     (if (>= delta 0)
				 (setcar (cdddr current_time) (+ (nth 3 current_time) (mod delta 31)))
			       (setcar (cdddr current_time) (+ (nth 3 current_time) (mod delta -31))))
			     (setcar (cddddr current_time) (+ (nth 4 current_time) (/ delta 31)))))
		      
		      (format-time-string "%Y-%m-%d %H:%M" (apply 'encode-time current_time)))))
    
    (let ((MATCH t)
	  (SCOPE 'agenda)
	  (SKIP nil))
      (org-map-entries
       (lambda()
	 (if (not (null (org-entry-get nil "Role")))
	     
	     (if (assoc (org-entry-get nil "Role") role_hours)
		 (setcdr (assoc (org-entry-get nil "Role") role_hours) (+ (cdr (assoc (org-entry-get nil "Role") role_hours)) (org-clock-sum-current-item start_date)))
	       (add-to-list 'role_hours (cons (org-entry-get nil "Role") (org-clock-sum-current-item start_date))))

	   (setq total_hours (+ total_hours (org-clock-sum-current-item start_date))))
	 
	 ) MATCH SCOPE SKIP))
    
    (org-table-create "3x1")
    (org-table-next-field)
    (insert "Roles")
    (org-table-next-field)
    (insert "%")
    (org-table-next-field)
    (insert "Clock sum")
    (org-table-hline-and-move)
    (beginning-of-line)
    
    (dolist (role_hours_alist role_hours)
      (org-table-next-field)
      (insert (car role_hours_alist))
      (org-table-next-field)
      (if (= total_hours 0.0)
	  (insert "0")
	(insert (number-to-string (round (* (/ (cdr role_hours_alist) total_hours) 100)))))
      (org-table-next-field)
      (insert (concat (number-to-string (floor (/ (cdr role_hours_alist) 60))) ":" (number-to-string (mod (cdr role_hours_alist) 60))))
      (org-table-align)))
  )

(setq org-habit-graph-column 60)

;; END of Time management module

;; BEGIN of Knowledge management module

;; END of Knowledge management module

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" default)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages (quote (org-plus-contrib gnuplot zenburn-theme)))
 '(safe-local-variable-values (quote ((epa-file-enrypt-to "fieldfirst2012@gmail.com")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
