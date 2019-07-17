(add-to-list 'org-capture-templates
	     '("h" "Health management module") t)

(add-to-list 'org-capture-templates
	     `("hj" "Add a new journal" plain
	       (file+datetree "./health_management/gratitude-journal.org.gpg")
	       ,(concat
		 "   :PROPERTIES:\n"
		 "   :Mood: %^{Today's mood|😀|🙂|😐|😞|😩}\n"
		 "   :Feelings: %(completing-read \"Feeling : \" (ref-completion \"/health_management/gratitude-journal.org.gpg\" t \"Feelings\") nil nil \"\")\n"
		 "   :Created-at: %U\n"
		 "   :Updated-at: %U\n"
		 "   :END:\n"
		 "   %^{Journal body}"
		 )) t)

(defun org-dblock-write:mood-matrix(params)
  "Report mood as a matrix with following indicators:
   Green: That day I'm happy.
   Yellow: That day I feel neutral.
   Red: That day I feel distress."
  (let ((MATCH t)
	(SCOPE '("./gratitude-journal.org.gpg"))
	(SKIP nil)
	(month_stats '()))
      (org-map-entries
       (lambda()
	 (when (org-entry-get nil "Mood")
	   (let* ((headline (org-entry-get nil "ITEM"))
		  (day_number (progn
				(string-match "^[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-\\([[:digit:]]\\{2\\}\\) .*$" headline)
				(string-to-number (match-string 1 headline))))
		  (day_mood (org-entry-get nil "Mood"))
		  (parent_headline (progn
				     (org-up-heading-safe)
				     (org-entry-get nil "ITEM")))
		  (month_name (progn
				(string-match "^\\([[:digit:]]\\{4\\}\\)-[[:digit:]]\\{2\\} \\([[:alpha:]]+\\)$" parent_headline)
				(concat (match-string 2 parent_headline) " " (match-string 1 parent_headline)))))
	     
	     (if (null (assoc month_name month_stats))
		 (let ((initial_assoc '())
		       (c 1))
		   (while (<= c 31)
		     (add-to-list 'initial_assoc (cons c nil) t)
		     (setq c (+ c 1)))
		   (add-to-list 'month_stats (cons month_name initial_assoc) t)))
	     
	     (setcdr (assoc month_name month_stats) (mapc (lambda(e)
							    (if (eq (car e) day_number)
								(setcdr e day_mood)))
							  (cdr (assoc month_name month_stats))))))

	 ) MATCH SCOPE SKIP)

      (let ((radical_face " ")
	    (happy_face " ")
	    (neutral_face " ")
	    (sad_face " ")
	    (awful_face " ")
	    (none_face " "))
	(put-text-property 0 1 'font-lock-face '(:background "darkgreen") radical_face)
	(put-text-property 0 1 'font-lock-face '(:background "limegreen") happy_face)
	(put-text-property 0 1 'font-lock-face '(:background "yellow") neutral_face)
	(put-text-property 0 1 'font-lock-face '(:background "orange") sad_face)
	(put-text-property 0 1 'font-lock-face '(:background "red") awful_face)
	(put-text-property 0 1 'font-lock-face '(:background "white") none_face)
	
	(dolist (m month_stats)
	  (insert (concat (car m) " "))
	  (dolist (d (cdr m))
	    (cond ((null (cdr d))
		   (insert none_face))
		  ((string-equal (cdr d) "😀")
		   (put-text-property 0 1 'help-echo (concat (number-to-string (car d)) " Pretty rad") radical_face)
		   (insert radical_face))
		  ((string-equal (cdr d) "🙂")
		   (put-text-property 0 1 'help-echo (concat (number-to-string (car d)) " Happy") happy_face)
		   (insert happy_face))
		  ((string-equal (cdr d) "😐")
		   (put-text-property 0 1 'help-echo (concat (number-to-string (car d)) " Mehh") neutral_face)
		   (insert neutral_face))
		  ((string-equal (cdr d) "😞")
		   (put-text-property 0 1 'help-echo (concat (number-to-string (car d)) " Sad") sad_face)
		   (insert sad_face))
		  ((string-equal (cdr d) "😩")
		   (put-text-property 0 1 'help-echo (concat (number-to-string (car d)) " Awful") awful_face)
		   (insert awful_face))))
	  (insert "\n")))))


(defun org-dblock-write:mood-statistics(params)
  "Report overall mood statistics"
  (let ((MATCH t)
	(SCOPE '("./gratitude-journal.org.gpg"))
	(SKIP nil)
	(mood_counts '())
	(total 0))
    (org-map-entries
     (lambda()
       (when (not (null (org-entry-get nil "Mood")))
	   (if (not (null (assoc (org-entry-get nil "Mood") mood_counts)))
	       (setcdr (assoc (org-entry-get nil "Mood") mood_counts)
		       (+ (cdr (assoc (org-entry-get nil "Mood") mood_counts)) 1))
	     (add-to-list 'mood_counts (cons (org-entry-get nil "Mood") 1.00) t))
	   (setq total (+ total 1)))
       ) MATCH SCOPE SKIP)
    (org-table-create "2x1")
    (org-table-next-field)
    (insert "Mood")
    (org-table-next-field)
    (insert "%")
    (org-table-hline-and-move)
    (beginning-of-line)
    (dolist (m mood_counts)
      (org-table-next-field)
      (insert (car m))
      (org-table-next-field)
      (insert (number-to-string (* (/ (cdr m) total) 100)))
      (org-table-align))))
