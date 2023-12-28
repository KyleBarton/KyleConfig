;; org-db.el
;; TODO turn into a package. At present, just playing around

;; Good start so far, but a few more things to figure out:

;; - There is a question of how (if at all) arbitrary edits can be
;;   handled. Should it be possible? If not, we'll have to define a
;;   lot of specific actions (e.g. change headline, change contents,
;;   etc)

;;; Import manually for now while you experiment and before packaging.
(use-package org-ml
  :ensure t)



(require 'org)
(require 'org-ml)

;; SQLITE BACKEND
;;
;; This is the section where sqlite gets used to store headlines
;; and data. Super simple right now

(defun org-db/select-matching-headline--temp (headline)
  "return exact headline match. Return value is a list where the
first element is the headline title, and the second is the full
element definition according to the org-element API"
  (let* ((raw-response (sqlite-select
			(sqlite-open "/Users/kylebarton/tmp/testdb.sqlite")
			"select * from headlines where title = ?"
			(list headline)
			nil))
	 ;; sqlite-select is going to produce a list of results but we
	 ;; know we're only getting one in this case.
	 (singleton-result (car raw-response))
	 (title-result (car singleton-result))
	 (data-result (cadr singleton-result)))
    
    (with-temp-buffer
      (insert data-result)
      (org-element-parse-buffer))))

(defun org-db/search-headlines-in-db (term)
  "Searches and returns headlines that match the search term. Here,
'match' just means that the `term' can be found as a substring in
any part of the headline."
  (let* ((raw-response (sqlite-select
			(sqlite-open "/Users/kylebarton/tmp/testdb.sqlite")
			"select * from headlines where title like ?;"
			(list
			 ;; Unfortunatey the API doesn't appear to
			 ;; interpolate '%' on its side, so I'm just
			 ;; concatenating the wildcards onto the
			 ;; search.
			 (concat "%" term "%"))
			nil))
	 (contents-list
	  (mapcar (lambda (x) (cadr x)) raw-response)))
    (print contents-list)))

(defun org-db/select-all-headline-data--temp ()
  "Select every headline and return each of them (as structured
data) as a list"
  (let* ((raw-response (sqlite-select
			(sqlite-open "/Users/kylebarton/tmp/testdb.sqlite")
			"select data from headlines;"
			nil
			nil)))
    raw-response))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Seed data from a buffer called "*plainorg*"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun org-db/sync-from-buffer ()
  "Sync from '*plainorg*'"
  (interactive)
  (org-db/delete-the-data)
  (org-db/seed-data--temp))

(defun org-db/delete-the-data ()
  "delete the data"
  (sqlite-execute
   (sqlite-open "/Users/kylebarton/tmp/testdb.sqlite")
   "delete from headlines;"
   nil))

;; Keeping this around until I can figure out the control plane. All
;; data plane should go through db now though.
(defun org-db/seed-data--temp ()
  "Hacky way to seed the data don't keep"
  (let*
      ((structure (with-current-buffer "*plainorg*"
		    (org-element-parse-buffer)))
       (top-level-els (nthcdr 2 structure)))
    (mapcar
     (lambda (el)
       (sqlite-execute
	(sqlite-open "/Users/kylebarton/tmp/testdb.sqlite")
	"insert into headlines (title, data) values (?, ?)"
	;; `((org-element-property :raw-value el)
	;;   el)
	(list
	 (org-element-property :raw-value el)
	 (org-element-interpret-data el))))
     top-level-els)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup org-db()
  "An org system based on external storage rather than files."
  :group 'org)

(defcustom org-db/org-db-buffer-name "*ORGDB*"
  "Name of the buffer in which org-db queries will be made"
  :type 'string
  :group 'org-db)


(defun org-db ()
  "Start up org db mode in its own special buffer"
  (interactive)
  (switch-to-buffer
   (get-buffer-create org-db/org-db-buffer-name))
  (org-db-mode))


(defun org-db/display-all--temp--db ()
  "Display all headlines in the org db"
  (interactive)
  (let
      ((source-structure (org-db/select-all-headline-data--temp)))
    (org-db/display--temp source-structure)))

(defun org-db/display--temp (structure)
  "Display the org structure in the org-db buffer"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (org-element-interpret-data structure))))

(defun org-db/exec-at-top-level-headline (fn)
  "Executes the function `fn' after moving point to the top-level
headline."
  (save-excursion
    (condition-case nil
	(outline-up-heading 10000)
      (error (message "Already at top level heading")))
    (funcall fn)))

(defun org-db/debug-show-current-headline ()
  "Print the current headline as both content and structure"
  (interactive)
  ;; (save-excursion
  ;;   (outline-up-heading 10000)
  ;;   (print (org-element-at-point)))
  (org-db/exec-at-top-level-headline
   (lambda () (let* ((element (org-element-at-point))
		     (structure (org-element-parse-buffer))
		     (structure-elements (org-element-contents structure))
		     ;; (nothing-doing (print element))
		     ;; (nothing-else (print structure))
		     (full-element
		      (org-element-map
			  structure
			  'headline
			(lambda (el)
			  (progn
			    ;; (message "hi")
			    ;; (print el)
			    (when
			      (and
			       (= (org-element-property :begin el)
				  (org-element-property :begin element))
			       (= (org-element-property :end el)
				  (org-element-property :end element)))
			      (identity el))))
			nil
			t))
		     (expanded (org-element-interpret-data full-element)))
		(message "full element:")
		(print full-element)
		(message "expanded:")
		(print expanded)
		(message "done")))))

(defun org-db/search-headlines--db (term)
  (interactive "M")
  (let
      ((structure (org-db/select-matching-headline--temp term)))
    (org-db/display--temp structure)))

(defun org-db/search-headlines (term)
  (interactive "M")
  (let
      ((structure (org-db/search-headlines-in-db term)))
    (org-db/display--temp structure)))

(defun org-db/kill--temp ()
  "Erase the org-db buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun org-db/exit--temp ()
  "Leave org-db mode"
  (interactive)
  (kill-current-buffer))

(defvar-keymap org-db-mode-map
  :doc "Local keymap for org-db mode"
  :parent org-mode-map)

;; ok new idea: don't do the buffer shit here, define a wrapping
;; function that does that.
(define-derived-mode org-db-mode org-mode "Org DB"
  "Major mode for handling org db queries"
  :interactive nil
  (use-local-map org-db-mode-map)
  (define-key org-db-mode-map (kbd "a") #'org-db/display-all--temp--db)
  (define-key org-db-mode-map (kbd "s") #'org-db/search-headlines--db)
  (define-key org-db-mode-map (kbd "f") #'org-db/search-headlines)
  (define-key org-db-mode-map (kbd "x") #'org-db/kill--temp)
  (define-key org-db-mode-map (kbd "q") #'org-db/exit--temp)
  (define-key org-db-mode-map (kbd "d") #'org-db/debug-show-current-headline)
  (setq buffer-read-only nil))
