;; Not a package yet, must be loaded like an plain-old lisp file.
;; Today, it assumes you've already loaded configuration.org - it needs the keymaps to extend.


;; org-agenda appears to be its own package, but "packaged" with
;; org. Without explicitly loading it like this, I don't have access
;; to `org-agenda-mode-map'.
(use-package org-agenda
  :ensure org)

(setq kjb/agenda-inbox-relative-filepath "/Dropbox/Local/Org/gtd/in/")
(setq kjb/agenda-work-relative-filepath "/Dropbox/Local/Org/gtd/work/")

;; Helpful for an inbox capture
(setq kjb/default-inbox-file-name "inbox.org")
;; e.g. $HOME/dropbox/Local/Org/gtd/in/inbox.org
(setq kjb/default-inbox-file-path
      (concat (getenv "HOME") kjb/agenda-inbox-relative-filepath kjb/default-inbox-file-name))

(setq org-agenda-files
      (list
       (concat (getenv "HOME") kjb/agenda-inbox-relative-filepath)
       (concat (getenv "HOME") kjb/agenda-work-relative-filepath)))

;; Log state changes with a quick timestamp
(setq org-log-state-notes-into-drawer "LOGBOOK")

;; When agenda starts, store the window config and restore when you're
;; done
(setq org-agenda-restore-windows-after-quit t)

;; When agenda starts, make it the only window
(setq org-agenda-window-setup 'only-window)


;; Save all org buffers when you start, leave, or change org agenda
;; view
(advice-add 'org-agenda-exit :before #'org-save-all-org-buffers)
(advice-add 'org-agenda-quit :before #'org-save-all-org-buffers)
(advice-add 'org-agenda-append-agenda :before #'org-save-all-org-buffers)
(advice-add 'org-agenda :before #'org-save-all-org-buffers)

;; Only thing I've added is `tag-down' to the `todo' sorting
;; strategy. I don't know much about this variable and it should
;; probably be revisited at a later time.
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
       (todo category-down tag-down category-keep)
       (tags priority-down category-keep)
       (search category-keep)))

;; Auto-complete all tags across all agenda files
(setq org-complete-tags-always-offer-all-agenda-tags t)

(setq org-use-tag-inheritance t)

(setq org-agenda-custom-commands
      '(("N" "Agenda and all TODOs"
	 ((agenda #1="")
	  (alltodo #1#)))
	("p" "List all tasks orded by project (tag)"
	 ((tags "TAGS={.}"
		((org-agenda-sorting-strategy '(tag-down))))))
	("?" "Check the Someday/Maybe list"
	 ((todo "SOMEDAYMAYBE")))
	("i" "List your ins"
	 ((todo "IN")))
	("n" "Next Actions"
	 ((todo "NEXTACTION")))
	("w" "Waiting For"
	 ((todo "WAITINGFOR")))
	("t" "Next Actions & Waiting Fors"
	 ((todo "NEXTACTION")
	  (todo "WAITINGFOR")))))

;; Refile all in one go, into projects if needed.
(setq org-outline-path-complete-in-steps nil)
;; With the project-file-first setup, you can use some regexp patterns
;; for targets - e.g. "Actions" and "WaitingFor" and "ReferenceData"
;; Need to see how it looks in orgzly

;; Because of how `kjb/project-org-capture' works, I can rely by
;; convention on the fact that each file in my `work' gtd folder will
;; have a single heading each for "Next Actions", "Waiting For", and
;; "Someday Mayb". Setting refile targets here relies on this
;; convention to find places to refile from inbox (or across `work'
;; files).
(setq org-refile-targets '((org-agenda-files :regexp . "Next Actions")
			   (org-agenda-files :regexp . "Waiting For")
			   (org-agenda-files :regexp . "Someday Maybe")))


(defun kjb/get-agenda-file-category-values ()
  "Get the values of all CATEGORY properties in org agenda files."
  (let ((agenda-files (org-agenda-files))
        (category-values '()))
    (dolist (file agenda-files)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-element-map (org-element-parse-buffer) 'headline
           (lambda (headline)
             (when-let ((category (org-element-property :CATEGORY headline)))
               (push category category-values)))))))
    (delete-dups category-values)))


(defun kjb/set-category-property-cr ()
  "Set's the 'category' property of the org element using
`org-set-property'. Offers a completing-read prompt based on all
categories being used in org-agenda-files (see
`kjb/get-agenda-file-category-values' for implementation)"
  (interactive)
  (let ((category
	 (completing-read "Category: " (kjb/get-agenda-file-category-values))))
    (org-set-property "CATEGORY" category)))

(defun kjb/set-category-property-cr--agenda ()
  "This function assumes that the user is starting in the agenda
buffer, and should not be called outside of
`org-agenda-mode-map'. Goes to the agenda item at point, invokes
`kjb/set-category-property-cr', and then refocuses on the org
agenda buffer. It assumes that the name of the org agenda buffer
from the variable `org-agenda-buffer-name', which is not part of
org-agenda's public API, as far as I can tell.

Therefore, this function should be considered unstable."
  (interactive)
  (save-excursion
    ;; head to the element
    (org-agenda-goto)
    ;; set its category
    (kjb/set-category-property-cr)
    ;; go back to the agenda buffer
    (pop-to-buffer org-agenda-buffer-name)))


(defun kjb/completing-read-category-filter ()
  "Filters by category using completing-read. Basically has the same
effect as `org-agenda-filter-by-category', but that function is
limited by the fact that it must filter by the category on the
line at point."
  (interactive)
  (let ((category
	 (completing-read "Category: " (kjb/get-agenda-file-category-values))))
    (org-agenda-filter-apply
     (setq org-agenda-category-filter
	   (list (concat "+" category)))
     'category)))


(defvar kjb/project-org-capture-name nil
  "Desired friendly name for a new project. This can be a full sentence, e.g.
'Travel to New York'. The project tag & filename will be derived
from this value.  See the function `kjb/project-org-capture' as
well as the variables `kjb/project-org-capture-filename' and
`kjb/project-org-capture-tagname'")

(defvar kjb/project-org-capture-filename nil
  "Desired filename for capturing a new project. Should derive from
  user-input on `kjb/project-org-capture-name'. See
  `kjb/project-org-capture'")

(defvar kjb/project-org-capture-tagname nil
  "Desired tagname for capturing a new project. Should derive from
user-input on `kjb/project-org-capture-name'. See
`kjb/project-org-capture'")

;; new version!
;; still doesn't re-focus properly though...
(defun kjb/read-prompt-in-floating-minibuffer (prompt &optional win-name-param)
  (let* ((window-name (or win-name-param "floating-minibuffer"))
	 (entered-value (with-selected-frame 
			  (make-frame `((auto-raise . t)
					(name . ,window-name)
					(menu-bar-lines . 0)
					(undecorated . t)
					(minibuffer . only)
					(left . 0.5)
					(top . 0.5)
					(height . 0.3)
					(width . 0.35)))
			  (unwind-protect
			      (read-from-minibuffer prompt)
			    (delete-frame)))))
    (progn entered-value)))



(defun kjb/allowed-tag-chars-list ()
  "Returns all chars allowed in an org tag"
  ;; There has to be a more elegant way to do this.
  (string-to-list
   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@_"))

(defun kjb/filter-by-allowed-tag-chars (input)
  "Filters an input string to only contain characters allowed in an
org tag: Letterse, numbers, '@', and '_'. TODO Factor out
`filter-by-allowed-chars' from this function so that it can be
used more generally, like for allowed filename chars."
  (let*
      ((allowed-chars
	(kjb/allowed-tag-chars-list))
       (filtered-chars '()))
    (progn
      (dolist (char (string-to-list input))
	(when (seq-contains-p allowed-chars char)
	  (setq filtered-chars (append filtered-chars (list char)))))
      (mapconcat 'string filtered-chars))))


(defun kjb/project-tag-name-from-name (project-name)
  "Returns a properly formatted tag name for a project, given its
name. It follows the following guidelines:
- Capitalizes each word: 'my project' -> 'My Project'
- Replaces spaces with underscores: 'My Project' -> 'My_Project'
- Omits any characters other than numbers, letters, '_', and '@'
- Caps its length at X (TODO it does not do this yet)"
  (kjb/filter-by-allowed-tag-chars ;; TODO gotta figure that out.
   (string-replace
    " " "_" (capitalize project-name))))


(defun kjb/project-org-capture ()
  "Capture a new project in an org file. This is a wrapper around
org-capture that allows the user to specify a filename by setting
`kjb/project-org-capture-filename'."
  (interactive)
  (let ((project-name (kjb/read-prompt-in-floating-minibuffer "Project Name: ")))
    (progn
      (setq kjb/project-org-capture-name project-name)
      (setq kjb/project-org-capture-tagname
	    (kjb/project-tag-name-from-name project-name))
      (setq kjb/project-org-capture-filename
	    (concat
	     (getenv "HOME")
	     kjb/agenda-work-relative-filepath
	     (string-replace " " "-" project-name)
	     ".org"))
      (org-capture nil "p")
      (setq kjb/project-org-capture-filename nil)
      (setq kjb/project-org-capture-tagname nil)
      (setq kjb/project-org-capture-name nil))))

(setq org-capture-templates
      `(("p"
	 "New Project File. Should not be called directly. Instead call from `kjb/project-org-capture'"
	 entry
	 (file kjb/project-org-capture-filename)
	 "* %(symbol-value 'kjb/project-org-capture-name)   :%(symbol-value 'kjb/project-org-capture-tagname):%?
** Purpose/Principals
** Vision & Outcomes
** Brainstorming
** Organization
** Reference Data
** Future Work To Consider
*** Someday Maybe
** Actionables
*** Waiting For
*** Next Actions")
	("i"
	 "New Inbox Entry"
	 entry
	 (file kjb/default-inbox-file-path)
	 "* IN %?")))

(setq org-todo-keywords
      '((sequence "IN(i)" "|") ;; No timestamping in; nothing should live here long anyway
	(sequence "NEXTACTION(n!)" "DONE(d!)")
	(sequence "WAITINGFOR(w!)" "DONE(d!)")
	(sequence "SOMEDAYMAYBE(s!)" "DONE(d!)")))

;; This face seems kind of nice in doom dark theme. Can be adjusted
;; later. Mostly when it has the same face as DONE, it messes with me,
;; so it needs to be something different.
(setq org-todo-keyword-faces
      `(("SOMEDAYMAYBE" . org-cite)))

;; Keymaps


;; a map to extend with top-level for GTD commands
(defvar agenda-map (make-sparse-keymap)
  "Org Agenda commands (GTD-style)")
(define-key agenda-map (kbd "p") 'kjb/project-org-capture)
(define-key agenda-map (kbd "c") 'kjb/set-category-property-cr)
(define-key agenda-map (kbd "a") 'org-agenda)
(define-key agenda-map (kbd "f") 'org-refile)


(define-key top-level-map (kbd "a") agenda-map)

;; Extending the org agenda mode map
;; Refile with a simple "f"
(define-key org-agenda-mode-map (kbd "f") 'org-agenda-refile)
;; Give it a category (action context) with "c"
(define-key org-agenda-mode-map (kbd "c") 'kjb/set-category-property-cr--agenda)
;; Similarly, allowing for a completing-read category filter with "C"
(define-key org-agenda-mode-map (kbd "C") #'kjb/completing-read-category-filter)
;; Standard vim-style row navigation
(define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
(define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line)

;; Access to my main key map from agenda mode map
(define-key org-agenda-mode-map (kbd "SPC") top-level-map)

;; Have to allow scrolling; next action lists get big
(define-key org-agenda-mode-map (kbd "C-u") #'evil-scroll-up)
(define-key org-agenda-mode-map (kbd "C-d") #'evil-scroll-down)
