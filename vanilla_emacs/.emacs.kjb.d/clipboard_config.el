;; TODO Someday, get a clipboard utility that can be ivy-read
;; from. For now, i'm keeping some code I hacked together for clipcat,
;; only to learn later that it wasn't a good option due to a known
;; issue (see below). I'm keeping the work for posterity so I can
;; remember how I did things.

;; All this stuff was a great learning experience for how to use
;; ivy-read. And, I think a clipboard history utility would be smart
;; in the long run. However,
;; https://github.com/xrelkd/clipcat/issues/4 really makes it unusable
;; in the context of emacs. For now, my exwm simulation keys get me to
;; a comfortable enough spot. I'm keeping this code around, in the
;; hopes that I a) submit a PR to clipcat to fix this, and b) factor
;; this into a little elisp module.

;; (defun kjb/get-raw-clipcat-list ()
;;   "Retrieve clipcat list, format as list of strings"
;;   (let ((raw-clipcat-list (shell-command-to-string "clipcatctl list")))
;;     ;; For some reason if my progn isn't here, ivy-clipcat-pop will
;;     ;; just paste the last character. Worth understanding further when
;;     ;; I have time (lol)
;;     (progn
;;       (delete "" (split-string
;; 		  raw-clipcat-list "\n")))))

;; (defun kjb/reduce-list-to-string (str-list)
;;   "Reduce a list of strings to a single string by concatenation. See
;; https://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-strings
;; for inspiration"
;;   (seq-reduce
;;    (lambda (a b)
;;      (concat a b))
;;    (seq-filter #'stringp str-list)
;;    ""))

;; (defun kjb/get-clipcat-alist (rawlist)
;;   "Creates an alist with (id . value) cons from clipcat"
;;   (let ((clipcat-alist ()))
;;     (mapcar
;;      (lambda (element)
;;        ;; We actually want to reverse key-values here for an ivy front-end:
;;        ;; key: the clip the user wants to select
;;        ;; value: the clip cache ID on which we'll act
;;        (let ((key (kjb/reduce-list-to-string
;; 		   (cdr (split-string
;; 			 element
;; 			 ":"))))
;; 	     (val (car (split-string element ":"))))
;; 	 (evil--add-to-alist clipcat-alist key val)))
;;      rawlist)
;;     clipcat-alist))


;; (defun kjb/clipcat-selection-to-keyboard (clip)
;;   "Calls clipcatctl to send the clip to the X keyboard
;; clip: cons of (clipcontent . clipid)
;; "
;;   (shell-command (concat "clipcatctl promote " (cdr clip))))

;; ;; AFTER ALL THIS: https://github.com/xrelkd/clipcat/issues/4 means this is going to be a bad solution in emacs. But hope remains! This proves we can use a CLI clipboard and adapt with ivy + dmenu. Guess I'll look at clipster next.
;; ;; UGH but python??
;; ;; Maybe i just write one in c? lol
;; (defun kjb/ivy-clipcat-pop()
;;   "Integrate clipcat options with ivy-read. Inspired by ivy-clipmenu"
;;   (interactive)
;;   (let ((ivy-sort-functions-alist nil)
;; 	(raw-clipcat-list (kjb/get-raw-clipcat-list)))
;;     (ivy-read "Clipcats: "
;; 	      ;; Show most recent clips first
;; 	      (reverse (kjb/get-clipcat-alist raw-clipcat-list))
;; 	      :action (lambda (clip)
;; 			;; TODO need to use clipcatctl promote on the ID and then use evil-paste-after
;; 			(kjb/clipcat-selection-to-keyboard clip)))))
