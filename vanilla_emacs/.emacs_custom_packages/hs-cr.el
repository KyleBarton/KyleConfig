;;; hs-cr.el --- Experimental MacOs integration for completing-read and Hammerspoon

;; Author: kjb <kjbarton4@gmail.com>
;; Version: 0.1
;; Package-Requires: ((s))
;; Keywords: hammerspoon, window
;; URL: https://github.com/KyleBarton/KyleConfig

;;; Commentary:

;; This package provides and integration with hammerspoon and
;; `completing-read` in order to allow window selection and manipulation via
;; a floating window. Integration is experimental and there are a few
;; standing TODOs:

;; - Window selection is not sorted, by history or any other
;; - attribute. This makes it hard on the user when there are many
;; - windows to consider.

;; - No additional ivy commands are defined. This is probably an
;; - interface we want to explore in order to arrange windows on the
;; - screen
;; - I think I can use consult to live-preview windows with this but not sure

;; - Multi-screen and multi-workspace arrangements not yet working.

;; This is a "v2" of hs-ivy as I look to remove my ivy dependency and
;; commit to a vmoce stack.

;;; Code:

(require 's)

(defgroup hs-cr()
  "Experimental MacOs integration for Ivy and Hammerspoon"
  :group 'Tools)

(defcustom hs-cr/hs-cmd "hs"
  "Hammerspoon executable for IPC. Defaults to hs and assumes it is
on the PATH.. In some cases, we do not want to add 'hs' straight
to the user PATH because of a conflict with hs, the
http-server. In this case, customize this variable to the
absolute path to the hs executable."
  :type 'string
  :group 'hs-ivy)

(defun hs-cr/completing-read-in-floating-window (window-name prompt collection action_fn)
  "Creates a floating frame in the middle of the screen for an ivy
  read. This function focuses on the management of the frame; the
  caller must define the action taken and provide a static collection.

  TODO for the future: add the ability to provide a collection function
  instead."
  (unwind-protect
      (with-current-buffer (get-buffer-create window-name)
	(make-frame `((auto-raise . t)
		      (name . ,window-name)
		      (menu-bar-lines . 0)
		      (undecorated . t)
		      (minibuffer . only)
		      (left . 0.5)
		      (top . 0.5)
		      (height . 0.3)
		      (width . 0.35)))
	(let ((selection (completing-read prompt collection)))
	  (funcall action_fn selection)))
    (delete-frame)))



;;;###autoload
(defun hs-cr/hs-window-select (floating-win-name windows)
  "Provides `hs-cr/completing-read-in-floating-window' with a collection
of windows and a selection function that calls Hammerspoon to
switch window focus. Collection provided to this function."
  (hs-cr/completing-read-in-floating-window
   floating-win-name
   "Window: "
   windows
   (lambda (selection)
			  (eshell-command (format
					   "%s -c \"focusWindowByTitle([[%s]])\""
					   hs-cr/hs-cmd
					   ;; In cases when the window
					   ;; name has a " in it,
					   ;; emacs <-> Hammerspoon
					   ;; IPC struggles if we
					   ;; don't escape the
					   ;; quotation explicitly.
					   (s-replace-all '(("\"" . "\\\""))
							  selection))))))

(provide 'hs-cr)

;;; hs-cr.el ends here
