;;; hs-ivy.el --- Experimental MacOs integration for Ivy and Hammerspoon

;; Author: kjb <kjbarton4@gmail.com>
;; Version: 0.1
;; Package-Requires: ((ivy "20210602.1349") (s))
;; Keywords: hammerspoon, window
;; URL: https://github.com/KyleBarton/KyleConfig

;;; Commentary:

;; This package provides and integration with hammerspoon and
;; `ivy-read` in order to allow window selection and manipulation via
;; a floating window. Integration is experimental and there are a few
;; standing TODOs:

;; - Window selection is not sorted, by history or any other
;; - attribute. This makes it hard on the user when there are many
;; - windows to consider.

;; - No additional ivy commands are defined. This is probably an
;; - interface we want to explore in order to arrange windows on the
;; - screen

;; - Multi-screen and multi-workspace arrangements not yet working.

;;; Code:

(require 'ivy)
(require 's)

(defgroup hs-ivy()
  "Experimental MacOs integration for Ivy and Hammerspoon"
  :group 'Tools)

(defcustom hs-ivy/hs-cmd "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs"
  "Hammerspoon executable for IPC. Defaults to
/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs. In
many cases, we do not want to add 'hs' straight to the user PATH
because of a conflict with hs, the http-server"
  :type 'string
  :group 'hs-ivy)

(defun hs-ivy/ivy-read-in-floating-window (window-name prompt collection action_fn)
  "Creates a floating frame in the middle of the screen for an ivy
  read. This function focuses on the management of the frame; the
  caller must define the action taken and provide a static collection.

  TODO for the future: add the ability to provide a collection function
  instead."
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
      (ivy-read prompt collection
		:action action_fn
		;; If the user exits out, return control to the
		;; previous window as this frame deletes.
		:unwind (lambda ()
			  (unless (eq ivy-exit 'done)
			    (eshell-command (format
					       "%s -c 'hs.window.switcher.nextWindow()'"
					       hs-ivy/hs-cmd)))
			  (delete-frame)))))
;;;###autoload
(defun hs-ivy/hs-window-select (floating-win-name windows)
  "Provides `hs-ivy/ivy-read-in-floating-window' with a collection
of windows and a selection function that calls Hammerspoon to
switch window focus. Collection provided to this function."
  (hs-ivy/ivy-read-in-floating-window
   floating-win-name
   "Window: "
   windows
   (lambda (selection)
			  (eshell-command (format
					   "%s -c \"focusWindowByTitle([[%s]])\""
					   hs-ivy/hs-cmd
					   ;; In cases when the window
					   ;; name has a " in it,
					   ;; emacs <-> Hammerspoon
					   ;; IPC struggles if we
					   ;; don't escape the
					   ;; quotation explicitly.
					   (s-replace-all '(("\"" . "\\\""))
							  selection))))))

(provide 'hs-ivy)

;;; hs-ivy.el ends here
