;;; hs-org-capture.el --- MacOs integration for creating org capture
;;; buffers in flyweight frames using hammerspoon.

;; Author: kjb <kjbarton4@gmail.com>
;; Version: 0.1
;; Package-Requires: ((s) (org-roam))
;; Keywords: hammerspoon, frame
;; URL: https://github.com/KyleBarton/KyleConfig

;;; Commentary:

;; TODO clean this up after implementation: This package provides an
;; integration with hammerspoon by providing an entrypoint function
;; which creates an emacs frame and executes a given function
;; (e.g. taking an org-roam note). After the emacs function concludes,
;; control is returned to hammerspoon for whatever window manipulation
;; is needed.

;; - Multi-screen and multi-workspace arrangements not yet working.

;;; Code:

;; TODO this needs to be renamed to hs-org-capture and stay specific to that.

(require 'org-roam)
(require 's)

(defgroup hs-org-capture()
  "Quick frames for hammerspoon keybindings"
  :group 'Tools)

(defcustom hs-org-capture/hs-cmd "hs"
  "Hammerspoon executable for IPC. Defaults to simply 'hs' and
assumes that it lives on the path. In some cases, you might have
a PATH collection (e.g. if you use node's http-server), in which
case you'll want to customize this value to the location of the
'hs' executable off-path."
  :type 'string
  :group 'hs-org-capture)

(defcustom hs-org-capture/org-roam-daily-capture-timestamp--frame-name "CAPTURE-TIMESTAMP"
  "Frame name used when taking a daily timestamped note"
  :type 'string
  :group 'hs-org-capture)

(defcustom hs-org-capture/hs-callback "hs.window.switcher.nextWindow()"
  "Hammerspoon function which is invoked when control is returned
to hammerspoon via IPC"
  :type 'string
  :group 'hs-org-capture)

(defun hs-org-capture/hs-callback ()
  "A hook function which will call hammerspoon via IPC after the
  capture is finalized. It will delete the frame and remove
  itself from `org-capture-after-finalize-hook' on its way out."
  (eshell-command (format
		   "%s -c '%s'"
		   hs-org-capture/hs-cmd
		   hs-org-capture/hs-callback))
  (remove-hook 'org-capture-after-finalize-hook #'hs-org-capture/hs-callback)
  (delete-frame))


(defun hs-org-capture/capture-in-floating-frame (window-name capture-fn &rest args)
  "Performs an org capture command in a floating frame. Uses hooks
to ensure `hs-org-capture/hs-callback' is called once after the
capture is completed."
  (make-frame `((auto-raise . t)
		(name . ,window-name)
		(menu-bar-lines . 0)
		(undecorated . t)
		(left . 0.5)
		(top . 0.5)
		(height . 0.3)
		(width . 0.35)))
  (select-frame-by-name window-name)
  (add-hook 'org-capture-after-finalize-hook #'hs-org-capture/hs-callback)
  (delete-other-windows)
  (apply capture-fn args)
  (delete-other-windows))


;;;###autoload
(defun hs-org-capture/org-roam-daily-capture-timestamp ()
  "Version 0.0.10"
  (interactive)
  (hs-org-capture/capture-in-floating-frame
   hs-org-capture/org-roam-daily-capture-timestamp--frame-name
   #'org-roam-dailies-capture-today nil "t"))

(provide 'hs-org-capture)

;;; hs-org-capture.el ends here
