;; Where I maintain the configuration & package management needed for exwm (https://github.com/ch11ng/exwm)


(defun footime ()
  (interactive)
  (print "footime"))

(use-package exwm
  :init
  (exwm-enable)
  :config
  (setq-default exwm-manage-force-tiling t))


;; stolen from exwm-config-example:

(defun kjb/exwm-start-process (command)
  "Starts a process interactively"
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

;; Stolen from https://github.com/ch11ng/exwm/issues/198
(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 50) exwm-title
             (concat (substring exwm-title 0 49) "...")))))

;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)


;; Simulate macOs-style copy/paste by mapping M-c and M-v to C-c and C-v in X11
;; This could potentially overwrite capitalize-word and scroll-down, but I don't use those.
;; Still need to figure out how to get the minibuffer working
(setq-default exwm-input-simulation-keys
	      '(([?\M-c] . [?\C-c])
		([?\M-v] . [?\C-v])))

;; This is needed for exwm or posframe will hide behind an X window
(setq ivy-posframe-parameters '((parent-frame nil)))
