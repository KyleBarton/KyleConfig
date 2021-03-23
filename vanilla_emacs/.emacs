;; It's all in org now!
(org-babel-load-file (concat default-directory "configuration.org"))

(load (concat default-directory ".emacs.kjb.d/exwm_config.el"))


;; This section relies on an Xmodmap that removes the CAPSLOCK
;; functionality, and replaces it with the `nabla' [8711] character,
;; which shouldn't ever be important to cntext (hopefully). The
;; Xmodmap lines should be thus:

;; ! Remove caps lock functionality
;; remove Lock = Caps_Lock
;; ! Caps Lock -> Nabla
;; keycode 66 = nabla
(evil-define-key 'motion 'global [8711] top-level-map)

(define-key exwm-mode-map [8711] top-level-map)

(define-key top-level-map [8711] #'kjb/exwm-start-process)

;; Keeping a todo list. This should be kept
(setq org-agenda-files '("~/agenda"))


(setq-default lsp-rust-analyzer-store-path "/home/kjb/.cargo/bin/rust-analyzer")
