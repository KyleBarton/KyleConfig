;; Meat of my config comes from this literate org file.
(org-babel-load-file (concat (getenv "HOME") "/configuration.org"))

;; Set up GTD (may move this to org some day)
(load (concat (getenv "HOME") "/.emacs_custom_packages/gtd.el"))

;; Set up my WIP theme so I can spend some time with it
;; Some notes
;; - I really only use two colors: one for in/next/waiting for, and one for someday/maybe (by stealing org-cite).
;;   - it would be good to mix this up a bit
;;   - really like the org-cite color right now: it's #57b6be
;; - org-done already looks pretty good, probably don't need to mess with that
(load (concat (getenv "HOME") "/.emacs_custom_packages/wip_theme.el"))
  ;; (use-package doom-themes
  ;;   :ensure t
  ;;   :config
  ;;   ;; Global settings (defaults)
  ;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  ;; 	  doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;   (load-theme 'doom-one t))


(setq split-height-threshold nil)
;; Always split horizontally
(setq split-width-threshold 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;NEW LANGUAGE SUPPORT ;;;;;;;;;;;;;;;;;;;;;

;; These are language modes I want to add to configuration.org, but
;;  haven't yet due to experimental nature/unsure/haven't used the
;;  mode enough yet.

;;;; Pywright for Python
;; Note that python-mode is built into emacs, and honestly has a lot
;; of the features I want. But this takes advantage of the pyright
;; language server, which is fine for now, since I don't have a strong
;; opinion
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))


;;; Elm mode - so far enjoying but haven't used at great scale yet.
(use-package elm-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
  (add-hook 'elm-mode-hook #'lsp))

;;; Yaml-mode - not super happy with this, and haven't taken the time
;;; to configure yet.
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;;; Haskell-mode and associated lsp - again, generally liking the
;;; experience so far, but haven't used at great scale and don't feel
;;; the need to solidify in configuration.org yet.
(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :config
  (add-hook 'haskell-mode-hook
	    (lambda ()
	      (eval (lsp))
	      (eval (lsp-ui-sideline-mode))))
  (add-hook 'haskell-literate-mode-hook #'lsp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

