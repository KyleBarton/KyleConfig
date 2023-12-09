;; Meat of my config comes from this literate org file.
(org-babel-load-file (concat (getenv "HOME") "/configuration.org"))

;; Set up GTD (may move this to org some day)
(load (concat (getenv "HOME") "/.emacs_custom_packages/gtd.el"))

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

