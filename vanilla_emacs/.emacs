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


;; Make font a bit bigger this isn't going to work until I can build
;; it into the emacs-client startup.
(set-frame-font "Menlo-16")


;; Org simple presentations
;;; Long-term: you want Beamer + Latex
;;; Short-term: epresent should work ok


(use-package epresent)

;; Let's play with tree-sitter

(use-package tree-sitter)
(use-package tree-sitter-langs)


(defun kjb/tree-sitter-rust-hook ()
  "Hook to add for rust-mode"
  (tree-sitter-mode)
  (tree-sitter-hl-mode))

(add-hook 'rust-mode-hook #'kjb/tree-sitter-rust-hook)
(add-hook 'rustic-mode-hook #'kjb/tree-sitter-rust-hook)


;; ~93 height on my wide screen. Double this before we split sandwich
;; style.
(setq split-height-threshold 160)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" default))
 '(ledger-reports
   '(("bal -e 2023/02/01 Assets Liabilities" "ledger bal -e 2023/02/01 Assets Liabilities")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(package-selected-packages
   '(elm-mode epresent lsp-haskell hledger-mode org-agenda org-roam-ui yaml-mode lsp-pyright tree-sitter-langs tree-sitter-mode tree-sitter marginalia consult consult-embark embark orderless ejc-company ejc-sql ejc-autocomplete ledger-mode gnuplot gnuplot-mode tide dracula-theme vterm selectrum org-roam lsp-java web-mode typescript-mode kotlin-mode rustic lua-mode lsp-ui lsp-mode flycheck dired-subtree evil-iedit-state iedit evil-collection magit neotree counsel-projectile projectile link-hint ace-window avy smex ivy-posframe counsel ivy evil-nerd-commenter evil-escape evil company exec-path-from-shell all-the-icons spacemacs-theme use-package))
 '(warning-suppress-log-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Menlo")))))
