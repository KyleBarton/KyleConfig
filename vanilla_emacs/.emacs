;; It's all in org now!
(org-babel-load-file (concat (getenv "HOME") "/configuration.org"))

(use-package ledger-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ledger-mode vterm dracula-theme poet-theme stimmung-themes org-roam lsp-java web-mode typescript-mode kotlin-mode rustic lua-mode lsp-ui lsp-mode flycheck dired-subtree evil-iedit-state iedit evil-collection magit neotree counsel-projectile projectile link-hint ace-window avy smex ivy-posframe counsel ivy evil-nerd-commenter evil-escape evil company exec-path-from-shell all-the-icons spacemacs-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
