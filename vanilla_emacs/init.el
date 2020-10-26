;; The basics
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq user-emacs-directory
      (file-truename (concat default-directory ".emacs.d/")))

;; Get use-package set up
(require 'package)
(setq package-archives `(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Theme, Font, & Appearance
(use-package soft-stone-theme
  :config
  (load-theme 'soft-stone t))


;; Evil

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (setq evil-shift-width 2) ;; lets be honest I prefer it this way
  (evil-mode 1))

(use-package evil-escape
  :config
  (evil-escape-mode))

;; Evil-nerd commenter does a few things that comment-line and comment-dwim don't (or at least not without more work):
;; 1. It handles regions and lines interchangeably without issue
;; 2. Keeps the cursor on the line commented
;; 3. Doesn't comment the next line down in visual line mode
(use-package evil-nerd-commenter)

;; Ivy (comparing with helm)
(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t))

(use-package counsel)

;; Creates a floating M-x frame
(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
      '((swiper                               . ivy-posframe-display-at-point)
        ;;(complete-symbol                    . ivy-posframe-display-at-point)
        ;;(t                                  . ivy-posframe-display)
	(counsel-M-x                          . ivy-posframe-display-at-window-center)
	(counsel-find-file                    . ivy-posframe-display-at-window-center)
	(ivy-switch-buffer                    . ivy-posframe-display-at-window-center)
	(counsel-describe-function            . ivy-posframe-display-at-window-center)
	(counsel-describe-variable            . ivy-posframe-display-at-window-center)
	(counsel-projectile-find-file         . ivy-posframe-display-at-window-center)
	(counsel-projectile-switch-to-buffer  . ivy-posframe-display-at-window-center)
	(counsel-projectile-switch-project    . ivy-posframe-display-at-window-center)
	(counsel-projectile-find-dir          . ivy-posframe-display-at-window-center)
	(counsel-projectile-ag                . ivy-posframe-display-at-window-center)))
  (ivy-posframe-mode 1))

;; Projectile!
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

;; Trying counsel-projectile so I can use POSFrame on everything
(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;; neotree (mostly for use with projectile)
(use-package neotree
  :init
  (setq neo-show-hidden-files t)
  ;; confirm to delete files, but not to create them
  (setq neo-confirm-create-file 'off-p)
  :config
  (defun neotree-projectile ()
  "Open NeoTree using the project root, focus on current buffer file.
Borrowed from a config here: https://www.emacswiki.org/emacs/NeoTree.
If neotree is open, closes it."
    (interactive)
    (if (neo-global--window-exists-p)
	(neotree-toggle)
	(let ((project-dir (projectile-project-root))
	      (file-name (buffer-file-name)))
	  (if project-dir
	      (progn
		(neotree-dir project-dir)
		(neotree-find file-name))
	    (message "Could not find git project root.")))))

  (add-hook 'neotree-mode-hook
	    (lambda ()
	      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
	      (define-key evil-normal-state-local-map (kbd "gr") 'neotree-refresh)
	      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
	      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node))))

;; Magit -- still need to set up shortcuts, but I know I want it
;;;; TODO at least evil-ify it ya know
(use-package magit)

;; Major Modes

;;;; Rust
;;TODO need:
;; indentation stuff and evil-shift-width integration
;; lsp stuff
(use-package rust-mode)

;; TODO need to figure out how to ensure the right packages here
;; One idea would be to send commands to these key maps from within use-package

(defvar help-map (make-sparse-keymap)
  "Help & describe functions. General documentation")
(define-key help-map (kbd "f") 'counsel-describe-function)
(define-key help-map (kbd "v") 'counsel-describe-variable)
(define-key help-map (kbd "k") 'describe-key)

(defvar buffer-map (make-sparse-keymap)
  "Buffer manipulation")
(define-key buffer-map (kbd "d") 'kill-current-buffer)
(define-key buffer-map (kbd "b") 'ivy-switch-buffer)

(defvar window-map (make-sparse-keymap)
  "Window manipulation")
(define-key window-map (kbd "k") 'windmove-up)
(define-key window-map (kbd "j") 'windmove-down)
(define-key window-map (kbd "h") 'windmove-left)
(define-key window-map (kbd "l") 'windmove-right)
(define-key window-map (kbd "d") 'delete-window)

(defvar file-map (make-sparse-keymap)
  "File manipulation")
(define-key file-map (kbd "s") 'save-buffer)
(define-key file-map (kbd "f") 'counsel-find-file)

;; additional projectile addons here
(define-key projectile-command-map (kbd "t") 'neotree-projectile)

(defvar top-level-map (make-sparse-keymap)
  "Top level map to send functions to delegate maps")
;; direct commands (no need for a submap here yet
(define-key top-level-map (kbd "SPC") 'counsel-M-x)
(define-key top-level-map (kbd "cl") 'evilnc-comment-or-uncomment-lines)
(define-key top-level-map (kbd "u") 'universal-argument)
(define-key top-level-map (kbd "ee") 'eshell)
;; To submaps
(define-key top-level-map (kbd "w") window-map)
(define-key top-level-map (kbd "f") file-map)
(define-key top-level-map (kbd "b") buffer-map)
(define-key top-level-map (kbd "h") help-map)
;; Projectile has its own submap
(define-key top-level-map (kbd "p") projectile-command-map)

(evil-define-key 'motion 'global (kbd "SPC") top-level-map)


;;;;;;;;;;;;;
;; IDEAS/TODOS (Misc)
;; - Projectile (DONE)
;;     - Would really like to get TreeMacs working with Projectile (DONE)
;;	   - Went with neotree after all, not loving TreeMacs sticky workspace design
;; - Major modes:
;;     - Python
;;     - Rust (in progress)
;;     - Typescript/Javascript
;;     - Java
;;     - C#
;;     - Ruby
;; - Org mode
;; - Get Literate setup running for init.el
;;     - Good time to organize a bit
;; - Improve themes/fonts
;; - Neotree/some other NERDTree-like thing
;; - Eshell/terminals
;; - More advanced search w/ivy
;; - Iedit/some multi-editor
;; - Magit
;; - TRAMP
;;;;;;;;;;;;;
