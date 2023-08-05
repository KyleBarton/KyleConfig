;; Meat of my config comes from this literate org file.
(org-babel-load-file (concat (getenv "HOME") "/configuration.org"))

;; Set up GTD (may move this to org some day)
(load (concat (getenv "HOME") "/.emacs_custom_packages/gtd.el"))
