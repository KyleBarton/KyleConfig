
;; OK, let's do some spotify

;; TODO make this secure before committing to git
(defun get-spotify-oauth2 ()
  "Returns cons (id . secret) needed to connect with oauth2 in spotify.el"
  (cons "TODOGENERATE" "TODOGENERATE"))

;; Nope, this is way too out of date, need to clone. Argh.
;; This is actually a completely different package. re:
;; https://github.com/danielfm/spotify.el/pull/66, this package is
;; _almost_ in MELPA, so instead of cloneing down, I'm just going to
;; be patient and check back later.
;; (use-package spotify
;;   :config
;;   (let ((spotify-oauth-cons (get-spotify-oauth2)))
;;     (setq spotify-oauth2-client-id (car spotify-oauth-cons))
;;     (setq spotify-oauth2-client-secret (cdr spotify-oauth-cons))
;;     (setq spotify-transport 'connect)))
