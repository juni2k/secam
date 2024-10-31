;; EMACS-B INIT FILE - the fast one
;;   (for note-taking, org-mode, light text editing)
;; =====================================================================

;; Stoopid emacs, this should be set automatically...
(setq user-init-file load-file-name)

;; Defaults etc.
;; =====================================================================

;; Frame title
(setq frame-title-format '("" "%b [Emacs-B]"))

;; Load common config
;; =====================================================================
(load (concat user-emacs-directory "common.el"))

;; Per-Device Customization
;; =====================================================================
(juni/load-local)
