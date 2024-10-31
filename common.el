;; Loaded by Emacs-A and Emacs-B

;; Constants
(setq nanont/indent-level 2)

;; Customization
;; =====================================================================
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Add 3rd-party-lisp to load path
(let ((add-it (lambda (dir)
                (add-to-list 'load-path dir))))
  (mapc add-it
        (directory-files (concat user-emacs-directory "vendor-lisp")
                         t
                         directory-files-no-dot-files-regexp)))

;; Convenience function for editing init.el
(defun edit-init ()
  (interactive)
  (find-file user-init-file))

;; System
;; =====================================================================
(defun add-to-paths (path)
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (setq exec-path (append exec-path (list path))))

(setq user-home-directory (concat (getenv "HOME")))

;; Startup
;; =====================================================================

;; Get rid of that super shitty default behaviour to open
;, new instances for file preview in a split window
; (add-hook 'emacs-startup-hook 'delete-other-windows)
(setq inhibit-startup-screen t)

;; Files
;; =====================================================================

;; Don't litter autosave files everywhere
(setq backup-directory-alist `(("." . "~/.emacs-autosave")))

;; Dired
;; =====================================================================

;; Hide details [press ( to show]
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Editing
;; =====================================================================

;; Line numbers and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)

;; Tell tabs to go home, and set indentation leven
(setq-default indent-tabs-mode nil)
(setq tab-width nanont/indent-level)

;; Don't indent the previous line when typing <RET>
;; (but keep indenting the new line)
(setq-default electric-indent-inhibit t)

;; Fill towards 72 columns ...
(setq-default fill-column 72)

;; Saving
;; =====================================================================

;; Delete trailing whitespace on save
(defun nanont/delete-trailing-whitespace-hook ()
  (when (not (eq major-mode 'markdown-mode))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'nanont/delete-trailing-whitespace-hook)

;; Misc. Keybindings
;; =====================================================================
(bind-key* "C-<tab>" 'mode-line-other-buffer)
(bind-key* "C-<" 'undo)
(bind-key* "<f5>" 'revert-buffer)
(bind-key* "C-c t" 'nanont/rotate-themes)
(bind-key* "C-x C-o" 'ff-find-other-file)
;; (define-key global-map (kbd "C-x k") 'kill-buffer-and-window)

;; Org
;; =====================================================================
(bind-key* "C-c l" 'org-store-link)
(bind-key* "C-c a" 'org-agenda)
(bind-key* "C-c c" 'org-capture)

;; Log time when closing TODO items
(setq org-log-done 'time)

;; Theming
;; =====================================================================

;; Fonts
;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(defun font-available-p (font-name)
  (find-font (font-spec :name font-name)))

(when (eq window-system 'x)
  (set-frame-font "6x13"))

(cond
 ;; This is a surprisingly good font, thanks MICROS~1
 ((font-available-p "Consolas")
  (set-frame-font "Consolas-11"))
 ;; *nix
 ((font-available-p "Unifont")
 (set-frame-font "Unifont-12")))

;; Per-Device Customization
;; =====================================================================
(defun juni/load-local ()
  (load (concat user-emacs-directory "local.el")))
