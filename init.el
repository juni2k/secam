;; MELPA
;; =====================================================================

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Require(s)
;; =====================================================================
(eval-when-compile
  (require 'bind-key)
  (require 'use-package))

;; Interactively Do Things -- part of Emacs since v22.
;; "oh wow, this makes everything so much better" -- martin 2023
(require 'ido)
(ido-mode t)

;; Defaults etc.
;; =====================================================================

;; Frame title
(setq frame-title-format '("" "%b :: Emacs NT 4.0 Workstation"))

;; Constants
(setq nanont/indent-level 2)

;; Tame Cocoa Emacs
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

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

;; General Emacs customization
;; =====================================================================

;; Load up inferior lisp in the same window. This tweak alone improves
;; my sanity by at least 10%.
(add-to-list 'display-buffer-alist
             '("\\*inferior-lisp\\*" (display-buffer-same-window)))

;; Dashboard
;; =====================================================================
(use-package page-break-lines
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents . 15)))
  ;; Text logo
  (setq dashboard-startup-banner 3)
  (dashboard-setup-startup-hook)
  ;; Make n and p work like in dired
  (define-key dashboard-mode-map (kbd "p") 'dashboard-previous-line)
  (define-key dashboard-mode-map (kbd "n") 'dashboard-next-line))

;; Files
;; =====================================================================

;; Don't litter autosave files everywhere
(setq backup-directory-alist `(("." . "~/.emacs-autosave")))

;; Dired
;; =====================================================================

;; Hide details [press ( to show]
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Theming
;; =====================================================================

(push (substitute-in-file-name "~/.emacs.d/themes/")
      custom-theme-load-path)

;; Fonts
;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(defun font-available-p (font-name)
  (find-font (font-spec :name font-name)))

(when (eq window-system 'x)
  (set-frame-font "6x13"))

(cond
 ;; This is a surprisingly good font, thanks M$
 ((font-available-p "Consolas")
  (set-frame-font "Consolas-11"))
 ;; *nix
 ((font-available-p "Unifont")
 (set-frame-font "Unifont-12")))

;; No tool bar
(tool-bar-mode -1)

;; No menu bar (except for Cocoa)
(unless (eq window-system 'ns)
  (menu-bar-mode -1))

;; No scroll bar
(scroll-bar-mode -1)

;; Mouse color
(set-mouse-color "#20B2AA")

;; Modes and Languages
;; =====================================================================

;; Lisp / Lisp-likes
;; -----------------

;; SLIME
(use-package slime
  :ensure t
  :config
  ;; We stan Steel Bank CL!
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy))

;; Geiser (Scheme, Racket, etc.)
(use-package geiser
  :ensure t
  :init
  ;; Do not open a new window at startup
  (setq geiser-repl-use-other-window nil)
  ;; Racket, and only Racket
  (setq geiser-active-implementations '(racket)))

;; Web mode
;; --------

(use-package web-mode
  :ensure t
  :mode ("\\.tt2\\'"
         "\\.dtl\\'"
         "\\.html\\'")
  :init
  (defun web-mode-custom-indent ()
    (setq web-mode-markup-indent-offset nanont/indent-level)
    (setq web-mode-css-indent-offset nanont/indent-level)
    (setq web-mode-code-indent-offset nanont/indent-level)
    (setq web-mode-indent-style nanont/indent-level))
  (add-hook 'web-mode-hook 'web-mode-custom-indent)
  :config
  ;; Stop web-mode from placing quotes like `abbr=""` the
  ;; second you type the equal sign
  (setq web-mode-enable-auto-quoting nil))

;; CSS Mode
;; --------

(setq css-indent-offset nanont/indent-level)

;; Perl oddities
;; -------------

;; Test files
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

;; Indentation
(setq perl-indent-level nanont/indent-level)

;; Align ( ) like { }
(setq perl-indent-parens-as-block t)

;; Get rid of displaying whitespace as underlines
(setq perl-invalid-face nil)

;; Lua oddities
;; ------------

(setq lua-indent-level nanont/indent-level)

;; Go oddities
;; -----------

;; Run goimports as a formatter
(setq gofmt-command "goimports")
(defun nanont/go-mode-before-save-hook ()
  (when (eq major-mode 'go-mode)
    (gofmt-before-save)))
(add-hook 'before-save-hook 'nanont/go-mode-before-save-hook)

;; C++ perversions
;; ---------------

;; Apply style from (n-parent dir) .clang-format
(setq clang-format-style-option "file")

;; clang-format
(defun nanont/c++-mode-before-save-hook ()
  (when (eq major-mode 'c++-mode)
    (clang-format-buffer)))

(add-hook 'before-save-hook 'nanont/c++-mode-before-save-hook)

;; Language Server oddities
;; ========================

(use-package ccls
  :ensure t)

(use-package lsp-mode
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq lsp-enable-on-type-formatting nil) ; FUCK YOU FUCK YOU FUCK YOU
                                           ; FUCK YOU FUCK YOU FUCK YOU
                                           ; FUCK YOU FUCK YOU FUCK YOU
                                           ; FUCK YOU FUCK YOU FUCK YOU
                                           ; FUCK YOU FUCK YOU FUCK YOU
  ;; Deferred for C
  :commands (lsp lsp-deferred)
  :hook (c-mode . (lambda () (require 'ccls) (lsp-deferred))))

;; Company
;; =====================================================================
(use-package company
  :ensure t
  :init
  (bind-key* "C-#" 'company-complete)
  (add-hook 'after-init-hook 'global-company-mode))

;; FZF
;; =====================================================================
(use-package fzf
  ; :ensure t
  :init
  ; This actually Does The Right Thing by asking
  ; for a path if not in a git directory! Funky!
  (bind-key* "C-c C-f" 'fzf-git))

;; Editing
;; =====================================================================

;; Line numbers on the left
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

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

;; Highlight matching parentheses on "hovering" over them
(show-paren-mode 1)

;; Enable EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Org
;; =====================================================================


(bind-key* "C-c l" 'org-store-link)
(bind-key* "C-c a" 'org-agenda)
(bind-key* "C-c c" 'org-capture)

;; Log time when closing TODO items
(setq org-log-done 'time)

;; org-journal
(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/org/journal")
  (org-journal-file-type 'weekly))

(bind-key* "C-c C-j" 'org-journal-new-entry)

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

;; Per-Device Customization
;; =====================================================================
(load (concat user-emacs-directory "local.el"))
