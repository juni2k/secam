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

;; use-package
;; =====================================================================
(eval-when-compile
  (require 'use-package))

;; Defaults etc.
;; =====================================================================

;; Frame title
(setq frame-title-format '("" "%b :: Emacs NT 4.0 Workstation"))

;; Constants
(setq nanont/indent-level 2)

;; Customization
;; =====================================================================
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Startup
;; =====================================================================

;; Get rid of that super shitty default behaviour to open
;, new instances for file preview in a split window
; (add-hook 'emacs-startup-hook 'delete-other-windows)
(setq inhibit-startup-screen t)

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

;; Theming
;; =====================================================================

(push (substitute-in-file-name "~/.emacs.d/themes/")
      custom-theme-load-path)

;; Theme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

(if (eq window-system 'w32)
    ;; Courier New can go fuck itself
    (set-face-attribute 'default nil :family "Consolas" :height 110))

;; No tool bar
(tool-bar-mode -1)

;; No menu bar
(menu-bar-mode -1)

;; No scroll bar
(scroll-bar-mode -1)

;; Mouse color
(set-mouse-color "#20B2AA")

;; Modes and Languages
;; =====================================================================

;; Lisp
;; ----

;; We stan Steel Bank CL!
(setq inferior-lisp-program "sbcl")

;; Web mode
;; --------

(use-package web-mode
  :ensure t
  :mode "\\.\(tt2\|html\)\\'"
  :init
  (defun web-mode-custom-indent ()
    (setq web-mode-markup-indent-offset nanont/indent-level)
    (setq web-mode-css-indent-offset nanont/indent-level)
    (setq web-mode-code-indent-offset nanont/indent-level)
    (setq web-mode-indent-style nanont/indent-level))
  (add-hook 'web-mode-hook 'web-mode-custom-indent))

;; CSS Mode
;; --------

(setq css-indent-offset nanont/indent-level)

;; Perl oddities
;; -------------

;; Disable perl-mode's terrible electric behaviour for some keys
(defun perl-mode-disable-auto-indent ()
  (local-unset-key (kbd "{"))
  (local-unset-key (kbd "}"))
  (local-unset-key (kbd ";"))
  (local-unset-key (kbd ":")))
(add-hook 'perl-mode-hook 'perl-mode-disable-auto-indent)

;; Indentation
(setq perl-indent-level nanont/indent-level)

;; Align ( ) like { }
(setq perl-indent-parens-as-block t)

;; Test files
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

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
  ;; Deferred for C
  :commands (lsp lsp-deferred)
  :hook (c-mode . (lambda () (require 'ccls) (lsp-deferred))))

;; Company
;; =====================================================================
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; FZF
;; =====================================================================
(use-package fzf
  :ensure t
  :init
  ; This actually Does The Right Thing by asking
  ; for a path if not in a git directory! Funky!
  (define-key global-map (kbd "C-c C-f") 'fzf-git))

;; Editing
;; =====================================================================

;; Line numbers on the left
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Fill column indicator ...
(when (version<= "27.1" emacs-version)
  (global-display-fill-column-indicator-mode))

;; ... but not for dashboard-mode!
(add-hook 'dashboard-mode-hook
          (lambda () (display-fill-column-indicator-mode -1)))

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

;; org-journal
(use-package org-journal
  :defer t
  :custom
  (org-extend-today-until 6) ; New day begins at 6 am!
  (org-journal-file-type 'weekly))

(define-key global-map (kbd "C-c C-j") 'org-journal-new-entry)

;; Saving
;; =====================================================================

;; Delete trailing whitespace on save
(defun nanont/delete-trailing-whitespace-hook ()
  (when (not (eq major-mode 'markdown-mode))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'nanont/delete-trailing-whitespace-hook)

;; Misc. Keybindings
;; =====================================================================
(define-key global-map (kbd "C-<tab>") 'mode-line-other-buffer)
(define-key global-map (kbd "C-<") 'undo)
(define-key global-map (kbd "<f5>") 'revert-buffer)
;; (define-key global-map (kbd "C-x k") 'kill-buffer-and-window)
