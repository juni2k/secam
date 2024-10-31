;; EMACS(-A) INIT FILE - the monumental one
;;   (for writing source code)
;; =====================================================================

;; Make this file run faster by making GC run less often
;; Remember to reset this value at the end!
(setq gc-cons-threshold 100000000)

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

;; Defaults etc.
;; =====================================================================

;; Frame title
(setq frame-title-format '("" "%b :: Emacs NT 4.0 Workstation"))

;; Tame Cocoa Emacs
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Load common config
;; =====================================================================
(load (concat user-emacs-directory "common.el"))

;; General Emacs customization
;; =====================================================================

;; Load up inferior lisp in the same window. This tweak alone improves
;; my sanity by at least 10%.
(add-to-list 'display-buffer-alist
             '("\\*inferior-lisp\\*" (display-buffer-same-window)))

;; Theming
;; =====================================================================

(push (substitute-in-file-name "~/.emacs.d/themes/")
      custom-theme-load-path)

(use-package kaolin-themes
  :ensure t)
(load-theme 'kaolin-blossom t)

;; I seriously do NOT get low-contrast people.
(set-background-color "#151515")

;; Make fringes use the default background colour
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; Disable left fringe (right one is sufficient)
(set-fringe-mode '(0 . nil))

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
  :ensure t
  :defer t
  :custom
  (org-journal-dir (if (member system-type '(windows-nt cygwin))
                       "~/Documents/org/journal"
                       "~/org/journal"))
  (org-journal-file-format "journal%Y%m%d.txt")
  (org-journal-file-type 'weekly))

(bind-key* "C-c C-j" 'org-journal-new-entry)

;; Manual bloat-loading facilities
;; =====================================================================
(defun load-bloat ()
  (interactive)
  (load (concat user-emacs-directory "bloat.el")))

;; Per-Device Customization
;; =====================================================================
(juni/load-local)

;; Post-init
;; =====================================================================
(custom-reevaluate-setting 'gc-cons-threshold)
