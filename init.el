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

;; Defaults etc.
;; =====================================================================

;; Frame title
(setq frame-title-format '("" "%b :: Emacs NT 4.0 Workstation"))

;; Constants
(setq nanont-indent-level 2)

;; Startup
;; =====================================================================

;; Files
;; =====================================================================

;; Don't litter autosave files everywhere
(setq backup-directory-alist `(("." . "~/.emacs-autosave")))

;; Modes
;; =====================================================================

;; Web mode
(add-to-list 'auto-mode-alist '("\\.tt2\\'" . web-mode))
(defun web-mode-custom-indent ()
  (setq web-mode-markup-indent-offset nanont-indent-level)
  (setq web-mode-css-indent-offset nanont-indent-level)
  (setq web-mode-code-indent-offset nanont-indent-level)
  (setq web-mode-indent-style nanont-indent-level))
(add-hook 'web-mode-hook 'web-mode-custom-indent)

;; Theming
;; =====================================================================

(push (substitute-in-file-name "~/.emacs.d/themes/")
      custom-theme-load-path)

;; Theme
(load-theme 'faff t)

;; Font
(add-to-list 'default-frame-alist
	     '(font . "6x13"))

;; No tool bar
(tool-bar-mode -1)

;; No menu bar
(menu-bar-mode -1)

;; No scroll bar
(scroll-bar-mode -1)

;; Mouse color
(set-mouse-color "#20B2AA")

;; LSP
;; =====================================================================

(require 'lsp-mode)

(setq lsp-enable-snippet nil)

;; For C++
(add-hook 'c++-mode-hook #'lsp-deferred)


;; UI
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

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
(setq tab-width nanont-indent-level)

;; Don't indent the previous line when typing <RET>
;; (but keep indenting the new line)
(setq-default electric-indent-inhibit t)

;; Fill towards 72 columns ...
(setq-default fill-column 72)

;; Highlight matching parentheses on "hovering" over them
(show-paren-mode 1)

;; Smooth scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)

;; Enable EditorConfig
(editorconfig-mode 1)

;; Saving
;; =====================================================================

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; CSS Mode
;; =====================================================================

(setq css-indent-offset nanont-indent-level)

;; Perl oddities
;; =====================================================================

;; Disable perl-mode's terrible electric behaviour for some keys
(defun perl-mode-disable-auto-indent ()
  (local-unset-key (kbd "{"))
  (local-unset-key (kbd "}"))
  (local-unset-key (kbd ";"))
  (local-unset-key (kbd ":")))
(add-hook 'perl-mode-hook 'perl-mode-disable-auto-indent)

;; Indentation
(setq perl-indent-level nanont-indent-level)

;; Lua oddities
;; =====================================================================

(setq lua-indent-level nanont-indent-level)

;; Go oddities
;; =====================================================================

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; C++ perversions
;; =====================================================================

;; Apply style from (n-parent dir) .clang-format
(setq clang-format-style-option "file")

;; clang-format
(defun nanont-c++-mode-before-save-hook ()
  (when (eq major-mode 'c++-mode)
    (clang-format-buffer)))

(add-hook 'before-save-hook 'nanont-c++-mode-before-save-hook)

;; Emacs might add some junk here
;; =====================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#333" "#ff5f87" "#3affa3" "#f6df92" "#b2baf6" "#c350ff" "#5af2ee" "#ccc"])
 '(custom-safe-themes
   (quote
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "6f9fc46380ff9f00da8c10b47bfb01341fe4d8a0e68dffffb0c0ba1d2cd887d8" "d986619578e8a8dabb846e91c54090b82d937672f54ffa0ef247c0428813d602" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "c221703cc604312f6f72349704f7329f80ccc6a261af769332ec80171b728cc0" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" "5ed25f51c2ed06fc63ada02d3af8ed860d62707e96efc826f4a88fd511f45a1d" "4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1" "296da7c17c698e963c79b985c6822db0b627f51474c161d82853d2cb1b90afb0" "ef98b560dcbd6af86fbe7fd15d56454f3e6046a3a0abd25314cfaaefd3744a9e" "905cee72827a1ac7ad75d7407bfb222ad519f9ebcd9d1f70f00e1115a8448cf6" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" default)))
 '(package-selected-packages
   (quote
    (clang-format flycheck lsp-ui lsp-mode lua-mode fzf base16-theme smooth-scrolling subatomic-theme auto-complete go-mode editorconfig fish-mode grandshell-theme magit typescript-mode paganini-theme brutalist-theme fireplace web-mode gruvbox-theme markdown-mode markdown-mode+ yaml-mode faff-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
