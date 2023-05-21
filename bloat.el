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
