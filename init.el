;; Defaults
;; =======================================

(setq
   ;; My name
   user-full-name "Owain Lewis"
   ;; My email
   user-mail-address "owain@owainlewis.com"
   ;; No need to see GNU agitprop
   inhibit-startup-screen t
   ;; No need to remind me what a scratch buffer is
   initial-scratch-message nil
   ;; Double-spaces after periods is morally wrong
   sentence-end-double-space nil
   ;; Never ding at me, ever
   ring-bell-function 'ignore
   ;; Prompts should go in the minibuffer, not in a GUI
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark
   mark-even-if-inactive nil
   ;; Let C-k delete the whole line
   kill-whole-line t
   ;; search should be case-sensitive by default
   case-fold-search nil
   ;; Add some buffering to line numbers
   linum-format "%d "
   ;; Require newlines at end of files
   require-final-newline t
   )

(setq
  make-backup-files nil
  auto-save-default nil
  create-lockfiles nil)

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-fill-column 110)

;; Keep custom variables out of this file
(setq custom-file (concat user-emacs-directory "/custom.el"))

(global-linum-mode t)

;; Disable clutter in GUI
(delete-selection-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)

(setq-default indent-tabs-mode nil)
;; (setq-default show-trailing-whitespace t)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Package setup
;; =======================================
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
	("melpa"        . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org"          . "http://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Packages
;; =======================================

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-dark+ t)
  :config
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t)
    :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package format-all
  :ensure t)

(use-package tuareg
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(setq haskell-process-type 'stack-ghci)
(setq haskell-stylish-on-save t)
(setq haskell-mode-stylish-haskell-path "brittany")

(use-package projectile
  :ensure t
  :config (projectile-mode 1))

(use-package helm
  :ensure t
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))

(use-package dashboard
    :ensure t
    :diminish dashboard-mode
    :config
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 10)))
    (dashboard-setup-startup-hook))
