;; Basics
;; =======================================

(setq user-full-name "Owain Lewis")
(setq user-mail-address "owain@owainlewis.com")

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq ring-bell-function 'ignore)

;; Keep custom variables out of this file
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Some general editor nice to haves
;; =======================================

(global-linum-mode t)
(setq linum-format "%d ")

(menu-bar-mode -1)
(column-number-mode t)

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Styles
;; =======================================

(set-frame-font "DejaVu Sans Mono 10")

;; Packages
;; =======================================

(use-package nimbus-theme
  :init (load-theme 'nimbus t)
  :ensure t)

(use-package yaml-mode :ensure t)
(use-package markdown-mode :ensure t)

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(setq haskell-process-type 'stack-ghci)
