(require 'cl)

(setq user-full-name "Owain Lewis")
(setq user-mail-address "owain@owainlewis.com")

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar packages '(
  auto-complete
  clojure-mode
  flycheck
  go-autocomplete
  go-eldoc
  go-mode
  go-autocomplete
  flymake-go
  haskell-mode
  magit
  markdown-mode
  monokai-theme
  yaml-mode)
  "Default packages")

(defun packages-installed-p ()
  (loop for pkg in packages
    when (not (package-installed-p pkg))
      do (return nil)
      finally (return t)))

(unless (packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(menu-bar-mode -1)
(auto-complete-mode 1)
(load-theme 'monokai t)

(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-config-default)
(require 'auto-complete-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'go-autocomplete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
