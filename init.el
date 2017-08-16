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
  go-mode
  haskell-mode
  intero
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

(global-linum-mode 1)
(setq linum-format " %d ")

(load-theme 'monokai t)

(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-config-default)
(require 'auto-complete-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'haskell-mode-hook 'intero-mode)
