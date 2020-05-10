;; Basics
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
   )

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq
  make-backup-files nil
  auto-save-default nil
  create-lockfiles nil)

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-fill-column 110)

(scroll-bar-mode -1)


;; Keep custom variables out of this file
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Some general editor nice to haves
;; =======================================

(global-linum-mode t)
(setq linum-format "%d ")

(delete-selection-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

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

(set-frame-font "DejaVu Sans Mono 12")

;; Init file updates
;; ======================================
(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.emacs.id/init.el"))

(bind-key "C-c e" #'open-init-file)

;; Packages
;; =======================================

;; (use-package nimbus-theme
;;   :init (load-theme 'nimbus t)
;;   :ensure t)

(use-package doom-themes
  :init (load-theme 'doom-challenger-deep t)
  :config
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t)
  :ensure t)

(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title "Welcome back.")
  :config (dashboard-setup-startup-hook))

(use-package yaml-mode :ensure t)

(use-package markdown-mode :ensure t)

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
(custom-set-variables '(haskell-stylish-on-save t))
(setq haskell-process-type 'stack-ghci)

(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package projectile
  :ensure t
  :diminish
  :bind (("C-c k" . #'projectile-kill-buffers)
	   ("C-c M" . #'projectile-compile-project))
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode))

(use-package helm
  :ensure t
  :bind (("M-a" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-SPC" . helm-dabbrev)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
	      ("M-i" . helm-previous-line)
	      ("M-k" . helm-next-line)
	      ("M-I" . helm-previous-page)
	      ("M-K" . helm-next-page)
	      ("M-h" . helm-beginning-of-buffer)
	      ("M-H" . helm-end-of-buffer))
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))
