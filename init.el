;;; init.el --- Simple modern Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; A small Emacs setup for modern software work.
;;
;; The taste here is boring on purpose:
;; - Prefer built-in Emacs features.
;; - Add packages only when they remove real friction.
;; - Keep everything readable in one file.

;;; Code:

;;;; Identity

(setq user-full-name "Owain Lewis"
      user-mail-address "owain@owainlewis.com")

;;;; Startup and files

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function #'ignore
      use-dialog-box nil
      sentence-end-double-space nil
      confirm-kill-processes nil
      require-final-newline t)

(defvar owain-cache-directory (expand-file-name "var/" user-emacs-directory))
(make-directory owain-cache-directory t)

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" owain-cache-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" owain-cache-directory) t))
      create-lockfiles nil
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq savehist-file (expand-file-name "savehist" owain-cache-directory)
      recentf-save-file (expand-file-name "recentf" owain-cache-directory))

(make-directory (expand-file-name "backups/" owain-cache-directory) t)
(make-directory (expand-file-name "auto-save/" owain-cache-directory) t)

(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 100)

(defalias 'yes-or-no-p #'y-or-n-p)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))

;;;; UI

(load-theme 'modus-vivendi t)

(blink-cursor-mode -1)
(column-number-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(save-place-mode 1)
(show-paren-mode 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(dolist (hook '(eshell-mode-hook
                shell-mode-hook
                term-mode-hook
                vterm-mode-hook
                help-mode-hook
                org-mode-hook
                markdown-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

;;;; Packages

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(require 'seq)

(defvar owain-skip-package-install
  (and (not (getenv "OWAIN_EMACS_INSTALL_PACKAGES"))
       (or noninteractive
           (getenv "OWAIN_EMACS_NO_INSTALL")))
  "When non-nil, do not install packages while loading init.el.")

(defvar owain-package-list
  '(apheleia
    cape
    consult
    corfu
    dockerfile-mode
    editorconfig
    exec-path-from-shell
    go-mode
    magit
    marginalia
    markdown-mode
    orderless
    rust-mode
    terraform-mode
    typescript-mode
    vertico
    yaml-mode
    yasnippet)
  "Packages this config installs on first run.")

(when (and (not owain-skip-package-install)
           (seq-some (lambda (package)
                       (not (package-installed-p package)))
                     owain-package-list))
  (package-refresh-contents))

(require 'use-package)

(setq use-package-always-ensure (not owain-skip-package-install)
      use-package-always-defer t
      use-package-verbose nil)

;;;; Built-in quality of life

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 100
        recentf-exclude `(,owain-cache-directory))
  (recentf-mode 1))

(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1))

(use-package project
  :ensure nil
  :bind (("C-c p p" . project-switch-project)
         ("C-c p f" . project-find-file)
         ("C-c p b" . project-switch-to-buffer)
         ("C-c p s" . project-shell)
         ("C-c p r" . project-query-replace-regexp)))

;;;; Minibuffer search and navigation

(use-package vertico
  :if (or (not owain-skip-package-install)
          (package-installed-p 'vertico))
  :demand t
  :config
  (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :if (or (not owain-skip-package-install)
          (package-installed-p 'marginalia))
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c s g" . consult-grep)
         ("C-c s r" . consult-ripgrep)
         ("C-c s f" . consult-find)
         ("C-c s i" . consult-imenu)
         ("M-y" . consult-yank-pop)))

;;;; Completion and snippets

(use-package corfu
  :if (or (not owain-skip-package-install)
          (package-installed-p 'corfu))
  :demand t
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-preview-current nil)
  :config
  (global-corfu-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package yasnippet
  :if (or (not owain-skip-package-install)
          (package-installed-p 'yasnippet))
  :demand t
  :config
  (yas-global-mode 1))

;;;; Development

(use-package eglot
  :ensure nil
  :hook ((go-mode go-ts-mode rust-mode rust-ts-mode typescript-mode typescript-ts-mode
          tsx-ts-mode js-mode js-ts-mode python-mode python-ts-mode
          c-mode c-ts-mode c++-mode c++-ts-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)
              ("C-c o" . eglot-code-action-organize-imports)))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package xref
  :ensure nil
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-go-back)))

(use-package editorconfig
  :if (or (not owain-skip-package-install)
          (package-installed-p 'editorconfig))
  :demand t
  :config
  (editorconfig-mode 1))

(use-package apheleia
  :bind (("C-c f" . apheleia-format-buffer)))

(use-package magit
  :bind (("C-c g" . magit-status)))

(use-package exec-path-from-shell
  :if (and (memq window-system '(mac ns x))
           (or (not owain-skip-package-install)
               (package-installed-p 'exec-path-from-shell)))
  :demand t
  :config
  (exec-path-from-shell-initialize))

;;;; Languages

(defun owain-remap-mode (from to)
  "Use TO instead of FROM when TO is available."
  (when (fboundp to)
    (add-to-list 'major-mode-remap-alist (cons from to))))

(when (boundp 'major-mode-remap-alist)
  (owain-remap-mode 'bash-mode 'bash-ts-mode)
  (owain-remap-mode 'c-mode 'c-ts-mode)
  (owain-remap-mode 'c++-mode 'c++-ts-mode)
  (owain-remap-mode 'css-mode 'css-ts-mode)
  (owain-remap-mode 'go-mode 'go-ts-mode)
  (owain-remap-mode 'java-mode 'java-ts-mode)
  (owain-remap-mode 'js-mode 'js-ts-mode)
  (owain-remap-mode 'json-mode 'json-ts-mode)
  (owain-remap-mode 'python-mode 'python-ts-mode)
  (owain-remap-mode 'ruby-mode 'ruby-ts-mode))

(when (fboundp 'typescript-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

(when (fboundp 'tsx-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(use-package go-mode)
(use-package rust-mode)
(use-package typescript-mode)
(use-package markdown-mode)
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package terraform-mode)

(use-package org
  :ensure nil
  :init
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-return-follows-link t))

;;;; Local customizations

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
