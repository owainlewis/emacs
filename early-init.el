;;; early-init.el --- Startup defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep startup quiet and fast before init.el loads.

;;; Code:

(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; early-init.el ends here
