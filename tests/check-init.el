;;; check-init.el --- Batch smoke test -*- lexical-binding: t; -*-

;;; Code:

(setq owain-skip-package-install t)

(let ((user-emacs-directory
       (file-name-directory (directory-file-name
                             (file-name-directory load-file-name)))))
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
  (load (expand-file-name "init.el" user-emacs-directory) nil t))

(message "Config loaded successfully")

;;; check-init.el ends here
