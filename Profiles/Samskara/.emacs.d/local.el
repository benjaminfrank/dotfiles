;;; local.el --- Local config
;;; Commentary:
;;
;;  For desktop.
;;
;;; Code:

(use-package darcula-theme
  :config
  (set-frame-font "Inconsolata-16"))

;; I'll usually want access to these..
(find-file (expand-file-name "scratch.el" user-emacs-directory))
(find-file "~/Projects")

;;; local.el ends here
