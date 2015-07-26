;;; local.el --- Local config
;;; Commentary:
;;
;;  Local config and WIP for my laptop.
;;
;;; Code:

(required 'darcula-theme (lambda() (set-frame-font "Inconsolata-18")) t)

(defun light-theme()
  (interactive)
  (required 'solarized (lambda()
                         (setq
                          solarized-high-contrast-mode-line t
                          solarized-emphasize-indicators nil)
                         (load-theme 'solarized-light)
                         (set-frame-font "Inconsolata-20"))) t)

;; I'll usually want access to these..
(find-file "~/Projects/ensime-emacs")
(find-file "~/Projects/ensime-server")

;; WIP
(find-file "~/Projects/e99")
;; it is not possible to add the CWD to the load-path in an automated
;; way, so we have to explicitly add elisp WIP to the load-path.
(add-to-list 'load-path "~/Projects/e99")

;;; local.el ends here
