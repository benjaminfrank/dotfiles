;;; local.el --- Local config
;;; Commentary:
;;
;;  Local config and WIP for my laptop.
;;
;;; Code:

(required 'darcula-theme)

(defun dark-theme ()
  "A dark coloured theme for hacking when there is no screen glare."
  (load-theme 'darcula t)
  (set-frame-font "Inconsolata-22"))
(dark-theme)

(defun light-theme ()
  "A light coloured theme for hacking when there is lots of screen glare."
  (load-theme 'leuven t)
  (set-frame-font "Inconsolata-22"))

;; I'll usually want access to these..
(find-file "~/Projects/")

;;; local.el ends here
