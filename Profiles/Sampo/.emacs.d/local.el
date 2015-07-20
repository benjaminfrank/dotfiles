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
