(required 'darcula-theme t (lambda() (set-frame-font "Inconsolata-18")))

(defun light-theme()
  (interactive)
  (required 'solarized t (lambda()
                           (setq
                            solarized-high-contrast-mode-line t
                            solarized-emphasize-indicators nil)
                           (load-theme 'solarized-light)
                           (set-frame-font "Inconsolata-20"))))
