(setq inhibit-startup-screen t
      show-paren-delay 0
      create-lockfiles nil
      make-backup-files nil
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      tab-width 2
      indent-tabs-mode nil
      show-trailing-whitespace t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(set-default-font "Inconsolata-16")

(package-initialize)
(load-theme 'solarized-dark 'NO-CONFIRM)

(global-set-key (kbd "C-<tab>") 'dabbrev-expand)

(defun indent-buffer ()
  "Indent (format) the buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))
(global-set-key (kbd "C-M-f") 'indent-buffer)

                                        ; TODO: go to beginning/end of buffer with page up/down when they are visible
                                        ; TODO: hungry backspace
                                        ; TODO Darcula theme
