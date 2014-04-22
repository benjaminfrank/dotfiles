(setq inhibit-startup-screen t
      show-paren-delay 0
      create-lockfiles nil
      make-backup-files nil
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      tab-width 2
      indent-tabs-mode nil
      scroll-error-top-bottom t
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
;(package-install 'hungry-delete)
(global-set-key (kbd "<C-backspace>") 'hungry-delete-backward)
;(global-set-key (kbd "C-M-h") 'backward-kill-word)

; TODO enter should enter and indent
; TODO backspace should go back one tab level
; TODO control-backspace should go back all whitespace or one word
; TODO Darcula theme


;; load the ensime lisp code...
(add-to-list 'load-path "ENSIME_ROOT/src/main/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
