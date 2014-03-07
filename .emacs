(setq inhibit-startup-screen t
      show-paren-delay 0
      create-lockfiles nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(set-default-font "Inconsolata-16")

; TODO pageup/pagedown
; TODO Darcula theme

(package-initialize)
(load-theme 'solarized-dark 'NO-CONFIRM)
