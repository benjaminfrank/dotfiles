(setq inhibit-startup-screen t
      show-paren-delay 0
      create-lockfiles nil
      make-backup-files nil
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      tab-width 2
      indent-tabs-mode nil
      scroll-error-top-bottom t
      show-trailing-whitespace t
      ispell-dictionary "british")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(set-default-font "Inconsolata-16")

(add-to-list 'load-path (concat user-emacs-directory "ensime/dist"))
; latest helm is incompatible with one of my older machines
(if (file-exists-p (concat user-emacs-directory "helm"))
    (add-to-list 'load-path (concat user-emacs-directory "helm")))
(if (file-exists-p "/usr/local/share/emacs/site-lisp")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(load-theme 'solarized-dark 'NO-CONFIRM)

(require 'git-gutter)
(require 'magit)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'scala-mode2)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'helm-config)
(helm-mode 1)

(defun indent-buffer ()
  "Indent (format) the buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun git-grep (search)
  ; https://www.ogre.com/node/447
  "git-grep the entire current repo"
  (interactive (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -P -n " search " `git rev-parse --show-toplevel`")))


; modified commands
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-<backspace>") 'hungry-delete-backward)
;(global-set-key (kbd "M-x") 'helm-M-x)

; new bindings
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(global-set-key (kbd "s-f") 'find-name-dired)
(global-set-key (kbd "s-f") 'magit-find-file-completing-read)
(global-set-key (kbd "s-F") 'git-grep)
;(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-n") 'ensime-search)

(require 'rmail) ; offlineimap grabs the mail
(require 'google-contacts)
(require 'google-contacts-message)
;(require 'notmuch-labeler)
(setq mail-user-agent 'message-user-agent
      user-mail-address "sam.halliday@gmail.com"
      user-full-name "Sam Halliday"
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      send-mail-function 'smtpmail-send-it
      google-contacts-user 'user-mail-address
      notmuch-fcc-dirs nil
      notmuch-saved-searches '((:name "inbox" :query "tag:inbox")
			       (:name "unread" :query "tag:unread")
			       (:name "flagged" :query "tag:flagged")))
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
