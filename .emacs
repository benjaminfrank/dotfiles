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
      ispell-dictionary "british"
      sentence-end-double-space nil
      ; email
      mail-user-agent 'message-user-agent
      user-mail-address "Sam.Halliday@gmail.com"
      user-full-name "Sam Halliday"
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      send-mail-function 'smtpmail-send-it
      notmuch-fcc-dirs nil
      message-auto-save-directory (concat user-emacs-directory "drafts")
      notmuch-search-oldest-first nil
      notmuch-address-command "google-contacts"
      notmuch-saved-searches '(("inbox" . "tag:inbox")
			       ("unread" . "tag:unread")
			       ("flagged" . "tag:flagged")))
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
(add-hook 'scala-mode-hook (lambda()(flyspell-prog-mode)))


(require 'helm-config)
(helm-mode 1)

(defun indent-buffer ()
  "Indent the entire buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context"
  (interactive)
  (if (looking-back "[\t\s\n\r]\\{2,\\}" (- (point) 3))
      (hungry-delete-backward)
    (backward-kill-word 1)))

(defun git-grep (search)
  ; https://www.ogre.com/node/447
  "git-grep the entire current repo"
  (interactive (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -P -n " search " `git rev-parse --show-toplevel`")))


; modified commands
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "C-x C-c") (lambda() (interactive)
				  (message-box "use 'M-x save-buffers-kill-terminal'")))

; new bindings
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(global-set-key (kbd "s-f") 'find-name-dired)
(global-set-key (kbd "s-f") 'magit-find-file-completing-read)
(global-set-key (kbd "s-F") 'git-grep)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-n") 'ensime-search)

(add-hook 'text-mode-hook (lambda()(flyspell-mode 1))); (C-c $) for corrections

(require 'notmuch)
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(require 'notmuch-address)
(notmuch-address-message-insinuate)
