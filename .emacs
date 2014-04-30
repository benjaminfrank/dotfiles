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
      message-auto-save-directory (concat user-emacs-directory "drafts")
      message-kill-buffer-on-exit t
      message-signature "Best regards,\nSam\n"
      notmuch-fcc-dirs nil
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
;(add-hook 'scala-mode-hook 'flyspell-prog-mode-hook)
(add-hook 'scala-mode-hook 'highlight-symbol-at-point-hook)

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

(defun count-buffers (&optional display-anyway)
  ;http://www.cb1.com/~john/computing/emacs/lisp/startup/buffer-misc.el
  "Display or return the number of buffers."
  (interactive)
  (let ((buf-count (length (buffer-list))))
    (if (or (interactive-p) display-anyway)
    (message "%d buffers in this Emacs" buf-count)) buf-count))

(defun safe-kill-emacs ()
  "Only exit emacs if this is a small sesssion"
  (interactive)
  (if (< (count-buffers) 10)
      (save-buffers-kill-emacs)
    (message-box "use 'M-x save-buffers-kill-emacs'")))


; modified commands
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "C-x C-c") 'safe-kill-emacs)

; new bindings
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(global-set-key (kbd "s-f") 'find-name-dired)
(global-set-key (kbd "s-f") 'magit-find-file-completing-read)
(global-set-key (kbd "s-F") 'git-grep)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-n") 'ensime-search)
(global-set-key (kbd "s-q") 'describe-foo-at-point)
(global-set-key (kbd "s-h") 'highlight-symbol-at-point)

(add-hook 'text-mode-hook (lambda()(flyspell-mode 1))); (C-c $) for corrections

(require 'notmuch)
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(require 'notmuch-address)
(notmuch-address-message-insinuate)

; mu4e is not as good as notmuch, but might have to do for gmail syncing
;; (require 'mu4e)
;; (setq mu4e-mu-home "~/.mu"
;;       mu4e-maildir "~/Gmail"
;;       mu4e-attachment-dir "~/Downloads"
;;       mu4e-sent-messages-behavior 'delete
;;       mu4e-drafts-folder "/drafts"
;;       mu4e-sent-folder   "/all"
;;       mu4e-trash-folder  "/trash"
;;       mu4e-change-filenames-when-moving nil
;;       mu4e-get-mail-command nil
;;       mu4e-action-tags-header "X-Keywords"
;;       mu4e-get-mail-command "offlineimap")
;; (add-to-list 'mu4e-headers-actions '("tRetag message" . mu4e-action-retag-message) t)
;; (add-to-list 'mu4e-view-actions '("tRetag message" . mu4e-action-retag-message) t)
;; (add-to-list 'mu4e-view-actions '("bView in browser" . mu4e-action-view-in-browser) t)

(defun describe-foo-at-point ()
  ;;; http://www.emacswiki.org/emacs/DescribeThingAtPoint
  "Show the documentation of the Elisp function and variable near point.
	This checks in turn:
	-- for a function name where point is
	-- for a variable name where point is
	-- for a surrounding function call
	"
  (interactive)
  (let (sym)
    (cond ((setq sym (ignore-errors
		       (with-syntax-table emacs-lisp-mode-syntax-table
			 (save-excursion
			   (or (not (zerop (skip-syntax-backward "_w")))
			       (eq (char-syntax (char-after (point))) ?w)
			       (eq (char-syntax (char-after (point))) ?_)
			       (forward-sexp -1))
			   (skip-chars-forward "`'")
			   (let ((obj (read (current-buffer))))
			     (and (symbolp obj) (fboundp obj) obj))))))
	   (describe-function sym))
	  ((setq sym (variable-at-point)) (describe-variable sym))
	  ((setq sym (function-at-point)) (describe-function sym)))))

(require 'highlight-symbol)
(require 'ctags)

