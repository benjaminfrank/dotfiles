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
      ensime-typecheck-when-idle nil
      ;;debug-on-error t
      ediff-window-setup-function 'ediff-setup-windows-plain
      erc-hide-list '("JOIN" "PART" "QUIT")
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
(set-frame-font "Inconsolata-16")

(global-auto-revert-mode 1)

(if (file-exists-p "/usr/local/share/emacs/site-lisp")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp"))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(defun required (package)
  (unless (require package nil 'no-error)
    (package-install package)
    (require package)))

(required 'solarized-theme)
(load-theme 'solarized-dark 'NO-CONFIRM)

(required 'hungry-delete)
(required 'misc-cmds)
(required 'git-gutter)
(required 'magit)
(required 'magit-find-file)
(required 'scala-mode2)
(required 'autopair)
(autopair-global-mode)

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
  (grep-find (concat "git --no-pager grep -P -n " search " `git rev-parse --show-toplevel`"))
  (other-window 1))

(defun count-buffers (&optional display-anyway)
  ;http://www.cb1.com/~john/computing/emacs/lisp/startup/buffer-misc.el
  "Display or return the number of buffers."
  (interactive)
  (let ((buf-count (length (buffer-list))))
    (if (or (interactive-p) display-anyway)
    (message "%d buffers in this Emacs" buf-count)) buf-count))

(defun exit ()
  "short hand for death to all buffers"
  (interactive)
  (save-buffers-kill-emacs))

(defun safe-kill-emacs ()
  "Only exit emacs if this is a small sesssion"
  (interactive)
  (if (< (count-buffers) 10)
      (save-buffers-kill-emacs)
    (message-box "use 'M-x exit'")))

(defun close-and-kill-next-pane ()
  ;; http://www.emacswiki.org/emacs/KillingBuffers
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun kill-current-buffer-and-its-windows ()
  "Kill without confirm"
  (interactive)
  (kill-buffer-and-its-windows (current-buffer)))

; modified commands
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x k") 'kill-buffer-and-its-windows)
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
(global-set-key (kbd "s-q") 'describe-foo-at-point)
(global-set-key (kbd "s-h") 'highlight-symbol-at-point)
(global-set-key (kbd "s-o") 'close-and-kill-next-pane)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

(add-hook 'text-mode-hook (lambda()(flyspell-mode 1))); (C-c $) for corrections

(required 'notmuch)
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(required 'notmuch-address)
(notmuch-address-message-insinuate)

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

(required 'highlight-symbol)
(required 'ctags)
(required 'auto-complete-exuberant-ctags)
(required 'ctags-update)
(ac-exuberant-ctags-setup)

(required 'erc)

(required 'flycheck)
(add-hook 'emacs-lisp-mode-hook '(lambda () (flycheck-mode)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-ctags-auto-update-mode)

;(add-to-list 'load-path (concat user-emacs-directory "ensime"))
(required 'ensime)
(required 'whitespace)
(required 'sbt-mode)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'turn-on-ctags-auto-update-mode)
(add-hook 'scala-mode-hook
	  '(lambda ()
					;	     (make-local-variable 'before-save-hook)
					;	     (add-hook 'before-save-hook 'whitespace-cleanup)
	     (make-local-variable 'forward-word)
	     (setq forward-word 'scala-syntax:forward-token)

	     (highlight-symbol-mode)
	     (local-set-key (kbd "s-n") 'ensime-search)
	     (local-set-key (kbd "RET") '(lambda ()
					   (interactive)
					   (newline-and-indent)
					   (scala-indent:insert-asterisk-on-multiline-comment)))

	     (local-set-key (kbd "C-c C-c") 'sbt-command)
	     (local-set-key (kbd "C-x '") 'sbt-run-previous-command)))

;; TODO ensime-server dev restart cycle hotkey

(defun ensime-developer-restart()
  (interactive)
  (kill-buffer-and-its-windows "*inferior-ensime-server-ensime*")
  (sbt-command "publishLocal")
  (ensime))

(add-hook 'sbt-mode-hook '(lambda ()
			    (setq compilation-skip-threshold 1)
			    (local-set-key (kbd "C-a") 'comint-bol)
			    (local-set-key (kbd "M-RET") 'comint-accumulate)))


(add-hook 'java-mode-hook '(lambda()
			     (make-local-variable 'before-save-hook)
			     (add-hook 'before-save-hook 'whitespace-cleanup)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-ctags-auto-update-mode)

;(require 'speedbar)
;(speedbar-add-supported-extension "\\.scala")
;(add-to-list 'speedbar-fetch-etags-parse-list
;     '("\\.scala" . speedbar-parse-c-or-c++tag))

;; HACK: for ensime dev
;;(find-file "~/Projects/ensime/src/main/elisp/ensime.el")
;;(ensime)
