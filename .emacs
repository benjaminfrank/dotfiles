(setq inhibit-startup-screen t
      show-paren-delay 0
      create-lockfiles nil
      make-backup-files nil
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      indent-tabs-mode nil
      tab-width 4
      column-number-mode t
      c-basic-offset 4
      scala-indent:use-javadoc-style t ; to match scalariform
      scroll-error-top-bottom t
      show-trailing-whitespace t
      ispell-dictionary "british"
      sentence-end-double-space nil
      ensime-typecheck-when-idle nil
      ensime-default-buffer-prefix "ENSIME-"
      debug-on-error t
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
;(mouse-avoidance-mode 'banish) ; https://github.com/ensime/ensime-server/issues/545
(global-auto-revert-mode 1)

(substitute-key-definition
 ;; allows using SPACE when in the minibuffer
 'minibuffer-complete-word
 'self-insert-command
 minibuffer-local-completion-map)

(if (file-exists-p "/usr/local/share/emacs/site-lisp")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp"))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

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
(magit-auto-revert-mode -1) ; we have auto-revert already
(required 'magit-find-file)
(required 'scala-mode2)
(required 'autopair)
(autopair-global-mode)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

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
      (hungry-delete-backward 1)
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

; keep buffers under control
(required 'midnight)
(add-to-list 'clean-buffer-list-kill-regexps "\\`\\*magit.*\\*\\'")
(add-to-list 'clean-buffer-list-kill-never-buffer-names "*ensime-events*")
; TODO: is there a clean way to add-to-list a list?
(add-to-list 'clean-buffer-list-kill-never-regexps ".*\\*sbt.*")
(add-to-list 'clean-buffer-list-kill-never-regexps ".*\\*ENSIME.*")

(defun declare-buffer-bankruptcy()
  "Declare buffer bankruptcy and clean up everything"
  (interactive)
  (let ((clean-buffer-list-delay-general 0)
	(clean-buffer-list-delay-special 0))
    (clean-buffer-list)))


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
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)


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

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(required 'highlight-symbol)
(required 'ctags)
(required 'auto-complete-exuberant-ctags)
(required 'ctags-update)
(ac-exuberant-ctags-setup)

(required 'erc)

(required 'flycheck)
(add-hook 'emacs-lisp-mode-hook '(lambda ()
				   (local-set-key (kbd "M-.") 'find-function-at-point)
				   (flycheck-mode)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-ctags-auto-update-mode)


; Allows ensime-dev. Don't forget to
;   rm -rf ~/.emacs.d/elpa/ensime-*
(let* ((local-ensime (concat user-emacs-directory "ensime")))
  (if (file-exists-p local-ensime)
      (progn
	(add-to-list 'load-path local-ensime)
	(require 'ensime))
    (required 'ensime)))
(required 'whitespace)
(required 'sbt-mode)

;; https://github.com/auto-complete/popup-el/issues/77
(require 'popup-complete)
(setq complete-in-region-use-popup t)
(require 'maker-mode)

(defun sbt-or-maker-command ()
  "Find and launch maker-command, falling back to sbt-command"
  (interactive)
  (if (maker:find-root)
      (call-interactively 'maker-command)
    (call-interactively 'sbt-command)))

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;(add-hook 'scala-mode-hook 'turn-on-ctags-auto-update-mode)
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

	     (local-set-key (kbd "C-c C-c") 'sbt-or-maker-command)
	     (local-set-key (kbd "C-c C-e") 'next-error)
	     (local-set-key (kbd "C-x '") 'sbt-run-previous-command)))

(setq ensime-goto-test-config-defaults
      ; TODO: is there a clean way to plist-put a list?
      (plist-put (plist-put
                  ensime-goto-test-config-defaults
                  :test-class-suffixes '("Spec" "Test" "Check"))
                 :test-template-fn 'ensime-goto-test--test-template-scalatest-1))

; the defaults have settings for "ensime-server" that I don't like
;(setq ensime-goto-test-configs nil)

;; TODO ensime-server dev restart cycle hotkey

(defun ensime-developer-restart()
  (interactive)
  (kill-buffer-and-its-windows "*ENSIME-ensime*")
  (sbt-command "publishLocal")
  (ensime))

(defun scala-start()
  (interactive)
  (sbt-command "test:compile")
  (ensime))


(add-hook 'sbt-mode-hook '(lambda ()
			    (setq compilation-skip-threshold 1)
			    (local-set-key (kbd "C-a") 'comint-bol)
			    (local-set-key (kbd "M-RET") 'comint-accumulate)))


(add-hook 'java-mode-hook '(lambda()
			     ;; http://www.emacswiki.org/emacs/IndentingJava
			     ;; java-mode resets all the default tab settings. dick.
			     (setq indent-tabs-mode nil
				   tab-width 4
				   c-basic-offset 4)
			     (turn-on-ctags-auto-update-mode)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-ctags-auto-update-mode)

;(require 'speedbar)
;(speedbar-add-supported-extension "\\.scala")
;(add-to-list 'speedbar-fetch-etags-parse-list
;     '("\\.scala" . speedbar-parse-c-or-c++tag))

;; HACK: for ensime dev
;;(find-file "~/Projects/ensime/src/main/elisp/ensime.el")
;;(ensime)

