;;; .emacs --- Emacs configuration
;; Copyright (C) 2014 - 2015 Sam Halliday (fommil)

;; Author: Sam Halliday <sam.halliday@gmail.com>
;; URL: https://github.com/fommil/unix/blob/master/.emacs

;;; Commentary:
;;
;; Personalised Emacs configuration for the following primary uses;
;;
;;   - Scala
;;   - R
;;   - Java
;;   - C
;;   - elisp
;;   - org-mode (with LaTeX)
;;   - email
;;
;; This file is broken into sections which gather similar features or
;; modes together. Sections are delimited by a row of semi-colons
;; (stage/functional sections) or a row of hyphens (major modes).

;;; Code:
;; This section is for global settings that do not require packages to
;; be installed prior to setting the variable. No need to document
;; what each does, as it is a simple case of looking up the symbol.
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ;;debug-on-error t
      show-paren-delay 0
      create-lockfiles nil
      make-backup-files nil
      load-prefer-newer t
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      indent-tabs-mode nil
      compilation-skip-threshold 2
      tab-width 4
      column-number-mode t
      c-basic-offset 4
      whitespace-style '(face trailing tab-mark lines-tail)
      whitespace-line-column 80
      nxml-slash-auto-complete-flag t
      scala-indent:use-javadoc-style t
      popup-complete-enabled-modes '(scala-mode)
      scroll-error-top-bottom t
      show-trailing-whitespace t
      ispell-dictionary "british"
      flyspell-prog-text-faces '(font-lock-doc-face)
      sentence-end-double-space nil
      ensime-default-buffer-prefix "ENSIME-"
      scala-outline-popup-select 'closest
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setup functions that are built-in to emacs
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(subword-mode 1)
(setenv "SBT_OPTS" (concat "-no-colors" (getenv "SBT_OPTS")))
(global-auto-revert-mode 1)
(substitute-key-definition
 ;; allows using SPACE when in the minibuffer
 'minibuffer-complete-word
 'self-insert-command
 minibuffer-local-completion-map)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(defun add-to-load-path (path)
  (when (file-exists-p path)
    (add-to-list 'load-path path)))
(add-to-load-path "/usr/local/share/emacs/site-lisp")
(add-to-load-path (concat user-emacs-directory "lisp"))

(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(fset 'html-mode 'nxml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setting up the MELPA package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun required (package &optional force hook melpa)
  "`autoload' a PACKAGE :symbol, installing if not present,
and running a HOOK :lambda when the file is loaded. FORCE :boolean
will use `require' instead of `autoload'. MELPA :symbol can be
used to specify the package name on the package manager for
awkward packages."
  (interactive)
  (unless (locate-library (symbol-name package))
    (if melpa
        (package-install melpa)
      (package-install package)))
  (when hook
    (eval-after-load (locate-library (symbol-name package)) hook))
  (if force
      (require package)
    (autoload package (symbol-name package))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global modes that should be loaded in order to
;; make them immediately available.
(required 'darcula-theme t (lambda() (set-frame-font "Inconsolata-16")))
(required 'autopair t (lambda()
                        (autopair-global-mode)))
(required 'midnight t (lambda()
                        (add-to-list 'clean-buffer-list-kill-regexps
                                     "\\`\\*magit.*\\*\\'")
                        (add-to-list 'clean-buffer-list-kill-never-buffer-names
                                     "*ensime-events*")
                        (add-to-list 'clean-buffer-list-kill-never-regexps
                                     ".*\\*sbt.*")
                        (add-to-list 'clean-buffer-list-kill-never-regexps
                                     ".*\\*ENSIME.*")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.
(required 'hungry-delete)
(required 'misc-cmds)
(required 'multiple-cursors)
(required 'darkroom)
(required 'git-gutter)
(required 'magit nil (lambda() (magit-auto-revert-mode -1)))
(required 'magit-find-file)
(required 'highlight-symbol)
(required 'ctags)
(required 'auto-complete-exuberant-ctags nil
          (lambda() (ac-exuberant-ctags-setup)))
(required 'ctags-update)
(required 'erc)
(required 'rainbow-mode)
(required 'pretty-lambdada)
(required 'whitespace)

(defun flyspell-ignore-http-and-https ()
  ;; http://emacs.stackexchange.com/a/5435
  "Ignore anything starting with 'http' or 'https'."
  (save-excursion
    (forward-whitespace -1)
    (when (looking-at " ")
      (forward-char)
      (not (looking-at "https?\\b")))))
(required 'flyspell nil (lambda()
                          (put 'text-mode
                               'flyspell-mode-predicate
                               'flyspell-ignore-http-and-https)))
(add-hook 'text-mode-hook (lambda()
                            (flyspell-mode 1)))
(required 'flycheck)
(required 'markdown-mode)
(add-hook 'markdown-mode-hook (lambda()
                                (yas-minor-mode)))
(required 'yasnippet)
(required 'elnode)
(required 'tidy)
(required 'persistent-scratch nil (lambda()
                                    (persistent-scratch-setup-default)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for generic interactive convenience methods.
;; Arguably could be uploaded to MELPA as package 'fommil-utils.
;; References included where shamelessly stolen.
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
    (if (subword-mode)
        (subword-backward-kill 1)
      (backward-kill-word 1))))

(defun git-grep (search)
  ;; https://www.ogre.com/node/447
  "git-grep the entire current repo"
  (interactive (list (completing-read
                      "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -P -n \""
                     search
                     "\" `git rev-parse --show-toplevel`"))
  (other-window 1))

(defun count-buffers (&optional display-anyway)
  ;;http://www.cb1.com/~john/computing/emacs/lisp/startup/buffer-misc.el
  "Display or return the number of buffers."
  (let ((buf-count (length (buffer-list))))
    (if (or (interactive-p) display-anyway)
        (message "%d buffers in this Emacs" buf-count)) buf-count))

(defun exit ()
  "short hand for DEATH TO ALL PUNY BUFFERS"
  (interactive)
  (save-buffers-kill-emacs))

(defun safe-kill-emacs ()
  "Only exit emacs if this is a small sesssion, otherwise prompt.
Excellent for people, like @fommil, who are forever typing C-x
C-c by accident."
  (interactive)
  (if (< (count-buffers) 10)
      (save-buffers-kill-emacs)
    (message-box "use 'M-x exit'")))

(defun close-and-kill-next-pane ()
  ;; http://www.emacswiki.org/emacs/KillingBuffers
  "If there are multiple windows, then close the other pane and
kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun kill-current-buffer-and-its-windows ()
  "Kill without confirm"
  (interactive)
  (kill-buffer-and-its-windows (current-buffer)))

(defun declare-buffer-bankruptcy()
  "Declare buffer bankruptcy and clean up everything"
  (interactive)
  (let ((clean-buffer-list-delay-general 0)
        (clean-buffer-list-delay-special 0))
    (clean-buffer-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for overriding common emacs keybindings with tweaks.
(global-unset-key (kbd "C-z")) ;; I hate you so much C-z
(global-set-key (kbd "C-x C-c") 'safe-kill-emacs)
(global-set-key (kbd "C-x k") 'kill-buffer-and-its-windows)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "M-<left>") 'subword-backward)
(global-set-key (kbd "C-<left>") 'subword-backward)
(global-set-key (kbd "M-<right>") 'subword-forward)
(global-set-key (kbd "C-<right>") 'subword-forward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for defining commonly invoked commands that deserve
;; a short binding instead of their packager's preferred binding.
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "s-f") 'magit-find-file-completing-read)
(global-set-key (kbd "s-F") 'git-grep)
(global-set-key (kbd "s-b") 'magit-blame-mode)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-h") 'highlight-symbol-at-point)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and configuring more involved
;; task/function-specific modes, organised by task or function.
;;..............................................................................
;; Email
(required 'notmuch nil (lambda()
                         (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
                         (add-hook 'mml-mode (lambda() (auto-fill-mode)))))
(required 'notmuch-address nil (lambda()
                                 (notmuch-address-message-insinuate)))


;;..............................................................................
;; elisp
(defun describe-foo-at-point ()
  ;;; http://www.emacswiki.org/emacs/DescribeThingAtPoint
  "Show the documentation of the Elisp function and variable near point.
        This checks in turn:
        -- for a function name where point is
        -- for a variable name where point is
        -- for a surrounding function call"
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

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (local-set-key (kbd "M-.") 'find-function-at-point)
            (local-set-key (kbd "s-q") 'describe-foo-at-point)
            (setq indent-tabs-mode nil tab-width 4 c-basic-offset 4)
            (rainbow-mode)
            (pretty-lambda-mode)
            (flyspell-prog-mode)
            (turn-on-eldoc-mode)
            (flycheck-mode)
            (yas-minor-mode)
            (turn-on-ctags-auto-update-mode)))


;;..............................................................................
;; Scala
(let* ((local-ensime (concat user-emacs-directory "ensime-emacs")))
  (when (file-exists-p local-ensime)
    (add-to-list 'load-path local-ensime)))
(required 'ensime nil
          (lambda()
            (setq ensime-goto-test-config-defaults
                  (plist-put (plist-put ;; TODO: clean up double plist-put
                              ensime-goto-test-config-defaults
                              :test-class-suffixes '("Spec" "Test" "Check"))
                             :test-template-fn 'ensime-template-wordspec))))
(required 'scala-mode2)
(required 'scala-outline-popup)
(required 'sbt-mode)
(required 'maker-mode)

(defun sbt-or-maker-command ()
  "Find and launch maker-command, falling back to sbt-command"
  (interactive)
  (if (maker:find-root)
      (call-interactively 'maker-command)
    (call-interactively 'sbt-command)))

(add-hook 'scala-mode-hook
          (lambda()
            (ensime-scala-mode-hook)
            (set (make-local-variable 'forward-word) 'scala-syntax:forward-token) 
            ;; TODO: make whitespace warning project-specific
            (set (make-local-variable 'whitespace-line-column) 116)
            (whitespace-mode)
            (flyspell-prog-mode)
            (highlight-symbol-mode)
            (local-set-key (kbd "s-n") 'ensime-search)
            (local-set-key (kbd "s-i") 'ensime-print-type-at-point)
            (local-set-key (kbd "s-o") 'scala-outline-popup)
            (define-key popup-isearch-keymap (kbd "s-o") 'popup-isearch-cancel)
            (local-set-key (kbd "RET")
                           (lambda()
                             (interactive)
                             (newline-and-indent)
                             (scala-indent:insert-asterisk-on-multiline-comment)))
             ;;(local-set-key (kbd "C-<right>") 'scala-syntax:forward-token)
            ;;(local-set-key (kbd "C-<left>") 'scala-syntax:backward-token)
            (local-set-key (kbd "C-c c") 'sbt-or-maker-command)
            (local-set-key (kbd "C-c e") 'next-error)))

(defun ensime-template-wordspec ()
  ""
  "package %TESTPACKAGE%
import org.scalatest._

class %TESTCLASS% extends WordSpec with Matchers {
  \"%IMPLCLASS%\" should {
    \"have a test!\" in {
      fail(\"no test\")
    }
  }
}
")

(defun scala-start()
  "Easy way to initialise All The Things for a Scala project"
  (interactive)
  (sbt-or-maker-command)
  (ensime))

(add-hook 'sbt-mode-hook (lambda()
                           (setq compilation-skip-threshold 1)
                           (local-set-key (kbd "C-c c") 'sbt-command)
                           (local-set-key (kbd "C-c e") 'next-error)
                           (local-set-key (kbd "M-RET") 'comint-accumulate)))
(add-hook 'maker-mode-hook (lambda()
                             (local-set-key (kbd "C-c c") 'maker-command)
                             (local-set-key (kbd "C-c e") 'next-error)))
(add-hook 'dired-mode-hook (lambda()
                             ;; a workflow optimisation too far?
                             (local-set-key (kbd "C-c c") 'sbt-or-maker-command)
                             (local-set-key (kbd "C-c e") 'next-error)))


;;..............................................................................
;; Java: watch out for https://github.com/ensime/ensime-server/issues/345
(add-hook 'java-mode-hook
          (lambda()
            ;; http://www.emacswiki.org/emacs/IndentingJava
            ;; java-mode resets all the default tab settings. dick.
            (setq indent-tabs-mode nil
                  tab-width 4
                  c-basic-offset 4)
            (yas-minor-mode)
            (turn-on-ctags-auto-update-mode)))

;;..............................................................................
;; C
(add-hook 'c-mode-hook (lambda()
                         (yas-minor-mode)
                         (turn-on-ctags-auto-update-mode)))


;;..............................................................................
;; org-mode
(required 'org)
(defun pandoc ()
  "If a hidden .pandoc file exists for the file, run it"
  ;; this effectively replaces pandoc-mode for me
  (interactive)
  (let ((command-file (concat (file-name-directory buffer-file-name)
                              "." (file-name-nondirectory buffer-file-name)
                              ".pandoc")))
    (when (file-exists-p command-file)
      (shell-command command-file))))

(add-hook 'org-mode-hook (lambda()
                           (auto-fill-mode)
                           (yas-minor-mode)
                           (local-set-key (kbd "C-c c") 'pandoc)))


;;..............................................................................
;; R
(required 'ess-site t nil 'ess)
;; bad packaging means we have to force load ess-site

;;; .emacs ends here
