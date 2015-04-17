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
;; (stage/functional sections) or a row of dots (primary modes).

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in emacs parameters
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ;;debug-on-error t
      enable-recursive-minibuffers t
      create-lockfiles nil
      make-backup-files nil
      load-prefer-newer t
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      indent-tabs-mode nil
      tab-width 4
      column-number-mode t
      scroll-error-top-bottom t
      show-trailing-whitespace t
      mail-user-agent 'message-user-agent
      user-mail-address "Sam.Halliday@gmail.com"
      user-full-name "Sam Halliday"
      send-mail-function 'smtpmail-send-it)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in packages that autoload
(setq show-paren-delay 0
      compilation-skip-threshold 2
      c-basic-offset 4
      source-directory (getenv "EMACS_SOURCE")
      nxml-slash-auto-complete-flag t
      sentence-end-double-space nil
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setup functions that are built-in to emacs
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(subword-mode 1)
(setenv "SBT_OPTS" (concat "-no-colors " (getenv "SBT_OPTS")))
(global-auto-revert-mode 1)
(substitute-key-definition
 ;; allows using SPACE when in the minibuffer
 'minibuffer-complete-word
 'self-insert-command
 minibuffer-local-completion-map)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(defun add-to-load-path (path)
  "Add PATH to LOAD-PATH if PATH exists."
  (when (file-exists-p path)
    (add-to-list 'load-path path)))
(add-to-load-path "/usr/local/share/emacs/site-lisp")
(add-to-load-path "/usr/share/org-mode/lisp")
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

;; TODO: try various obvious permutations on `function` => `package`
;; mapping, such as dropping `-mode` from the name
(defun required (function &optional force hook package)
  "`autoload' an interactive FUNCTION :symbol, installing if not present.

FORCE :boolean will use `require' instead of `autoload'.

Runs a HOOK :lambda when the file is loaded.

PACKAGE :symbol is used to workaround packages that have been
distributed under a different name than their function."
  (interactive)
  (let* ((sym (or package function))
         (name (symbol-name sym)))
    (unless (locate-library name)
      (package-install sym))
    (when hook
      (eval-after-load (locate-library name) hook))
    (if force
        (require function)
      (autoload function name nil 'interactive))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for generic interactive convenience methods.
;; Arguably could be uploaded to MELPA as package 'fommil-utils.
;; References included where shamelessly stolen.
(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[\t\s\n\r]\\{2,\\}" (- (point) 3))
      (hungry-delete-backward 1)
    (if (subword-mode)
        (subword-backward-kill 1)
      (backward-kill-word 1))))

(defun exit ()
  "Short hand for DEATH TO ALL PUNY BUFFERS!"
  (interactive)
  (if (daemonp)
      (message "You silly")
    (save-buffers-kill-emacs)))

(defun safe-kill-emacs ()
  "Only exit Emacs if this is a small session, otherwise prompt."
  (interactive)
  (if (daemonp)
      (delete-frame)
    (let ((count-buffers (length (buffer-list))))
      (if (< count-buffers 10)
          (save-buffers-kill-emacs)
        (message-box "use 'M-x exit'")))))

(defun kill-current-buffer-and-its-windows ()
  "Kill without confirm."
  (interactive)
  (kill-buffer-and-its-windows (current-buffer)))

(defun declare-buffer-bankruptcy()
  "Declare buffer bankruptcy and clean up everything."
  (interactive)
  (let ((clean-buffer-list-delay-general 0)
        (clean-buffer-list-delay-special 0))
    (clean-buffer-list)))

(defun flyspell-ignore-http-and-https ()
  ;; http://emacs.stackexchange.com/a/5435
  "Ignore anything starting with 'http' or 'https'."
  (save-excursion
    (forward-whitespace -1)
    (when (looking-at " ")
      (forward-char)
      (not (looking-at "https?\\b")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global modes that should be loaded in order to
;; make them immediately available.
(required 'midnight t (lambda()
                        (add-to-list 'clean-buffer-list-kill-regexps
                                     "\\`\\*magit.*\\*\\'")
                        (add-to-list 'clean-buffer-list-kill-never-regexps
                                     ".*\\*sbt.*")
                        (add-to-list 'clean-buffer-list-kill-never-regexps
                                     ".*\\*ENSIME-server.*")))
(required 'persistent-scratch t (lambda() (persistent-scratch-setup-default)))
(required 'autopair t (lambda() (autopair-global-mode)))
(required 'highlight-symbol t
          (lambda() (add-hook 'find-file-hook (lambda() (highlight-symbol-mode)))))

(setq guide-key/guide-key-sequence t)
(required 'guide-key t (lambda() (guide-key-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.
(required 'hungry-delete)
(required 'misc-cmds)
(required 'multiple-cursors)
(required 'git-gutter)

(setq magit-last-seen-setup-instructions "1.4.0")
(required 'magit nil (lambda()
                       (magit-auto-revert-mode -1)))
;;                       (magit-remove-popup-key 'magit-push-popup :actions ?P)))
;;                       (magit-change-popup-key 'magit-push-popup :actions ?e ?P)))

(required 'magit-find-file)
(required 'ctags-create-tags-table nil nil 'ctags)
(required 'turn-on-ctags-auto-update-mode nil nil 'ctags-update)
(required 'auto-complete-exuberant-ctags nil (lambda() (ac-exuberant-ctags-setup)))
(required 'company-mode nil nil 'company)
(required 'rainbow-mode)
(required 'flycheck)
(required 'yas-minor-mode nil (lambda() (yas-reload-all)) 'yasnippet)
(required 'elnode)
(required 'tidy-buffer nil nil 'tidy)
(required 'darkroom-mode nil nil 'darkroom)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(required 'erc)

(setq whitespace-style '(face trailing tab-mark lines-tail)
      whitespace-line-column 80)
(required 'whitespace)

(setq ispell-dictionary "british"
      flyspell-prog-text-faces '(font-lock-doc-face))
(required 'flyspell nil (lambda()
                          (put 'text-mode
                               'flyspell-mode-predicate
                               'flyspell-ignore-http-and-https)))
(add-hook 'text-mode-hook (lambda() (flyspell-mode 1)))

(setq ag-reuse-window 't)
(required 'ag)
(setq projectile-use-native-indexing t
      projectile-use-git-grep t)
(required 'projectile nil (lambda() (require 'vc-git)))

(required 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(required 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

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
;;(global-set-key (kbd "s-f") 'magit-find-file-completing-read)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-F") 'projectile-ag)
;; projectile-find-tag is too slow: https://github.com/bbatsov/projectile/issues/668
(global-set-key (kbd "M-.") 'find-tag)
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
(setq smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      message-auto-save-directory (concat user-emacs-directory "drafts")
      message-kill-buffer-on-exit t
      message-signature "Best regards,\nSam\n"
      notmuch-fcc-dirs nil
      notmuch-search-oldest-first nil
      notmuch-address-command "google-contacts"
      notmuch-saved-searches '(("inbox" . "tag:inbox")
                               ("unread" . "tag:unread")
                               ("flagged" . "tag:flagged")))
(required 'notmuch nil (lambda()
                         (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
                         (add-hook 'mml-mode (lambda() (auto-fill-mode)))))
(required 'notmuch-address nil (lambda() (notmuch-address-message-insinuate)))


;;..............................................................................
;; elisp
(defun describe-foo-at-point ()
  ;;; http://www.emacswiki.org/emacs/DescribeThingAtPoint
  "Show the documentation of the function and variable near point."
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

(defun elisp-find-tag-or-find-func ()
  ;;; Could be a lot smarter for non-function symbols (variables, faces, packages, etc)
  "Use `find-tag' to find symbol at point, falling back to `find-func' features."
  (interactive)
  (let* ((fun (function-called-at-point)))
    (if (and fun (find-function-noselect fun (not source-directory)))
        (find-function-do-it fun nil 'switch-to-buffer)
      (find-tag (symbol-name (symbol-at-point))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (local-set-key (kbd "M-.") 'elisp-find-tag-or-find-func)
            (local-set-key (kbd "s-q") 'describe-foo-at-point)
            (setq indent-tabs-mode nil tab-width 4 c-basic-offset 4)
            (rainbow-mode)
            (when (fboundp 'prettify-symbols-mode) ;; added in 24.4
              (prettify-symbols-mode))
            (flyspell-prog-mode)
            (eldoc-mode)
            (flycheck-mode)
            (yas-minor-mode)
            (company-mode)
            (turn-on-ctags-auto-update-mode)))


;;..............................................................................
;; Scala
(setq scala-indent:use-javadoc-style t
      popup-complete-enabled-modes '(scala-mode)
      ensime-default-buffer-prefix "ENSIME-"
      scala-outline-popup-select 'closest)
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

;;(autoload 'maker:find-root "maker-mode")
(defun sbt-or-maker-command ()
  "Find and launch `maker-command', falling back to `sbt-command'."
  (interactive)
  (if (maker:find-root)
      (call-interactively 'maker-command)
    (call-interactively 'sbt-command)))

(add-hook 'scala-mode-hook
          (lambda()
            (projectile-mode)
            ;; TODO https://github.com/hvesalai/scala-mode2/issues/75
            (set (make-local-variable 'forward-word) 'scala-syntax:forward-token)
            ;; TODO: make whitespace warning project-specific
            (set (make-local-variable 'whitespace-line-column) 116)
            (whitespace-mode)
            (flyspell-prog-mode)
            (highlight-symbol-mode)
            (local-set-key (kbd "s-n") 'ensime-search)
            (local-set-key (kbd "s-i") 'ensime-print-type-at-point)
            (local-set-key (kbd "s-o") 'scala-outline-popup)
            (local-set-key (kbd "RET")
                           (lambda()
                             (interactive)
                             (newline-and-indent)
                             (scala-indent:insert-asterisk-on-multiline-comment)))
             ;;(local-set-key (kbd "C-<right>") 'scala-syntax:forward-token)
            ;;(local-set-key (kbd "C-<left>") 'scala-syntax:backward-token)
            ;;(local-set-key (kbd "C-c c") 'sbt-or-maker-command)
            (local-set-key (kbd "C-c c") 'sbt-command)
            (local-set-key (kbd "C-c e") 'next-error)
            (required 'ensime t)
            (ensime-mode 1)
            (required 'scala-outline-popup t)
            (git-gutter-mode)
            (define-key popup-isearch-keymap (kbd "s-o") 'popup-isearch-cancel)))


(defun ensime-template-wordspec ()
  "ENSIME template for a ScalaCheck WordSpec style test."
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
  (sbt-command)
  (ensime))

(add-hook 'sbt-mode-hook (lambda()
                           ;;(setq compilation-skip-threshold 1)
                           (local-set-key (kbd "C-c c") 'sbt-command)
                           (local-set-key (kbd "C-c e") 'next-error)
                           (local-set-key (kbd "M-RET") 'comint-accumulate)))
(add-hook 'maker-mode-hook (lambda()
                             (local-set-key (kbd "C-c c") 'maker-command)
                             (local-set-key (kbd "C-c e") 'next-error)))
(add-hook 'dired-mode-hook (lambda()
                             (projectile-mode)
                             ;; a workflow optimisation too far?
                             (local-set-key (kbd "C-c c") 'sbt-command)
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
            (projectile-mode)
            (company-mode)
            (local-set-key (kbd "C-c e") 'next-error)
            (turn-on-ctags-auto-update-mode)))


;;..............................................................................
;; C
(add-hook 'c-mode-hook (lambda()
                         (yas-minor-mode)
                         (projectile-mode)
                         (company-mode)
                         (turn-on-ctags-auto-update-mode)))

;;..............................................................................
;; org-mode
;; 'org is a system install and has a default binding to .org files
;;(required 'org)
;; ox-taskjuggler isn't available on MELPA, must be a system install
(when (locate-library "ox-taskjuggler")
  (require 'ox-taskjuggler))
(required 'markdown-mode)
(defun pandoc ()
  "If a hidden .pandoc file exists for the file, run it."
  ;; this effectively replaces pandoc-mode for me
  (interactive)
  (let ((command-file (concat (file-name-directory buffer-file-name)
                              "." (file-name-nondirectory buffer-file-name)
                              ".pandoc")))
    (when (file-exists-p command-file)
      (shell-command command-file))))

(defun markup-common-hooks()
  (auto-fill-mode)
  (yas-minor-mode)
  (local-set-key (kbd "C-c c") 'pandoc))
(add-hook 'org-mode-hook (lambda()
                           (markup-common-hooks)))
(add-hook 'markdown-mode-hook 'markup-common-hooks)

;;..............................................................................
;; R
(required 'ess-site nil nil 'ess)
;; bad packaging means we have to manually setup R-mode
(autoload 'R-mode "ess-site" nil t)
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))


;;..............................................................................
;; User Site Local
(let ((user-local (concat user-emacs-directory "local.el")))
  (when (file-exists-p user-local)
    (load user-local)))

;;; .emacs ends here
