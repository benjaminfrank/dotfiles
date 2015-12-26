;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014 - 2015 Sam Halliday (fommil)
;; License: http://www.gnu.org/licenses/gpl.html

;; URL: https://github.com/fommil/dotfiles/blob/master/.emacs.d/init.el

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
;; High Priority Site Local
(load (concat user-emacs-directory "local-preinit.el") 'no-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in emacs parameters
(setq
 inhibit-startup-screen t
 initial-scratch-message nil
 enable-local-variables t
 create-lockfiles nil
 make-backup-files nil
 ;;load-prefer-newer t ;; WORKAROUND Debian bug
 column-number-mode t
 scroll-error-top-bottom t
 scroll-margin 15
 user-full-name "Sam Halliday")

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in packages that autoload
(setq
 help-window-select t
 show-paren-delay 0.5
 dabbrev-case-fold-search nil
 tags-case-fold-search nil
 tags-revert-without-query t
 tags-add-tables nil
 compilation-skip-threshold 2
 compilation-scroll-output 'first-error
 source-directory (getenv "EMACS_SOURCE")
 org-confirm-babel-evaluate nil
 nxml-slash-auto-complete-flag t
 sentence-end-double-space nil
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "sensible-browser"
 ediff-window-setup-function 'ediff-setup-windows-plain)

(setq-default
 c-basic-offset 4)

(add-hook 'prog-mode-hook
          (lambda() (setq show-trailing-whitespace t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setup functions that are built-in to emacs
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1) ;; show-smartparens is too slow
(global-auto-revert-mode 1)

(electric-indent-mode 0)
(remove-hook 'post-self-insert-hook
             'electric-indent-post-self-insert-function)
(global-auto-composition-mode 0)
(auto-encryption-mode 0)
(tooltip-mode 0)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(defun add-to-load-path (path)
  "Add PATH to LOAD-PATH if PATH exists."
  (when (file-exists-p path)
    (add-to-list 'load-path path)))
(add-to-load-path (concat user-emacs-directory "lisp"))


(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
;; WORKAROUND http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16449
(add-hook 'nxml-mode-hook (lambda() (flyspell-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setting up the MELPA package manager
(require 'package)
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package subword
  :ensure nil
  :diminish subword-mode
  :config (global-subword-mode 1))

(use-package dired
  :ensure nil
  :config
  ;; a workflow optimisation too far?
  (bind-key "C-c c" 'sbt-command dired-mode-map)
  (bind-key "C-c e" 'next-error dired-mode-map))

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

(defun unfill-paragraph (&optional region)
  ;; http://www.emacswiki.org/emacs/UnfillParagraph
  "Transforms a paragraph in REGION into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun unfill-buffer ()
  "Unfill the buffer for function `visual-line-mode'."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region 0 (point-max))))

(defun revert-buffer-no-confirm ()
  ;; http://www.emacswiki.org/emacs-en/download/misc-cmds.el
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[ \t\n\r\l]\\{2,\\}" (- (point) 3))
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
      ;; intentionally not save-buffers-kill-terminal as it has an
      ;; impact on other client sessions.
      (delete-frame)
    (let ((count-buffers (length (buffer-list))))
      (if (< count-buffers 10)
          (save-buffers-kill-emacs)
        (message-box "use 'M-x exit'")))))

(defun declare-buffer-bankruptcy ()
  "Declare buffer bankruptcy and clean up everything using `midnight'."
  (interactive)
  (let ((clean-buffer-list-delay-general 0)
        (clean-buffer-list-delay-special 0))
    (clean-buffer-list)))

(defun company-or-dabbrav-complete ()
  "Force a `company-complete', falling back to `dabbrev-expand'."
  (interactive)
  (if company-mode
      (company-complete)
    (call-interactively 'dabbrev-expand)))

(defun sp-restrict-c (sym)
  "Smartparens restriction on `SYM' for C-derived parenthesis."
  (sp-restrict-to-pairs-interactive "{([" sym))

(defun plist-merge (&rest plists)
  "Create a single property list from all PLISTS.
Inspired by `org-combine-plists'."
  (let ((rtn (pop plists)))
    (dolist (plist plists rtn)
      (setq rtn (plist-put rtn
                           (pop plist)
                           (pop plist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global modes that should be loaded in order to
;; make them immediately available.
(use-package midnight
  :config
  (add-to-list 'clean-buffer-list-kill-regexps "\\`\\*magit.*\\*\\'")
  (add-to-list 'clean-buffer-list-kill-never-regexps ".*\\*sbt.*")
  (add-to-list 'clean-buffer-list-kill-never-regexps ".*\\*ENSIME-server.*"))

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

(use-package projectile
  :demand
  ;; nice to have it on the modeline
  :init
  (setq projectile-use-git-grep t)
  :config
  (projectile-global-mode)
  :bind
  (("s-f" . projectile-find-file)
   ("s-F" . projectile-ag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

(use-package hungry-delete
  :commands hungry-delete)

(use-package git-gutter
  :commands git-gutter-mode)

(use-package magit
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package git-timemachine
  :commands git-timemachine
  :init (setq
         git-timemachine-abbreviation-length 4))

(use-package etags-select
  :commands etags-select-find-tag)

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil))

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode)

(use-package flycheck
  :diminish flycheck-mode
  :commands flycheck-mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

;; DEPRECATED https://github.com/mineo/yatemplate/issues/4
(defvar-local yatemplate-owner user-full-name
  "The copyright owner for the buffer.
Particularly useful when combined with `dir-locals.el'.")
(defvar-local yatemplate-license "http://www.gnu.org/licenses/gpl.html"
  "The license (usually a URL) for the buffer.
It is always better to explicitly list the license per file than
to refer to the LICENSE file. Particularly useful when combined
with `dir-locals.el'.")
(put 'yatemplate-owner 'safe-local-variable #'stringp)
(put 'yatemplate-license 'safe-local-variable #'stringp)

(use-package yatemplate
  :defer 2 ;; WORKAROUND https://github.com/mineo/yatemplate/issues/3
  :config
  (auto-insert-mode)
  (setq auto-insert-alist nil)
  (yatemplate-fill-alist))

(use-package writeroom-mode
  ;; BUGs to be aware of:
  ;; https://github.com/joostkremers/writeroom-mode/issues/18
  ;; https://github.com/company-mode/company-mode/issues/376
  ;;:diminish writeroom-mode
  :commands writeroom-mode)

(use-package whitespace
  :commands whitespace-mode
  :diminish whitespace-mode
  :init
  ;; BUG: https://emacs.stackexchange.com/questions/7743
  (put 'whitespace-line-column 'safe-local-variable #'integerp)
  (setq whitespace-style '(face trailing tabs lines-tail)
        whitespace-line-column 80))
(defun whitespace-mode-with-local-variables ()
  "A variant of `whitespace-mode' that can see local variables."
  ;; WORKAROUND https://emacs.stackexchange.com/questions/7743
  (add-hook 'hack-local-variables-hook 'whitespace-mode nil t))

(use-package flyspell
  :commands flyspell-mode
  :diminish flyspell-mode
  :init (setq
         ispell-dictionary "british"
         flyspell-prog-text-faces '(font-lock-doc-face))
  :config
  (put 'text-mode
       'flyspell-mode-predicate
       'flyspell-ignore-http-and-https))

(defun flyspell-ignore-http-and-https ()
  ;; http://emacs.stackexchange.com/a/5435
  "Ignore anything starting with 'http' or 'https'."
  (save-excursion
    (forward-whitespace -1)
    (when (looking-at " ")
      (forward-char)
      (not (looking-at "https?\\b")))))

(use-package idomenu
  :commands idomenu
  :bind ("M-i" . idomenu))

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :commands rainbow-delimiters-mode)

(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; nice whitespace / indentation when creating statements
  (sp-local-pair '(c-mode java-mode) "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair '(c-mode java-mode) "{" nil :post-handlers '(("||\n[i]" "RET")))

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)
  (bind-key "s-<home>" 'sp-beginning-of-sexp smartparens-mode-map)
  (bind-key "s-<end>" 'sp-end-of-sexp smartparens-mode-map)
  (bind-key "s-<left>" 'sp-beginning-of-previous-sexp smartparens-mode-map)
  (bind-key "s-<right>" 'sp-next-sexp smartparens-mode-map)
  (bind-key "s-<up>" 'sp-backward-up-sexp smartparens-mode-map)
  (bind-key "s-<down>" 'sp-down-sexp smartparens-mode-map))

(use-package hydra
  :commands defhydra
  :bind ("C-M-s" . hydra-splitter/body))

(defun hydra-splitter/body ()
  "Defines a Hydra to resize the windows."
  ;; overwrites the original function and calls it
  ;; https://github.com/abo-abo/hydra/issues/149
  (interactive)
  (require 'hydra-examples)
  (funcall
   (defhydra hydra-splitter nil "splitter"
     ("<left>" hydra-move-splitter-left)
     ("<down>" hydra-move-splitter-down)
     ("<up>" hydra-move-splitter-up)
     ("<right>" hydra-move-splitter-right))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for overriding common emacs keybindings with tweaks.
(global-unset-key (kbd "C-z")) ;; I hate you so much C-z
(global-set-key (kbd "C-x C-c") 'safe-kill-emacs)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-.") 'projectile-find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for defining commonly invoked commands that deserve
;; a short binding instead of their packager's preferred binding.
(global-set-key (kbd "C-<tab>") 'company-or-dabbrav-complete)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;;..............................................................................
;; elisp
(use-package lisp-mode
  :ensure nil
  :commands emacs-lisp-mode
  :config
  (bind-key "RET" 'comment-indent-new-line emacs-lisp-mode-map)
  (bind-key "C-c c" 'compile emacs-lisp-mode-map))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :commands eldoc-mode)

(use-package focus
  :commands focus-mode)

(use-package pcre2el
  :commands rxt-toggle-elisp-rx
  :init (bind-key "C-c / t" 'rxt-toggle-elisp-rx emacs-lisp-mode-map))

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (setq show-trailing-whitespace t)

            ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/481
            (add-hook 'post-self-insert-hook 'sp--post-self-insert-hook-handler)

            (whitespace-mode-with-local-variables)
            (focus-mode)
            ;; WORKAROUND https://github.com/larstvei/Focus/issues/4
            (add-hook 'after-revert-hook (lambda ()
                                           (focus-mode 0)
                                           (focus-mode 1)))
            (rainbow-mode)
            (prettify-symbols-mode)
            (eldoc-mode)
            (flycheck-mode)
            (yas-minor-mode)
            (company-mode)
            (smartparens-strict-mode)
            (rainbow-delimiters-mode)))


;;..............................................................................
;; Scala
(setq scala-indent:use-javadoc-style t
      scala-indent:align-parameters t
      ensime-default-buffer-prefix "ENSIME-"
      ensime-prefer-noninteractive t
      ensime-refactor-enable-beta t
      ensime-refactor-preview t
      ensime-refactor-auto-apply-file-limit 1
      ensime-refactor-auto-apply-hunk-limit 1)

;; prefer local ensime-emacs to MELPA install (for dev)
(add-to-load-path (concat user-emacs-directory "ensime-emacs"))

;; Java / Scala support for templates
(defun mvn-package-for-buffer ()
  "Calculate the expected package name for the buffer;
assuming it is in a maven-style project."
  (let* ((kind (file-name-extension buffer-file-name))
         (root (locate-dominating-file default-directory kind)))
    (when root
      (require 'subr-x) ;; maybe we should just use 's
      (replace-regexp-in-string
       (regexp-quote "/") "."
       (string-remove-suffix "/"
                             (string-remove-prefix
                              (expand-file-name (concat root "/" kind "/"))
                              default-directory))
       nil 'literal))))

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(use-package scala-mode2
  :interpreter
  ("scala" . scala-mode)
  :config
  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)
  (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
  (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
  (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
  (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)
  ;; BUG https://github.com/Fuco1/smartparens/issues/468
  ;; backwards/next not working particularly well

  ;; i.e. bypass company-mode
  (bind-key "C-<tab>" 'dabbrev-expand scala-mode-map)

  (bind-key "C-c c" 'sbt-command scala-mode-map)
  (bind-key "C-c e" 'next-error scala-mode-map))

(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (if (ensime-connection-or-nil)
      (ensime-edit-definition)
    (projectile-find-tag)))

(use-package ensime
  :commands ensime ensime-mode
  :config
  (add-hook 'git-timemachine-mode-hook (lambda() (ensime-mode 0)))

  (bind-key "s-n" 'ensime-search ensime-mode-map)
  (bind-key "s-i" 'ensime-print-type-at-point ensime-mode-map)
  (bind-key "M-." 'ensime-edit-definition-with-fallback ensime-mode-map)

  (setq ensime-goto-test-config-defaults
        (plist-merge ensime-goto-test-config-defaults
                     '(:test-class-suffixes ("Spec" "Test" "Check"))
                     '(:test-template-fn ensime-goto-test--test-template-scalatest-flatspec))))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/hvesalai/sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)

  (bind-key "C-c c" 'sbt-command sbt:mode-map)
  (bind-key "C-c e" 'next-error sbt:mode-map))

(add-hook 'scala-mode-hook
          (lambda()
            ;; WORKAROUND https://github.com/hvesalai/scala-mode2/issues/99
            (remove-hook 'post-self-insert-hook 'scala-indent:indent-on-parentheses)
            ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/481
            (add-hook 'post-self-insert-hook 'sp--post-self-insert-hook-handler)

            (whitespace-mode-with-local-variables)
            (smartparens-mode)
            (yas-minor-mode)
            (git-gutter-mode)
            (company-mode)
            (ensime-mode)

            (set (make-local-variable 'company-backends)
                 ;; https://github.com/company-mode/company-mode/issues/390
                 ;; (ensime-company :with company-yasnippet)
                 '(ensime-company
                   (company-keywords company-dabbrev-code company-etags company-yasnippet)))

            (scala-mode:goto-start-of-code)))

;;..............................................................................
;; Java: watch out for https://github.com/ensime/ensime-server/issues/345
(add-hook 'java-mode-hook
          (lambda()
            (yas-minor-mode)
            (company-mode)
            (smartparens-mode)
            (local-set-key (kbd "C-c e") 'next-error)))


;;..............................................................................
;; C
(add-hook 'c-mode-hook (lambda()
                         (yas-minor-mode)
                         (company-mode)
                         (smartparens-mode)))

;;..............................................................................
;; org-mode
(add-hook 'writeroom-mode-hook
          (lambda ()
            ;; NOTE weird sizing bug in writeroom
            (delete-other-windows)))

(add-hook 'org-mode-hook
          (lambda ()
            (yas-minor-mode)
            (company-mode)
            (visual-line-mode)
            (local-set-key (kbd "C-c c") 'pandoc)
            (local-set-key (kbd "s-c") 'picture-mode)
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((ditaa . t)))))

(use-package markdown-mode
  :commands markdown-mode)
(add-hook 'markdown-mode-hook
          (lambda ()
            (yas-minor-mode)
            (company-mode)
            (visual-line-mode)))

;;..............................................................................
;; R
(use-package ess-site
  :ensure ess
  :mode ("\\.R\\'" . R-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS specific
(pcase system-type
  (`gnu/linux
   (load (concat user-emacs-directory "init-linux.el"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Site Local
(load (concat user-emacs-directory "local.el") 'no-error)

;;; init.el ends here
