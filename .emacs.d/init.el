;;; init.el --- Emacs configuration

;; Copyright (C) 2014 - 2015 Sam Halliday (fommil)
;; License: http://www.gnu.org/licenses/gpl.html

;; Author: Sam Halliday <sam.halliday@gmail.com>
;; URL: https://github.com/fommil/unix/blob/master/.emacs.d/init.el

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
      enable-local-variables :safe
      ;;debug-on-error t
      ;;debug-on-quit t
      enable-recursive-minibuffers t
      create-lockfiles nil
      make-backup-files nil
      load-prefer-newer t
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
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
      dabbrev-case-fold-search nil
      ;;dabbrev-case-replace nil
      tags-case-fold-search nil
      compilation-skip-threshold 2
      c-basic-offset 4
      source-directory (getenv "EMACS_SOURCE")
      org-ditaa-jar-path "~/.ditaa.jar"
      org-confirm-babel-evaluate nil
      nxml-slash-auto-complete-flag t
      sentence-end-double-space nil
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "sensible-browser"
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setup functions that are built-in to emacs
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1) ;; show-smartparens is too slow
(global-subword-mode 1)
(setenv "SBT_OPTS" (concat "-no-colors " (getenv "SBT_OPTS")))
(global-auto-revert-mode 1)
(substitute-key-definition
 ;; allows using SPACE when in the minibuffer
 'minibuffer-complete-word
 'self-insert-command
 minibuffer-local-completion-map)

;; Disabling a few things that Emacs turns on by default (and I don't like)
(electric-indent-mode 0)
(global-auto-composition-mode 0)
;;(auto-compression-mode 0) ;; breaks help / elisp navigation
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
(require 'cl-lib)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun required (feature &optional hook force)
  "`autoload' an interactive FEATURE, which is either:

- function :symbol
  a symbol with consistent package and filename.
- (function:symbol package:symbol)
  a function with a different package (filename is package).
- (function:symbol package:symbol name:string)
  a function with inconsistent package name and filename.

The package will be downloaded using function `package-install'
if not found locally.

Runs a HOOK :lambda when the file is loaded.

FORCE :boolean will use `require' instead of `autoload'."
  (interactive)
  (cl-multiple-value-bind (function package filename)
      (pcase feature
        ((pred symbolp)   (list feature feature (symbol-name feature)))
        (`(,fn ,pkg)      (list fn pkg (symbol-name pkg)))
        (`(,fn ,pkg ,nm)  (list fn pkg nm)))
    (unless (locate-library filename)
      (package-install package))
    (when hook
      (eval-after-load filename hook))
    (if force
        (require function)
      (autoload function filename nil 'interactive))))

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
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

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

(defun minor-modes-active ()
  "The minor modes that are enabled in the current buffer."
  (interactive)
  (let (active-modes)
    (dolist (mode minor-mode-list active-modes)
      (if (and (boundp mode) (symbol-value mode))
          (add-to-list 'active-modes mode)))))

(defvar copyright-owner user-full-name
  "The copyright owner for the buffer.
e.g. when `dir-locals.el' provides a `copyright-owner' variable.
Particularly useful in yasnippet templates.")

(defun newfile-template ()
  "Populate with a yasnippet template called `newfile' for the `major-mode'."
  (when (eq 0 (buffer-size))
    (when (member 'yas-minor-mode (minor-modes-active))
      (let ((snippet (yas-lookup-snippet "newfile" major-mode 'noerror)))
        (when snippet
          (yas-expand-snippet snippet)
          (hack-local-variables))))))

(defun mvn-package-for-buffer ()
  "Calculate the expected package name for the buffer;
assuming it is in a maven-style project."
  ;; TODO: not Windows friendly
  (interactive)
  (let ((kind (file-name-extension buffer-file-name)))
    (require 'subr-x) ;; maybe we should just use 's
    (replace-regexp-in-string
     (regexp-quote "/") "."
     (string-remove-suffix "/"
                           (string-remove-prefix
                            (expand-file-name (concat (locate-dominating-file default-directory kind) "/" kind "/"))
                            default-directory))
     nil 'literal)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global modes that should be loaded in order to
;; make them immediately available.
(required 'midnight (lambda()
                      (add-to-list 'clean-buffer-list-kill-regexps
                                   "\\`\\*magit.*\\*\\'")
                      (add-to-list 'clean-buffer-list-kill-never-regexps
                                   ".*\\*sbt.*")
                      (add-to-list 'clean-buffer-list-kill-never-regexps
                                   ".*\\*ENSIME-server.*")) t)
(required 'persistent-scratch (lambda() (persistent-scratch-setup-default)) t)
(required 'highlight-symbol
          (lambda() (add-hook 'find-file-hook (lambda() (highlight-symbol-mode)))) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.
(required '(package-utils-upgrade-all package-utils))
(required 'hungry-delete)
(required 'misc-cmds)
(required 'multiple-cursors)
(required 'git-gutter)

(setq magit-revert-buffers t
      magit-push-always-verify nil)
(required 'magit)

(setq git-timemachine-abbreviation-length 4)
(required 'git-timemachine)

(required '(ctags-create-tags-table ctags))
(required '(ctags-auto-update-mode ctags-update))

;; TODO: create minimal company-backends list.
;;
;; For any given word completion, company-mode queries each backend
;; and the first one that returns non-nil is then asked for a
;; completion --- then completion terminates (even if no suggestions)
(setq company-dabbrev-ignore-case nil
      company-dabbrev-code-ignore-case nil
      company-dabbrev-downcase nil)
(required '(company-mode company)
          (lambda ()
            (require 'company-yasnippet)
            ;; disables TAB in company-mode, freeing it for yasnippet
            (define-key company-active-map [tab] nil)))

(required 'rainbow-mode)
(required '(flycheck-cask-setup flycheck-cask))
(required 'flycheck)

(required '(yas-minor-mode yasnippet) (lambda() (yas-reload-all)))
(add-hook 'find-file-hook 'newfile-template)

(required 'elnode)
(required '(tidy-buffer tidy))

;; no writeroom-mode-hook to attach to in start or close
;; https://github.com/joostkremers/writeroom-mode/issues/18
;; also, incompatible with company-mode
;; https://github.com/company-mode/company-mode/issues/376
;; (set (make-local-variable 'buffer-face-mode-face) '(:height 175))
;; (buffer-face-mode 1)
(required 'writeroom-mode)
(required 'scratch)

(setq erc-prompt-for-password nil ;; prefer ~/.authinfo for passwords
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-autojoin-channels-alist
      '(("irc.freenode.net" "#emacs")
        ("irc.gitter.im" "#ensime/ensime-server" "#ensime/ensime-emacs")))
(required 'erc)

(setq whitespace-style '(face trailing tabs lines-tail)
      ;;whitespace-style '(face trailing tab-mark lines-tail)
      whitespace-line-column 80)
(put 'whitespace-line-column 'safe-local-variable #'integerp)
(put 'copyright-owner 'safe-local-variable #'stringp)

(required '(whitespace-mode whitespace))
;; local whitespace-line-column are ignored unless loaded by
;; hack-local-variables-hook
;; https://emacs.stackexchange.com/questions/7743
;; so make sure to load whitespace-mode in a buffer local hook


(setq ispell-dictionary "british"
      flyspell-prog-text-faces '(font-lock-doc-face))
(required 'flyspell (lambda()
                      (put 'text-mode
                           'flyspell-mode-predicate
                           'flyspell-ignore-http-and-https)))
(add-hook 'text-mode-hook (lambda() (flyspell-mode 1)))

(setq ag-reuse-window 't)
(required 'ag)
(setq projectile-use-native-indexing t
      projectile-use-git-grep t)
(required 'projectile (lambda()
                        ;; https://github.com/bbatsov/projectile/issues/755
                        (require 'vc-git)
                        ;; projectile-find-tag can be slow/broken for big TAGS
                        ;; https://github.com/bbatsov/projectile/issues/668
                        ;; https://github.com/bbatsov/projectile/issues/683
                        (define-key projectile-mode-map (kbd "C-c p j") 'find-tag)))
(required 'idomenu)

(required '(smartparens-mode smartparens)
          (lambda()
            (require 'smartparens-config)
            (sp-use-smartparens-bindings)
            (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
            (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
            (sp-pair "{" "}" :wrap "C-{")

            ;; nice whitespace / indentation when creating statements
            (sp-local-pair 'c-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'c-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'java-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'java-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
            (define-key smartparens-mode-map (kbd "C-<left>") 'subword-left)
            (define-key smartparens-mode-map (kbd "C-<right>") 'subword-right)))

(required 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(required 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(required 'hydra)
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

;; guide-key is a really great way to learn keybindings, but I've
;; outgrown it and now fall back to `C-h m' when in doubt.
;;
;;(setq guide-key/guide-key-sequence t)
;;(required '(guide-key-mode guide-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for overriding common emacs keybindings with tweaks.
(global-unset-key (kbd "C-z")) ;; I hate you so much C-z
(global-set-key (kbd "C-x C-c") 'safe-kill-emacs)
;;(global-set-key (kbd "C-x k") 'kill-buffer-and-its-windows)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "M-i") 'idomenu)
(global-set-key (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for defining commonly invoked commands that deserve
;; a short binding instead of their packager's preferred binding.
(global-set-key (kbd "C-<tab>") 'dabbrev-expand) ;; fallback incase company-mode isn't available
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-F") 'projectile-ag)
(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "s-b") 'magit-blame)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-h") 'highlight-symbol-at-point)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
;; https://github.com/Fuco1/smartparens/wiki/Working-with-expressions#navigation-functions
;; TODO: restrict sexp navigation in C derived languages to just the parenthesis
(global-set-key (kbd "s-<delete>") 'sp-kill-sexp)
(global-set-key (kbd "s-<backspace>") 'sp-backward-kill-sexp)
(global-set-key (kbd "s-<home>") 'sp-beginning-of-sexp)
(global-set-key (kbd "s-<end>") 'sp-end-of-sexp)
(global-set-key (kbd "s-<left>") 'sp-beginning-of-previous-sexp)
(global-set-key (kbd "s-<right>") 'sp-next-sexp)
(global-set-key (kbd "s-<up>") 'sp-backward-up-sexp)
(global-set-key (kbd "s-<down>") 'sp-down-sexp)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "C-M-s") 'hydra-splitter/body)

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
      notmuch-address-command "notmuch-addrlookup"
      notmuch-saved-searches '(("inbox" . "tag:inbox")
                               ("unread" . "tag:unread")
                               ("flagged" . "tag:flagged")
                               ("all" . "*")))
(required 'notmuch (lambda() (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)))
(required 'notmuch-address (lambda() (notmuch-address-message-insinuate)))
(add-hook 'message-mode-hook (lambda()
                               ;; hmm, undecided about filling emails...
                               ;;(auto-fill-mode -1)
                               ;;(visual-line-mode)
                               (writeroom-mode)))

;;..............................................................................
;; shell scripts
(add-hook 'sh-mode-hook (lambda()
                          (electric-indent-local-mode)))

;;..............................................................................
;; elisp
(defun elisp-find-tag-or-find-func ()
  ;;; Could be a lot smarter for non-function symbols (variables, faces, packages, etc)
  "Use `find-func' to find symbol at point, falling back to `find-tag' features."
  (interactive)
  (let ((fun (function-called-at-point)))
    (if (and fun (find-function-noselect fun (not source-directory)))
        (find-function-do-it fun nil 'switch-to-buffer)
      (find-tag (symbol-name (symbol-at-point))))))

(defun add-default-directory-to-load-path ()
  "Add the current working directory to the `load-path'.
Useful for interactive elisp projects."
  (interactive)
  (add-to-list 'load-path default-directory))

(eval-after-load "lisp-mode"
  (lambda ()
    (define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-find-tag-or-find-func)
    (define-key emacs-lisp-mode-map (kbd "RET") 'comment-indent-new-line)
    (define-key emacs-lisp-mode-map (kbd "C-c c") 'compile)))

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/481
            (add-hook 'post-self-insert-hook 'sp--post-self-insert-hook-handler)

            ;; allows dir-locals for whitespace settings
            (add-hook 'hack-local-variables-hook 'whitespace-mode nil t)

            ;; indent-tabs-mode needs to be reset
            (setq indent-tabs-mode nil)
            (rainbow-mode)
            (when (fboundp 'prettify-symbols-mode) ;; added in 24.4
              (prettify-symbols-mode))
            (flyspell-prog-mode)
            (eldoc-mode)
            (flycheck-mode)
            (flycheck-cask-setup)
            (yas-minor-mode)
            (company-mode)
            (smartparens-strict-mode)
            (ctags-auto-update-mode)
            ))


;;..............................................................................
;; Scala
(setq scala-indent:use-javadoc-style t
      scala-indent:align-parameters t
      ensime-default-buffer-prefix "ENSIME-"
      scala-outline-popup-select 'closest)

(defun sp-restrict-c (sym)
  ;; TODO: extend mode-map to all C-derived languages
  (sp-restrict-to-pairs-interactive "{([" sym))

(let* ((local-ensime (concat user-emacs-directory "ensime-emacs")))
  (when (file-exists-p local-ensime)
    (add-to-list 'load-path local-ensime)))

(required 'scala-mode2
          (lambda ()
            (require 'smartparens)
            (define-key scala-mode-map (kbd "s-o") 'scala-outline-popup)
            ;; (define-key scala-mode-map (kbd "RET")
            ;;                (lambda()
            ;;                  (interactive)
            ;;                  (newline-and-indent)
            ;;                  (scala-indent:insert-asterisk-on-multiline-comment)))

            (define-key scala-mode-map (kbd "s-<delete>") (sp-restrict-c 'sp-kill-sexp))
            (define-key scala-mode-map (kbd "s-<backspace>") (sp-restrict-c 'sp-backward-kill-sexp))
            (define-key scala-mode-map (kbd "s-<home>") (sp-restrict-c 'sp-beginning-of-sexp))
            (define-key scala-mode-map (kbd "s-<end>") (sp-restrict-c 'sp-end-of-sexp))
            (define-key scala-mode-map (kbd "s-<left>") (sp-restrict-c 'sp-beginning-of-previous-sexp))
            ;; would prefer sp-next-sexp but restriction is broken
            ;; https://github.com/Fuco1/smartparens/issues/468
            (define-key scala-mode-map (kbd "s-<right>") (sp-restrict-c 'sp-beginning-of-next-sexp))
            (define-key scala-mode-map (kbd "s-<up>") (sp-restrict-c 'sp-backward-up-sexp))
            (define-key scala-mode-map (kbd "s-<down>") (sp-restrict-c 'sp-down-sexp))

            (define-key scala-mode-map (kbd "C-c c") 'sbt-command)
            (define-key scala-mode-map (kbd "C-c e") 'next-error)))

(required 'ensime
          (lambda()
            (add-hook 'git-timemachine-mode-hook (lambda() (ensime-mode 0)))

            (define-key ensime-mode-map (kbd "s-n") 'ensime-search)
            (define-key ensime-mode-map (kbd "s-i") 'ensime-print-type-at-point)
            (define-key ensime-mode-map (kbd "M-.") 'ensime-edit-definition-with-fallback)

            (setq ensime-goto-test-config-defaults
                  (plist-put (plist-put ;; TODO: clean up double plist-put
                              ensime-goto-test-config-defaults
                              :test-class-suffixes '("Spec" "Test" "Check"))
                             :test-template-fn 'ensime-goto-test--test-template-scalatest-flatspec))))

(required 'scala-outline-popup)
(required 'sbt-mode)

(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (if (ensime-connection-or-nil)
      (ensime-edit-definition)
    (call-interactively 'find-tag)))

(add-hook 'scala-mode-hook
          (lambda()
            ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/481
            (add-hook 'post-self-insert-hook 'sp--post-self-insert-hook-handler)

            (projectile-mode)

            (add-hook 'hack-local-variables-hook 'whitespace-mode nil t)

            ;; disable this post-self-insert-hook
            (defun scala-indent:indent-on-parentheses ())

            (flyspell-prog-mode)
            (highlight-symbol-mode)
            (smartparens-mode)
            (yas-minor-mode)
            (git-gutter-mode)
            (ctags-auto-update-mode)

            ;; forces load of ensime
            (required 'ensime nil t)
            (ensime-mode 1)

            (scala-mode:goto-start-of-code)))

(add-hook 'ensime-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 ;; https://github.com/company-mode/company-mode/issues/390
                 ;; (ensime-company :with company-yasnippet)
                 '(ensime-company
                   (company-keywords company-etags company-yasnippet)))))

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
            (smartparens-mode)
            (local-set-key (kbd "C-c e") 'next-error)
            (ctags-auto-update-mode)))


;;..............................................................................
;; C
(add-hook 'c-mode-hook (lambda()
                         (yas-minor-mode)
                         (projectile-mode)
                         (company-mode)
                         (smartparens-mode)
                         (ctags-auto-update-mode)))

;;..............................................................................
;; org-mode
;; 'org is a system install but doing a 'required on taskjuggler forces
;; an install of org-plus-contrib from ELPA
(required '(org-taskjuggler-export org-plus-contrib))

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

(defun unwrap-buffer ()
  "Unwrap the buffer for function `visual-line-mode'."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region 0 (point-max))))

(defun writeroom-ask ()
  "Interactively ask if the user wants to go into writeroom-mode."
  (interactive)
  (when (y-or-n-p "Go into writeroom-mode? ")
    (delete-other-windows)
    (visual-line-mode)
    (writeroom-mode)))

(defun markup-common-hooks()
  (writeroom-ask)
  (yas-minor-mode)
  (company-mode)
  ;;(auto-fill-mode)

  (set (make-local-variable 'company-backends)
       '(company-ispell)) ;; only dictionary completions
  (local-set-key (kbd "C-c c") 'pandoc))
(add-hook 'org-mode-hook (lambda()
                           (markup-common-hooks)
                           (local-set-key (kbd "s-c") 'picture-mode)
                           (org-babel-do-load-languages
                            'org-babel-load-languages
                            '((ditaa . t)))))
(add-hook 'markdown-mode-hook (lambda()
                                (markup-common-hooks)
                                ;; github interprets newlines
                                (visual-line-mode)))

;;..............................................................................
;; R
(required '(ess-site ess))
;; bad packaging means we have to manually setup R-mode
(autoload 'R-mode "ess-site" nil t)
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))

;;..............................................................................
;; Chat rooms
(defun gitter()
  "Connect to Gitter."
  (interactive)
  (erc-tls :server "irc.gitter.im" :port 6697))
(defun freenode()
  "Connect to Freenode."
  (interactive)
  (erc :server "irc.freenode.net" :port 6667))


;;..............................................................................
;; User Site Local
(let ((user-local (concat user-emacs-directory "local.el")))
  (when (file-exists-p user-local)
    (load user-local)))

;;; init.el ends here
