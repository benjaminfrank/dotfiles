;;; init.el --- Emacs configuration
;; Copyright (C) 2014 - 2015 Sam Halliday (fommil)

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
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; TODO change the parameter order to be (defn hook force) where defn is either:
;; function; (function package); or (function package filename)
(defun required (function &optional force hook package filename)
  "`autoload' an interactive FUNCTION :symbol, installing if not present.

FORCE :boolean will use `require' instead of `autoload'.

Runs a HOOK :lambda when the file is loaded.

PACKAGE :symbol is used to workaround packages that have been
distributed under a different name than their function.

FILENAME :string is only needed if the filename on disk is not
the package or function name."
  (interactive)
  (let* ((sym (or package function))
         (name (or filename (symbol-name sym))))
    (unless (locate-library name)
      (package-install sym))
    (when hook
      (eval-after-load name hook))
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
(required 'highlight-symbol t
          (lambda() (add-hook 'find-file-hook (lambda() (highlight-symbol-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.
(required 'hungry-delete)
(required 'misc-cmds)
(required 'multiple-cursors)
(required 'git-gutter)

(setq magit-revert-buffers t)
(required 'magit)

(setq git-timemachine-abbreviation-length 4)
(required 'git-timemachine)

(required 'ctags-create-tags-table nil nil 'ctags)
(required 'ctags-auto-update-mode nil nil 'ctags-update)

;; TODO: create minimal company-backends list.
;;
;; For any given word completion, company-mode queries each backend
;; and the first one that returns non-nil is then asked for a
;; completion --- then completion terminates (even if no suggestions)
(setq company-dabbrev-ignore-case nil
      company-dabbrev-code-ignore-case nil
      company-dabbrev-downcase nil)
(required 'company-mode nil nil 'company)
(required 'rainbow-mode)
(required 'flycheck)
(required 'yas-minor-mode nil (lambda() (yas-reload-all)) 'yasnippet)
(required 'elnode)
(required 'tidy-buffer nil nil 'tidy)

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
(required 'whitespace-mode nil nil 'whitespace)
;; local whitespace-line-column are ignored unless loaded by
;; hack-local-variables-hook
;; https://emacs.stackexchange.com/questions/7743
;; so make sure to load whitespace-mode in a buffer local hook


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
(required 'projectile nil (lambda()
                            ;; https://github.com/bbatsov/projectile/issues/755
                            (require 'vc-git)
                            ;; projectile-find-tag can be slow/broken for big TAGS
                            ;; https://github.com/bbatsov/projectile/issues/668
                            ;; https://github.com/bbatsov/projectile/issues/683
                            (define-key projectile-mode-map (kbd "C-c p j") 'find-tag)))
(required 'idomenu)

(required 'smartparens-mode nil
          (lambda()
            (require 'smartparens-config)
            (sp-use-smartparens-bindings)
            ;; nice whitespace / indentation when creating statements
            (sp-local-pair 'c-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'c-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'java-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'java-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
            (define-key smartparens-mode-map (kbd "C-<left>") 'subword-left)
            (define-key smartparens-mode-map (kbd "C-<right>") 'subword-right))
          'smartparens)

(required 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(required 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; guide-key is a really great way to learn keybindings, but I've
;; outgrown it and now fall back to `C-h m' when in doubt.
;;
(setq guide-key/guide-key-sequence t)
(required 'guide-key-mode nil nil 'guide-key)

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
                               ("flagged" . "tag:flagged")))
(required 'notmuch nil (lambda()
                         (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
                         (add-hook 'mml-mode (lambda()
                                               (visual-line-mode)
                                               (writeroom-mode)))))
(required 'notmuch-address nil (lambda() (notmuch-address-message-insinuate)))


;;..............................................................................
;; shell scripts
(add-hook 'sh-mode-hook (lambda()
                          (electric-indent-local-mode)))

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
            ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/481
            (add-hook 'post-self-insert-hook 'sp--post-self-insert-hook-handler)

            (add-hook 'hack-local-variables-hook 'whitespace-mode nil t)
            (local-set-key (kbd "M-.") 'elisp-find-tag-or-find-func)
            (local-set-key (kbd "s-q") 'describe-foo-at-point)
            ;; indent-tabs-mode needs to be reset
            (setq indent-tabs-mode nil
                  tab-width 4
                  c-basic-offset 4)
            (rainbow-mode)
            (when (fboundp 'prettify-symbols-mode) ;; added in 24.4
              (prettify-symbols-mode))
            (flyspell-prog-mode)
            (eldoc-mode)
            (flycheck-mode)
            (yas-minor-mode)
            (company-mode)
            (smartparens-strict-mode)
            (ctags-auto-update-mode)))


;;..............................................................................
;; Scala
(setq scala-indent:use-javadoc-style t
      scala-indent:align-parameters t
      ensime-default-buffer-prefix "ENSIME-"
      scala-outline-popup-select 'closest)
(let* ((local-ensime (concat user-emacs-directory "ensime-emacs")))
  (when (file-exists-p local-ensime)
    (add-to-list 'load-path local-ensime)))
(required 'ensime nil
          (lambda()
            (add-hook 'git-timemachine-mode-hook (lambda() (ensime-mode 0)))
            (setq ensime-goto-test-config-defaults
                  (plist-put (plist-put ;; TODO: clean up double plist-put
                              ensime-goto-test-config-defaults
                              :test-class-suffixes '("Spec" "Test" "Check"))
                             :test-template-fn 'ensime-goto-test--test-template-scalatest-flatspec))))
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
            ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/481
            (add-hook 'post-self-insert-hook 'sp--post-self-insert-hook-handler)

            (projectile-mode)

            (add-hook 'hack-local-variables-hook 'whitespace-mode nil t)

            ;; disable this post-self-insert-hook
            (defun scala-indent:indent-on-parentheses ())

            (flyspell-prog-mode)
            (highlight-symbol-mode)
            (smartparens-mode)
            (local-set-key (kbd "s-n") 'ensime-search)
            (local-set-key (kbd "s-i") 'ensime-print-type-at-point)
            (local-set-key (kbd "s-o") 'scala-outline-popup)
            (local-set-key (kbd "RET")
                           (lambda()
                             (interactive)
                             (newline-and-indent)
                             (scala-indent:insert-asterisk-on-multiline-comment)))

            ;; TODO: extend scala-mode-map to all C-derived languages
            (defun sp-restrict-c (sym)
              (sp-restrict-to-pairs-interactive "{([" sym))
            (local-set-key (kbd "s-<delete>") (sp-restrict-c 'sp-kill-sexp))
            (local-set-key (kbd "s-<backspace>") (sp-restrict-c 'sp-backward-kill-sexp))
            (local-set-key (kbd "s-<home>") (sp-restrict-c 'sp-beginning-of-sexp))
            (local-set-key (kbd "s-<end>") (sp-restrict-c 'sp-end-of-sexp))
            (local-set-key (kbd "s-<left>") (sp-restrict-c 'sp-beginning-of-previous-sexp))
            ;; would prefer sp-next-sexp but restriction is broken
            ;; https://github.com/Fuco1/smartparens/issues/468
            (local-set-key (kbd "s-<right>") (sp-restrict-c 'sp-beginning-of-next-sexp))
            (local-set-key (kbd "s-<up>") (sp-restrict-c 'sp-backward-up-sexp))
            (local-set-key (kbd "s-<down>") (sp-restrict-c 'sp-down-sexp))

            ;;(local-set-key (kbd "C-<right>") 'scala-syntax:forward-token)
            ;;(local-set-key (kbd "C-<left>") 'scala-syntax:backward-token)
            ;;(local-set-key (kbd "C-c c") 'sbt-or-maker-command)
            (local-set-key (kbd "C-c c") 'sbt-command)
            (local-set-key (kbd "C-c e") 'next-error)

            (required 'scala-outline-popup t)
            (git-gutter-mode)
            (define-key popup-isearch-keymap (kbd "s-o") 'popup-isearch-cancel)

            (required 'ensime t)
            (ensime-mode 1)

            (set (make-local-variable 'company-backends)
                 '(ensime-company (company-keywords company-etags)))

            (scala-mode:goto-start-of-code)))

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
(required 'org-taskjuggler-export nil nil 'org-plus-contrib)

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
(required 'ess-site nil nil 'ess)
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
