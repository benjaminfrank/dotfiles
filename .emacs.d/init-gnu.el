;;; init-linux.el --- Appended to init for GNU boxen -*- lexical-binding: t -*-

;; Copyright (C) 2015 Sam Halliday
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Some packages only make sense on GNU/Linux, typically because of
;;  external applications.  This is where they go.
;;
;;; Code:

;; keeps flycheck happy
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.

(setq
 ;; allows projectile to see .log files, even though git ignores them
 projectile-git-command "cat <(git ls-files -zco --exclude-standard) <(find . -name '*.log' -maxdepth 4 -print0)"
 browse-url-generic-program "sensible-browser"
 x-select-enable-clipboard t
 interprogram-paste-function 'x-cut-buffer-or-selection-value)


(use-package ess-site
  :ensure ess
  :mode ("\\.R\\'" . R-mode))

(use-package package-utils
  :commands package-utils-upgrade-all)

(use-package flycheck-cask
  :commands flycheck-cask-setup
  :config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup)))

(use-package elnode
  :commands elnode-make-webserver)

(use-package erc
  :commands erc erc-tls
  :init
  (setq
   erc-prompt-for-password nil ;; prefer ~/.authinfo for passwords
   erc-hide-list '("JOIN" "PART" "QUIT")
   erc-autojoin-channels-alist
   '(("irc.freenode.net" "#emacs")
     ("irc.gitter.im" "#ensime/ensime-server" "#ensime/ensime-emacs"))))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and configuring more involved
;; task/function-specific modes, organised by task or function.
;;..............................................................................
;; Email
(setq smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      mail-user-agent 'message-user-agent
      user-mail-address "Sam.Halliday@gmail.com"
      send-mail-function 'smtpmail-send-it
      message-auto-save-directory (concat user-emacs-directory "drafts")
      message-kill-buffer-on-exit t
      message-signature "Best regards,\nSam\n"
      notmuch-search-line-faces '(("unread" :weight bold)
                                  ("flagged" :inherit 'font-lock-string-face))
      notmuch-fcc-dirs nil
      notmuch-search-oldest-first nil
      notmuch-address-command "notmuch-addrlookup"
      notmuch-saved-searches '(("inbox" . "tag:inbox")
                               ("unread" . "tag:unread")
                               ("flagged" . "tag:flagged")
                               ("all" . "*")))
(use-package notmuch
  :commands notmuch
  :config
  (add-hook 'message-setup-hook #'company-mode)
  (add-hook 'message-setup-hook #'mml-secure-sign-pgpmime))

;;..............................................................................
;; Clojure

(use-package flycheck-clojure)
(use-package flycheck-pos-tip)
(use-package cider
  :commands cider-jack-in
  :config
  (bind-key "C-c c" 'compile clojure-mode-map))
(defalias 'cider 'cider-jack-in)
(add-hook 'clojure-mode-hook
          (lambda ()
            (show-paren-mode)
            ;;(focus-mode)
            (rainbow-mode)
            (eldoc-mode)

            ;; BUG https://github.com/clojure-emacs/squiggly-clojure/issues/39
            (flycheck-clojure-setup)
            (flycheck-mode)
            (flycheck-pos-tip-mode)

            (yas-minor-mode)
            (company-mode)
            (smartparens-strict-mode)
            (rainbow-delimiters-mode)))

(add-hook 'cider-repl-mode-hook #'eldoc-mode)


;;..............................................................................
;; shell scripts
(add-hook 'sh-mode-hook #'electric-indent-local-mode)

;;..............................................................................
;; org-mode
(use-package org
  :ensure org-plus-contrib
  :defer t)

(use-package synosaurus
  :commands synosaurus-choose-and-replace
  :init (setq synosaurus-choose-method 'popup)
  :config
  (bind-key "C-c s r" 'synosaurus-choose-and-replace text-mode-map))

;; seems to be a performance problem in emacs 24.5 in some text modes (e.g. email)
;;(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'markdown-mode-hook #'flyspell-mode)

(defun pandoc ()
  "If a hidden .pandoc file exists for the file, run it."
  ;; this effectively replaces pandoc-mode for me
  (interactive)
  (let ((command-file (concat (file-name-directory buffer-file-name)
                              "." (file-name-nondirectory buffer-file-name)
                              ".pandoc")))
    (when (file-exists-p command-file)
      (shell-command command-file))))

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

;;; init-linux.el ends here
