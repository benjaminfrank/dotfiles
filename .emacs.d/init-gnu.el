;;; init-linux.el --- Appended to init for Linux boxen -*- lexical-binding: t -*-

;; Copyright (C) 2015 Sam Halliday
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Some packages only make sense on Linux, typically because of
;;  external applications. This is where they go.
;;
;;; Code:

;; keeps flycheck happy
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.

(setq
 mail-user-agent 'message-user-agent
 user-mail-address "Sam.Halliday@gmail.com"
 send-mail-function 'smtpmail-send-it
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

(use-package tidy
  :commands tidy-buffer)

(use-package erc
  :commands erc erc-tls
  :init
  (setq
   erc-prompt-for-password nil ;; prefer ~/.authinfo for passwords
   erc-hide-list '("JOIN" "PART" "QUIT")
   erc-autojoin-channels-alist
   '(("irc.freenode.net" "#emacs")
     ("irc.gitter.im" "#ensime/ensime-server" "#ensime/ensime-emacs"))))

(use-package ag
  :commands ag
  :init
  (setq ag-reuse-window 't)
  :config
  (add-hook 'ag-search-finished-hook (lambda () (pop-to-buffer next-error-last-buffer))))

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
(use-package notmuch
  :commands notmuch
  :config (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime))
;;(use-package notmuch-address
;;  :commands notmuch-address)

;;..............................................................................
;; shell scripts
(add-hook 'sh-mode-hook (electric-indent-local-mode))

;;..............................................................................
;; org-mode
(use-package org
  :ensure org-plus-contrib
  :defer t)

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
