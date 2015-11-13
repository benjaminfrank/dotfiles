;;; init-linux.el --- Appended to init for Linux boxen -*- lexical-binding: t -*-

;; Copyright (C) 2015 Sam Halliday
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Some packages only make sense on Linux, typically because of
;;  external applications. This is where they go.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.

(required '(package-utils-upgrade-all package-utils))
(required '(flycheck-cask-setup flycheck-cask))
(required 'elnode)
(required '(tidy-buffer tidy))

(setq erc-prompt-for-password nil ;; prefer ~/.authinfo for passwords
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-autojoin-channels-alist
      '(("irc.freenode.net" "#emacs")
        ("irc.gitter.im" "#ensime/ensime-server" "#ensime/ensime-emacs")))
(required 'erc)

(setq ag-reuse-window 't)
(required 'ag)

(required 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(required 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

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
;; org-mode
;; 'org is a system install but doing a 'required on taskjuggler forces
;; an install of org-plus-contrib from ELPA
;; doesn't work on Windows
;; Debugger entered--Lisp error: (overflow-error "100000000")
;; eval-buffer(#<buffer  *load*-834341> nil "d:/.emacs.d/elpa/org-plus-contrib-20150921/org-footnote.el" nil t)  ; Reading at buffer position 20900
(required '(org-taskjuggler-export org-plus-contrib))

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
