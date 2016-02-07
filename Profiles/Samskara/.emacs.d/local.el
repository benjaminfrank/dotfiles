;;; local.el --- Local config
;;; Commentary:
;;
;;  For desktop.
;;
;;; Code:

(use-package darcula-theme
  :config
  (set-frame-font "Inconsolata-16"))

;; I'll usually want access to these..
(find-file (expand-file-name "scratch.el" user-emacs-directory))
(find-file "~/Projects")

(eval-after-load 'sbt-mode
  (defun sbt:initialize-for-compilation-mode ()
    (setq-local
     compilation-directory-matcher
     '("--go-home-compile.el--you-are-drn^H^H^Hbugs--"))
    (setq-local
     compilation-error-regexp-alist
     ;; NOTE: must support C:\I\Like\To\Code\In\Windows style paths
     '(("^\\[error\\][[:space:]]\\([\\w/]:?[^[:space:]:]+\\):\\([[:digit:]]+\\):" 1 2 nil 2 1)
       ("^\\[warn\\][[:space:]]\\([\\w/]:?[^[:space:]:]+\\):\\([[:digit:]]+\\):" 1 2 nil 1 1)
       ("^\\[info\\][[:space:]]\\([\\w/]:?[^[:space:]:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0 1)
       ("^\\[info\\][[:space:]]-[[:space:]]\\(.*\\) \\*\\*\\* FAILED \\*\\*\\*" nil nil nil 2 1)))
    (setq-local
     compilation-mode-font-lock-keywords
     '(
       ("^\\[error\\] \\(Failed: Total .*\\)"
        (1 sbt:error-face))
       ("^\\[info\\] \\(Passed: Total [0-9]+, Failed 0, Errors 0, Passed [0-9]+\\)\\(\\(?:, Skipped [0-9]*\\)?\\)"
        (1 sbt:info-face)
        (2 sbt:warning-face))
       ("^\\[info\\] \\(Passed: Total [0-9]+, Failed [1-9][0-9]*.*\\)"
        (1 sbt:error-face))
       ("^\\[info\\] \\(Passed: Total [0-9]+, Failed [0-9]+, Errors [1-9][0-9]*.*\\)"
        (1 sbt:error-face))
       ("^\\[info\\] \\([0-9]+ examples?, 0 failure, 0 error\\)"
        (1 sbt:info-face))
       ("^\\[info\\] \\([0-9]+ examples?, [1-9][0-9]* failure, [0-9]+ error\\)"
        (1 sbt:error-face))
       ("^\\[info\\] \\([0-9]+ examples?, [0-9]* failure, [1-9][0-9]+ error\\)"
        (1 sbt:error-face))
       ("^\\[info\\][[:space:]]+\\(java.lang.AssertionError.*\\)"
        (1 sbt:error-face))
       ("^\\[info\\].*\\(\\*\\*\\* FAILED \\*\\*\\*\\)"
        (1 sbt:error-face))
       ("^\\[info\\].*\\(!!! IGNORED !!!\\)"
        (1 sbt:warning-face))
       ("^\\[info\\][[:space:]]-[[:space:]]\\(should.*\\)"
        (1 sbt:info-face))
       ("^\\[\\(error\\)\\]"
        (1 sbt:error-face))
       ("^\\[\\(warn\\)\\]"
        (1 sbt:warning-face))
       ("^\\[\\(success\\)\\]"
        (1 sbt:info-face))))
    (compilation-setup t)))


;;; local.el ends here
