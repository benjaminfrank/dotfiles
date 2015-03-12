(setq user-emacs-directory (expand-file-name "./emacs.d"))

(defadvice message (before ensime-log-to-file (format-string &rest args))
  (let ((text (when format-string
		(format "%s\n" (apply 'format format-string args)))))
    (princ text 'external-debugging-output)
    text))
(when (not (null window-system))
  (ad-activate 'message))

(setq command-error-function
      (lambda(a b c)
        (message "%s %s %s" a b c)
        (kill-emacs 1)))
