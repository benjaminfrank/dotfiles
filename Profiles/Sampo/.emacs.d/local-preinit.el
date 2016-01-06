;;; preinit.el --- Adds use-package and ELPA sources -*- lexical-binding: t -*-

;; Copyright (C) 2016 Sam Halliday
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Runs before `init.el' to provide `use-package' and set up the ELPA package
;;  archives.
;;
;;  Intentionally kept machine local so that it can be swapped out in
;;  environments that do not have access to these repositories.
;;
;;; Code:

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

;; Local Variables:
;; compile-command: "cask exec ert-runner preinit.el"
;; End:

;;; preinit.el ends here
