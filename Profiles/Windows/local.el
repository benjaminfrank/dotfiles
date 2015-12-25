;;; local.el --- Windows Config -*- lexical-binding: t -*-

;; Copyright (C) 2015 Sam Halliday
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Windows setup, primarily for manual testing of ENSIME on Windows.
;;  meahahae.
;;
;;; Code:

(setq debug-on-error t)

(required 'darcula-theme (lambda() (set-frame-font "Courier-14")) t)
;; Note: F11 goes fullscreen

;; External apps:

;; WORKAROUND: some apps don't support spaces in the path, so use DOS names

;; JDK from http://www.oracle.com/technetwork/java/javase/downloads/java-archive-downloads-javase6-419409.html#jdk-6u45-oth-JPR
;; Don't forget to set PATH according to https://docs.oracle.com/javase/tutorial/essential/environment/paths.html
;; (I also recommend setting JAVA_HOME and JDK_HOME)

;; SBT from http://www.scala-sbt.org/0.13/tutorial/Installing-sbt-on-Windows.html
;; C:/Program Files/sbt/bin/sbt.bat
(setq sbt:program-name "C:/PROGRA~1/sbt/bin/sbt.bat")


;; Silver Searcher from http://blog.kowalczyk.info/software/the-silver-searcher-for-windows.html
;; C:\Program\ Files/Ag/ag.exe --- doesn't work on 32 bit

;; Aspell "full installer" and English dict from http://aspell.net/win32/
(setq ispell-program-name "C:/Program Files/Aspell/bin/aspell.exe")

;; CTags from http://ctags.sourceforge.net/
(setq ctags-update-command "C:/PROGRA~1/Ctags/ctags58/ctags.exe -e -R")
(setq ctags-command ctags-update-command) ;; *sigh*

;; Git (intentionally 1.8) from https://github.com/msysgit/msysgit/releases/
;; to be used with http://magit.vc/elpa/v1/ (not version from MELPA)
(setq magit-git-executable "C:/Program Files/Git/bin/git.exe")

;;; local.el ends here
