(deftheme Darkula
  ;; based on https://github.com/bmdhacks/emacs-color-theme-darkula
  "Inspired by IntelliJ's Darcula theme")

;; NOTE: https://github.com/alezost/alect-themes/#emacs-2431-and-earlier

;; "C-u C-x =" useful for inspecting misbehaving faces.
;; "M-x list-faces-display" useful for listing everything that new major modes introduce.

(custom-theme-set-faces
 'Darkula
 '(default ((t (:inherit nil :stipple nil :background "#2B2B2B" :foreground "#a9b7c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry nil :family "Inconsolata"))))
 '(cursor ((t (:foreground "#042028" :background "#708183"))))
 '(error ((t (:inherit 'default :underline (:style wave :color "red")))))
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(variable-pitch ((t (:family "Liberation Serif"))))
 ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
 '(font-lock-warning-face ((t (:underline (:style wave :color "orange" :inherit 'default)))))
					;for a construct that is peculiar, or that greatly changes the meaning of other text.
 '(font-lock-function-name-face ((t (:foreground "#fec66c" :inherit 'default))))
					;for the name of a function being defined or declared. 
 '(font-lock-variable-name-face ((t (:inherit 'default))))
					;for the name of a variable being defined or declared. 
 '(font-lock-keyword-face ((t (:foreground "#cc7832" :inherit 'default))))
					;for a keyword with special syntactic significance, like ‘for’ and ‘if’ in C. 
 '(font-lock-comment-face ((t (:foreground "#808080" :inherit 'default))))
					;for comments. 
 '(font-lock-comment-delimiter-face ((t (:inherit 'font-lock-comment-face))))
					;for comments delimiters, like ‘/*’ and ‘*/’ in C.
 '(font-lock-type-face ((t (:foreground "#4e807d" :inherit 'default))))
					;for the names of user-defined data types. 
 '(font-lock-constant-face ((t (:foreground "#6897bb" :weight bold :inherit 'font-lock-variable-name-face))))
					;for the names of constants, like ‘NULL’ in C. 
 '(font-lock-builtin-face ((t (:inherit 'font-lock-keyword-face))))
					;for the names of built-in functions. 
 '(font-lock-preprocessor-face ((t (:inherit 'font-lock-builtin-face))))
					;for preprocessor commands.
 '(font-lock-string-face ((t (:foreground "#a6c25c" :inherit 'default))))
					;for string constants. 
 '(font-lock-doc-face ((t (:foreground "#629755" :inherit 'font-lock-comment-face))))
					;for documentation strings in the code.
 '(font-lock-negation-char-face ((t (:underline (:color foreground-color :style line) :inherit 'default))))
					;for easily-overlooked negation characters.
 '(flymake-errline ((t (:inherit 'error))))
 '(flymake-warnline ((t (:inherit 'warning)))) 
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:weight bold :slant normal :underline nil :inverse-video nil :foreground "#259185"))))
 '(highlight ((t (:background "#0a2832"))))
 '(region ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#465a61" :background "#042028"))))
 '(shadow ((t (:foreground "#465a61"))))
 '(secondary-selection ((t (:background "#0a2832"))))
 '(trailing-whitespace ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#c60007" :background "red1"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:weight normal :slant normal :underline (:color foreground-color :style line) :inverse-video nil :foreground "#5859b7"))))
 '(link-visited ((t (:weight normal :slant normal :underline (:color foreground-color :style line) :inverse-video nil :foreground "#c61b6e" :inherit (link)))))
 '(fringe ((t (:background nil :foreground nil))))
 '(header-line ((t (:weight normal :slant normal :underline nil :box nil :inverse-video t :foreground "#708183" :background "#0a2832" :inherit (mode-line)))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:weight normal :slant normal :underline nil :box nil :inverse-video t :foreground "#3c3f41" :background "#a9b7c6"))))
 '(mode-line-inactive ((t (:weight normal :slant normal :underline nil :box nil :inverse-video t :foreground "#3c3f41" :background "#313335" :inherit (mode-line)))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88))) (t (:inherit (highlight)))))
 '(isearch ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#bd3612" :background "#042028"))))
 '(isearch-fail ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#bd3612" :background "#042028"))))
 '(lazy-highlight ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#a57705" :background "#042028"))))
 '(compilation-info ((t (:weight bold :foreground "#a6c25c" :underline nil))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1"))
          (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3"))
          (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow"))
          (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue"))
          (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'Darkula)
