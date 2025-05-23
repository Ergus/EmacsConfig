;;; simple-16-theme.el --- Custom theme for faces  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena <spacibba@aol.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(deftheme simple-16
  "Dark theme with a set of simple 16 colors only.")

(defmacro simple-16-theme-color (colorname)
  "Get color by name COLORNAME from `simple-16-color-theme-alist'."
  (alist-get colorname '((black . "#000000")
                         (red . "#cd0000")
                         (green . "#00cd00")
                         (yellow . "#cdcd00")
                         (blue . "#0000ee")
                         (magenta . "#cd00cd")
                         (cyan . "#00cdcd")
                         (white . "#e5e5e5")
                         (brightblack . "#444444") ;;
                         (brightred . "#ff0000")
                         (brightgreen . "#00ff00")
                         (brightyellow . "#ffff00")
                         (brightblue . "#5c5cff")
                         (brightmagenta . "#ff00ff")
                         (brightcyan . "#00ffff")
                         (brightwhite . "#ffffff"))))

(custom-theme-set-faces
 'simple-16
 `(default ((t :background ,(simple-16-theme-color black)
               :foreground ,(simple-16-theme-color white))))

 `(shadow ((t :background ,(simple-16-theme-color black)
              :foreground ,(simple-16-theme-color brightblack))))
 `(error ((t :foreground ,(simple-16-theme-color brightred))))

 `(font-lock-builtin-face ((t :foreground ,(simple-16-theme-color green))))
 `(font-lock-comment-face ((t :foreground ,(simple-16-theme-color blue))))
 `(font-lock-constant-face ((t :foreground ,(simple-16-theme-color magenta))))
 `(font-lock-doc-face ((t :foreground ,(simple-16-theme-color cyan))))
 `(font-lock-function-name-face ((t :foreground ,(simple-16-theme-color white))))
 `(font-lock-keyword-face ((t :foreground ,(simple-16-theme-color yellow))))
 `(font-lock-preprocessor-face ((t :foreground ,(simple-16-theme-color magenta))))
 `(font-lock-string-face ((t :foreground ,(simple-16-theme-color red))))
 `(font-lock-type-face ((t :foreground ,(simple-16-theme-color green))))
 `(font-lock-variable-name-face ((t :foreground ,(simple-16-theme-color white))))
 ;; New ts faces
 `(font-lock-function-call-face ((t :foreground ,(simple-16-theme-color magenta))))
 `(font-lock-number-face ((t :foreground ,(simple-16-theme-color red))))
 `(font-lock-variable-name-face ((t :foreground ,(simple-16-theme-color green))))
 `(font-lock-variable-use-face ((t :foreground ,(simple-16-theme-color cyan))))
 `(font-lock-property-name-face ((t :foreground ,(simple-16-theme-color green))))

 `(highlight ((default :background ,(simple-16-theme-color blue) :foreground unspecified)
	      (((class color) (min-colors 16)) :background ,(simple-16-theme-color brightblack))))

 `(secondary-selection ((t :background ,(simple-16-theme-color blue) :foreground unspecified)
			(((class color) (min-colors 16)) :background ,(simple-16-theme-color brightblack))))

 `(isearch ((t :background ,(simple-16-theme-color blue)
	       :foreground unspecified
	       :weight ultrabold)))

 `(lazy-highlight ((default :background ,(simple-16-theme-color cyan))
		   (((class color) (min-colors 16)) :background ,(simple-16-theme-color brightblack))))

 `(region ((default :background ,(simple-16-theme-color blue)
		    :foreground ,(simple-16-theme-color white))
	   (((class color) (min-colors 16)) :background ,(simple-16-theme-color brightblue) :foreground unspecified)))

 `(trailing-whitespace ((default :background ,(simple-16-theme-color cyan))
			(((class color) (min-colors 16)) :background ,(simple-16-theme-color brightblack))))

 `(whitespace-tab ((t :background unspecified :foreground ,(simple-16-theme-color brightblack))))

 ;; Modeline
 `(mode-line ((t :background ,(simple-16-theme-color blue)
		 :foreground ,(simple-16-theme-color white)
		 :weight bold)))

 `(mode-line-inactive ((default :background ,(simple-16-theme-color black)
				:foreground ,(simple-16-theme-color white)
				:weight normal)
		       (((class color) (min-colors 16))
			:background ,(simple-16-theme-color brightblack)
			:foreground ,(simple-16-theme-color brightwhite))))

 ;; line numbers
 `(line-number ((default :foreground ,(simple-16-theme-color white))
		(((class color) (min-colors 16)) :foreground ,(simple-16-theme-color brightblack))))

 `(line-number-current-line ((t :foreground ,(simple-16-theme-color green))))

 ;; column indicator
 `(fill-column-indicator ((default :foreground ,(simple-16-theme-color white))
			  (((class color) (min-colors 16)) :foreground ,(simple-16-theme-color brightblack))))

 `(show-paren-match ((default :background ,(simple-16-theme-color blue))
		     (((class color) (min-colors 16)) :background ,(simple-16-theme-color brightblack))))

 ;; tab-bar
 `(tab-bar ((t :inherit default :weight light)))
 `(tab-bar-tab ((t :inherit tab-bar :background ,(simple-16-theme-color blue) :weight ultra-bold)))
 `(tab-bar-tab-inactive ((t :inherit tab-bar :foreground ,(simple-16-theme-color white))))

 ;; tab-line
 `(tab-line ((t :inherit default :weight light)))
 `(tab-line-tab ((t :inherit tab-line)))
 `(tab-line-tab-current ((t :inherit tab-line :background ,(simple-16-theme-color blue) :weight ultra-bold)))
 `(tab-line-tab-inactive ((t :inherit tab-line :foreground ,(simple-16-theme-color white))))

 ;; Some others
 `(Man-overstrike ((t :inherit font-lock-type-face :bold t)))
 `(Man-underline ((t :inherit font-lock-keyword-face :underline t)))

 `(which-func ((t :background unspecified :foreground ,(simple-16-theme-color white))))

 `(dired-directory ((t :foreground ,(simple-16-theme-color cyan))))

 ;;`(tooltip ((t :background ,(simple-16-theme-color brightblack) :foreground ,(simple-16-theme-color white))))
 
 ;; External packages
 ;; Company
 `(company-tooltip ((default :background ,(simple-16-theme-color blue)
			     :foreground ,(simple-16-theme-color white))
		    (((class color) (min-colors 16)) :background ,(simple-16-theme-color brightblack))))

 `(company-tooltip-common ((t :inherit company-tooltip
                              :foreground ,(simple-16-theme-color green))))
 `(company-tooltip-selection ((t :background ,(simple-16-theme-color blue)
                                 :weight ultra-bold)))
 `(company-tooltip-scrollbar-track ((default :background ,(simple-16-theme-color blue))
				    (((class color) (min-colors 16)) :background ,(simple-16-theme-color brightblack))))

 `(company-tooltip-scrollbar-thumb ((t :background ,(simple-16-theme-color blue))))

 ;; Avy
 `(avy-lead-face ((t :background ,(simple-16-theme-color blue)
		     :foreground ,(simple-16-theme-color brightwhite))))

 ;; Flycheck
 `(flycheck-error ((t :inherit nil :background unspecified :foreground unspecified :underline t)))

 ;; Flymake
 ;; Error in line
 `(flymake-end-of-line-diagnostics-face ((t :inherit shadow :slant italic))) ;; make it less annoying

 `(eshell-prompt ((t :weight bold :background unspecified
		     :foreground ,(simple-16-theme-color brightblack))))

 ;; highlight-escape-sequences
 `(hes-escape-backslash-face ((t :inherit font-lock-regexp-grouping-backslash
				 :foreground ,(simple-16-theme-color magenta))))
 `(hes-escape-sequence-face ((t :inherit font-lock-regexp-grouping-construct
				:foreground ,(simple-16-theme-color magenta))))
 ;; mu4e
 `(mu4e-unread-face ((t :inherit nil :foreground ,(simple-16-theme-color blue) :weight bold)))
 `(mu4e-system-face ((t :inherit nil :foreground ,(simple-16-theme-color green))))
 `(mu4e-trashed-face ((t :inherit nil :foreground ,(simple-16-theme-color red))))
 ;;`(mu4e-replied-face ((t (:inherit nil :foreground ,(simple-16-theme-color blue)))))
 `(mu4e-header-highlight-face ((t :inherit hl-line :weight bold :underline nil)))

 ;; eglot
 ;; hints inline
 `(eglot-inlay-hint-face ((t :inherit shadow :weight ultra-light :slant italic)))
 `(eglot-diagnostic-tag-unnecessary-face ((t :inherit nil :background unspecified :foreground unspecified :underline t)))
 `(eglot-diagnostic-tag-deprecated-face ((t :inherit nil :background unspecified :foreground unspecified :underline t :strike-through t)))

 ;; tty menu
 `(tty-menu-enabled-face ((t :inherit nil
			     :foreground ,(simple-16-theme-color black)
			     :background ,(simple-16-theme-color white)
			     :weight bold)))
 `(tty-menu-disabled-face ((t :inherit tty-menu-enabled-face
			      :foreground ,(simple-16-theme-color brightblack)
			      :weight ultra-light)))
 
 `(tty-menu-selected-face ((t :inherit tty-menu-enabled-face
			      :background ,(simple-16-theme-color cyan))))

 )

(provide-theme 'simple-16)

;;; simple-16-theme.el ends here
