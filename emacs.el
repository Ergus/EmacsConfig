;;; init.el --- Emacs Initialization and Configuration
;; Copyright (C) 2018 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;; Code:

;;__________________________________________________________
;; For using Melpa and Elpa
(require 'package)      ;; You might already have this line
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;;__________________________________________________________
;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq-default use-package-verbose t))

(use-package paradox :ensure t
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config (setq paradox-execute-asynchronously t))

;;__________________________________________________________
;; Benchmark-init
(use-package benchmark-init :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;__________________________________________________________
;; To put all my lisp scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(ecb-options-version "2.50")
 '(large-file-warning-threshold 100000000)
 '(mumamo-submode-indent-offset 4)
 '(org-agenda-files (quote ("~/file.org")))
 '(package-selected-packages
   (quote
	(counsel-projectile ibuffer-projectile projectile better-shell desktop-environment swiper company-reftex smartparens vdiff neotree tramp-term exec-path-from-shell flycheck-status-emoji flycheck-popup-tip corral ivy-historian historian calfw cmake-font-lock dired-sidebar notmuch flycheck-color-mode-line irony systemd lua-mode rust-mode julia-mode markdown-mode cuda-mode column-enforce-mode move-text yasnippet-snippets which-key winum all-the-icons-ivy spaceline-all-the-icons spaceline spacemacs-theme ibuffer-sidebar ibuffer-tramp imenu-anywhere helm smart-mode-line-powerline-theme company-quickhelp symon gnuplot paradox irony-eldoc pyenv-mode python-mode flycheck-pycheckers ein elpy highlight-escape-sequences highlight-numbers diminish flyspell-correct-ivy helm-c-yasnippet helm-smex helm-tramp helm-cscope xcscope counsel-etags counsel-gtags ggtags helm-gtags magit bbdb- counsel-bbdb highlight-blocks counsel-notmuch counsel-tramp highlight-indent-guides highlight-parentheses smex counsel ivy multi-term bongo flycheck-ycmd company-ycmd ycmd modern-cpp-font-lock anzu smart-mode-line clean-aindent-mode multiple-cursors d-mode jabber exwm benchmark-init tabbar cobol-mode shell-pop smart-tabs-mode elscreen yasnippet yaxception flycheck-clang-analyzer flycheck-julia langtool company-go auctex company-auctex sphinx-mode qt-pro-mode opencl-mode flyspell-popup alert async bbdb bind-key cl-generic cmake-mode company concurrent emms flycheck js2-mode let-alist math-symbol-lists polymode popup with-editor sunrise-x-buttons sunrise-commander ruby-tools ruby-electric nasm-mode markdown-mode+ hlinum highlight go-snippets go-mode gnuplot-mode flycheck-rust flycheck-irony flycheck-cstyle f90-interface-browser elpa-mirror ecb company-math company-lua company-jedi company-irony-c-headers company-irony company-c-headers company-bibtex cmake-project bbdb-vcard bbdb-handy)))
 '(paradox-github-token t)
 '(same-window-buffer-names
   (quote
	("*eshell*" "*Python*" "*shell*" "*Buffer List*" "*scheme*" "*")))
 '(tabbar-separator (quote (1))))

;;__________________________________________________________
;; Internal options

(global-font-lock-mode t)      ;; Use font-lock everywhere.
(setq font-lock-maximum-decoration t)
(savehist-mode t)              	 ;; Historial
(auto-compression-mode t)      	 ;; Uncompress on the fly:
(show-paren-mode t)            	 ;; Highlight couple parentesis
(auto-revert-mode t)             ;; Autoload files changed in disk
(global-linum-mode t)            ;; Numero de linea a la izquierda
(delete-selection-mode)          ;; Sobreescribe seleccion al pegar
(menu-bar-mode -1)               ;; Quitar barra superior (no la uso)
;;(desktop-save-mode 1)          ;; Save open windows before close, for next section

(setq-default vc-follow-symlinks nil	            ;; Open links not open
			  transient-mark-mode t     ;; Highlight marked region
			  line-number-mode t        ;; Display line numbers
			  column-number-mode t      ;; Display column numbers
			  linum-format "%4d\u2502"  ;; Formato numero linea
			  tab-always-indent 't      ;; make tab key do indent only
			  initial-scratch-message "Welcome Jimmy!!"
			  ring-bell-function 'ignore
			  user-full-name "Jimmy Aguilar Mena"
			  inhibit-startup-message t
			  inhibit-startup-screen t
			  tab-width 4               ;; Tabulador a 4
			  make-backup-files nil     ;; Sin copias de seguridad
			  create-lockfiles nil      ;; No lock files, goot for tramp
			  visible-bell nil          ;; Flash the screen (def)
			  scroll-step 1             ;; Scroll one by one
			  ;;scroll-preserve-screen-position 1
			  scroll-conservatively 100000
			  scroll-margin 0
			  fill-column 80            ;; default is 70
			  tooltip-mode t            ;; Tool tip in the echo
			  )

;;__________________________________________________________
;;Packages options
;;__________________________________________________________

;;__________________________________________________________
; Two options for diffs
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (with-eval-after-load 'winner
	(add-hook 'ediff-after-quit-hook-internal #'winner-undo)))

;; more like vimdiff
(use-package vdiff :ensure t
  :commands (vdiff-files
             vdiff-files3
             vdiff-buffers
			 vdiff-buffers3))

;;__________________________________________________________
;; Diminish To Hide Packages from bar
(use-package diminish :ensure t)

;;__________________________________________________________
;; Smart-parents (parentesis correctos para los modos)
(use-package smartparens :ensure t
  :init (smartparens-global-mode)
  :config
    (use-package smartparens-config :ensure smartparens)
    (smartparens-global-mode))


;;__________________________________________________________
;; Mocp and multi-term music player
(use-package multi-term :ensure t
  :config
  (autoload 'mocp "mocp" "mocp in emacs" t))

;;__________________________________________________________
;; Indentado on new line smart way

;;(use-package clean-aindent-mode :ensure t     ;; Elimina el indentado extra, mejor que los anteriores para programar
;;  :bind("RET" . newline-and-indent)
;;  :init
;;  (electric-indent-mode -1)  ; no electric indent, auto-indent is sufficient
;;  (clean-aindent-mode t)
;;  (setq clean-aindent-is-simple-indent t))

;;__________________________________________________________
;; Keys
(use-package bind-key :ensure t)

;;__________________________________________________________
;; which-key
(use-package which-key :ensure t
  :diminish
  :config
  (which-key-mode t))

;;__________________________________________________________
;; winum (windows number)
(use-package winum :ensure t
  :config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode))

;;__________________________________________________________
;; Status bar (mode line in emacs) two options to chose

;;(use-package smart-mode-line :ensure t
;;  :config
;;  (setq sml/theme 'powerline)
;;  (sml/setup))

;; (use-package powerline :ensure t
;;   :config
;;   (powerline-default-theme))

(use-package spaceline :ensure t
  :demand t
  :init
  (if (display-graphic-p)
	  (setq powerline-default-separator 'arrow-fade)
	(setq powerline-default-separator 'utf-8))
  :config
  (use-package spaceline-config :ensure spaceline
	:init
	(setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
	:config
	(spaceline-spacemacs-theme))
	;;(spaceline-toggle-minor-modes-off))

  (set-face-attribute 'mode-line nil :background "#5c5cff" :foreground "white"))

;;__________________________________________________________
;; Clipboard copy and paste with: M-w & C-c v
(defun my/xclipboard () "Define my clipboard functions with xsel."
       (defun xcopy () "Copies selection to x-clipboard."
              (interactive)
			  (if (region-active-p)
				  (if (display-graphic-p)
					  (progn
						(message "Copied region to x-clipboard!")
						(call-interactively 'clipboard-kill-ring-save))
					(progn
					  (shell-command-on-region
					   (region-beginning) (region-end) "xsel -i -b")
					  (message "Copied region to clipboard!") ;
					  (deactivate-mark)
					  (call-interactively 'kill-ring-save)))
				(message "No region active; can't yank!")))

       (defun xpaste () "Pastes from x-clipboard."
              (interactive)
              (if (display-graphic-p)
				  (clipboard-yank)
				(insert (shell-command-to-string "xsel -o -b"))))

	   (global-set-key (kbd "M-w") 'xcopy)
       (global-set-key (kbd "C-S-v") 'xpaste)

	   ;; =================== For Lorentz ======================
	   (defun aberrant-copy () "Lorentz's aberrant copy."
			  (interactive)
			  (call-interactively 'mouse-set-region)
			  (if (use-region-p)
				  (progn
					(shell-command-on-region
					 (region-beginning) (region-end) "xsel -i -p")
					(message "Copied region to PRIMARY!"))
				(message "No region active; can't yank!")))

	   (defun aberrant-paste () "Lorentz's aberrant paste."
			  (interactive)
			  (insert (shell-command-to-string "xsel -o -p")))

	   (global-set-key [drag-mouse-1] 'aberrant-copy)
	   (global-set-key [mouse-2] 'aberrant-paste))

(if (executable-find "xsel")
	(my/xclipboard)
  (message "No xsel in your path, install it in your system!!!"))

;;__________________________________________________________
;; hlinum
(use-package hlinum :ensure t ;; resalta el numero de la linea
  :config
  (hlinum-activate)           ;; Highlight linenum
  ;; Keep highlighted linenum in all buffers
  ;;(setq-default linum-highlight-in-all-buffersp t)
  )

;;__________________________________________________________
;; Move current line up and down Shift+arrow
(use-package move-text :ensure t
  :bind
  (([(control shift up)] . move-text-up)
   ([(control shift down)] . move-text-down)))

;;__________________________________________________________
;;  Seleccionar con el mouse
(use-package mouse
  :config
  (xterm-mouse-mode t)            ;; mover el cursor al click
  (defun track-mouse (e))
  (setq-default mouse-sel-mode t) ;; Mouse selection
  (set-mouse-color "white")       ;; Flechita del mouse en blanco
  (mouse-wheel-mode t)            ;; scrolling con el mouse
  )

  (global-set-key [drag-mouse-2] 'mouse-yank-at-click)

(blink-cursor-mode 0)             ;; Parpadeo del cursor modo texto
(set-cursor-color "white")        ;; Set cursor and mouse colours

;;__________________________________________________________
;; Multiple Cursors
(global-unset-key (kbd "C-c <down-mouse-1>"))
(use-package multiple-cursors  :ensure t ;; Multiple cursors package
  :bind (("C-c m" . mc/edit-lines)
		 ("C-c n" . mc/mark-next-like-this)
		 ("C-c p" . mc/mark-previous-like-this)
         ("C-c <mouse-1>" . mc/add-cursor-on-click)))

;;__________________________________________________________
;; My program's mode hooks

(defun my/prog-mode-hook () "Some hooks only for prog mode."
	   (which-function-mode t)     ;; Shows the function in spaceline
	   (electric-pair-mode t)      ;; Autoannadir parentesis
	   (electric-indent-mode t)    ;; Corrige indentacion con tab o enter (now default)
	   (setq show-trailing-whitespace t)
	   )

(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;;__________________________________________________________
;; Mark column 80 when crossed
(use-package column-enforce-mode :ensure t
  :diminish
  :hook prog-mode
  :config
  (column-enforce-mode t)
  (setq column-enforce-comments nil)
  ;;(setq column-enforce-column <your desired column>)
  (set-face-attribute 'column-enforce-face nil
					  :background "gray90" :foreground "black"))

;;__________________________________________________________
;;  The Colors (I want to change this for a real theme, there are maaaaany)

(defun my/colors () "Define my color theme."
       (set-background-color "black")
       (set-foreground-color "white")

       (set-face-foreground 'bold "LightGoldenrod")
       (set-face-foreground 'bold-italic "grey20")
       (set-face-foreground 'italic "yellow3")

       (set-face-foreground 'font-lock-preprocessor-face "magenta3")    ;; Preprocessor

       ;;(set-face-foreground 'font-lock-comment-face "LightSalmon4")   ;; Comentarios
       ;;(set-face-foreground 'font-lock-doc-face "LightSalmon1")       ;; Documentation
       (set-face-foreground 'font-lock-comment-face "blue")         ;; Comentarios
       (set-face-foreground 'font-lock-doc-face "blue")             ;; Documentation

       (set-face-foreground 'font-lock-string-face "red")           	;; Strings
       (set-face-foreground 'font-lock-function-name-face "white")  	;; Funciones
       (set-face-foreground 'font-lock-variable-name-face "white")  	;; Variables
       (set-face-foreground 'font-lock-constant-face "DeepPink1")   	;; Constates y Clases

       (set-face-foreground 'font-lock-type-face "green")       ;; Tipos (int, float)
       (set-face-foreground 'font-lock-keyword-face "DarkGoldenrod1")   ;; Keywords (for, if)
       (set-face-foreground 'font-lock-builtin-face "green4")   ;; Keywords (for, if)

       (set-face-attribute 'highlight nil :foreground "red")

       (set-face-attribute 'secondary-selection nil :background "darkblue" :foreground "skyblue")

       ;; search C-s, resalta lo que encuentra
       (set-face-attribute 'isearch nil :background "blue" :foreground "white") ;; Busqueda
       (set-face-attribute 'region nil :background "white" :foreground "black") ;; Seleccion C-space
       (set-face-attribute 'linum nil :background "black" :foreground "green")  ;; resalta la linea actual
	   ;; color de la linea en el panel activo/inactivo
	   ;; (set-face-attribute  'mode-line nil :foreground "black" :background "gray90":box '(:line-width 1 :style released-button))
	   ;; (set-face-attribute  'mode-line-inactive nil :foreground "black" :background "gray70" :box '(:line-width 1 :style released-button))
       )

(my/colors)

;;__________________________________________________________
;; Lineas de Indentado
(use-package highlight-indent-guides :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "gray20"))

;;__________________________________________________________
;; Resalta parentesis entorno al cursor
(use-package highlight-parentheses :ensure t
  :diminish
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)

  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)

  (setq hl-paren-colors
		(quote
		 ("brightgreen" "IndianRed1" "IndianRed3" "IndianRed4"))))

;;__________________________________________________________
;; Resalta scopes entorno al cursor
(use-package highlight-blocks :ensure t
  :config
  (define-key function-key-map "\e[1;5R" [C-f3])
  (global-set-key (kbd "C-<f3>") 'highlight-blocks-now)

  (set-face-attribute 'highlight-blocks-depth-2-face nil :background "gray15")
  (set-face-attribute 'highlight-blocks-depth-3-face nil :background "gray20")
  (set-face-attribute 'highlight-blocks-depth-4-face nil :background "gray25")
  (set-face-attribute 'highlight-blocks-depth-5-face nil :background "gray30")
  (set-face-attribute 'highlight-blocks-depth-6-face nil :background "gray35")
  (set-face-attribute 'highlight-blocks-depth-7-face nil :background "gray40")
  (set-face-attribute 'highlight-blocks-depth-8-face nil :background "gray45")
  (set-face-attribute 'highlight-blocks-depth-9-face nil :background "gray50")
  )

(use-package highlight-escape-sequences :ensure t
  :config
  (add-hook 'prog-mode-hook 'hes-mode)
  (set-face-attribute 'hes-escape-backslash-face nil :foreground "magenta")
  (set-face-attribute 'hes-escape-sequence-face nil :foreground "magenta"))

(use-package highlight-numbers :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (set-face-attribute 'highlight-numbers-number nil :foreground "red")
  )

;;__________________________________________________________
;; Flyspell (Orthography)
(use-package flyspell :ensure t
  :diminish
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (use-package flyspell-popup :ensure t
	:bind (:map flyspell-mode-map
				("C-c ." . flyspell-popup-correct)))

  (use-package flyspell-correct-ivy :ensure t
	:commands (flyspell-correct-ivy)
	:bind (:map flyspell-mode-map
				("C-c ;" . flyspell-correct-previous-word-generic))
	:init
	(setq flyspell-correct-interface #'flyspell-correct-ivy)))

;;__________________________________________________________
;; {c/c++}-mode
;;__________________________________________________________

;;__________________________________________________________
;; Indent with tabs align with spaces
(use-package smart-tabs-mode :ensure t
  :config
  (smart-tabs-insinuate 'c 'c++))

;;__________________________________________________________
;; Improve completion for cmake project (I don't need this now)
;;(use-package cmake-ide :ensure t
;;  :config
;;  (add-hook 'c-mode-common-hook #'cmake-ide-setup))

;;__________________________________________________________
;; Cscope for c-mode (go to functions)
(use-package xcscope :ensure t
  :init
  (add-hook 'c-mode-common-hook 'cscope-setup)
  :bind (("C-c ," . cscope-find-global-definition-no-prompting)
		 ("C-c d" . cscope-find-global-definition)
		 ("C-c f" . cscope-find-this-symbol)
		 ("C-c *" . cscope-pop-mark)))

;;__________________________________________________________
;; C common mode (for all c-like languajes)

(defun my/c-mode-common-hook () "My hook for C and C++."

	   (use-package company-c-headers :ensure t ;; company-c-headers
		 :after company
		 :config
		 (add-to-list 'company-backends 'company-c-headers))
	   
	   (use-package preproc-font-lock :ensure t ;; Preprocessor
		 :config
		 (preproc-font-lock-global-mode 1)
		 (set-face-attribute 'preproc-font-lock-preprocessor-background nil
							 :inherit 'font-lock-preprocessor-face))
	   
	   (c-set-offset 'cpp-macro 0 nil)
	   (message "Loaded my/c-mode common"))

(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

;;__________________________________________________________
;; c++-init-hook (before all the others)
(defun my/c-init-hook ()   "Initialization for 'c-mode' run before any other."
	   (setq c-doc-comment-style
			 '((java-mode . javadoc)
			   (pike-mode . autodoc)
			   (c-mode    . javadoc)
			   (c++-mode  . javadoc))

			 c-basic-offset 4       ;; Default is 2
			 c-indent-level 4       ;; Default is 2
			 indent-tabs-mode t)

	   (setq-default c-default-style
					 '((java-mode . "java")
					   (awk-mode . "awk")
					   (other . "linux"))))

;; This hook run before any other hook in c-mode
(add-hook 'c-initialization-hook 'my/c-init-hook)

;;__________________________________________________________
;; Agrega doble indentation a clases y simple a structs (para private y public)
(defun my/c++-lineup-inclass (langelem) "LANGELEM Offset struct vs class."
       (let ((inclass (assoc 'inclass c-syntactic-context)))
         (save-excursion
           (goto-char (c-langelem-pos inclass))
           (if (or (looking-at "struct")
                   (looking-at "typedef struct"))
               '+
             '++))))

;;__________________________________________________________
;; C++ mode
(use-package modern-cpp-font-lock :ensure t
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(defun my/c++-mode-hook () "My C++-Mode hook function."
	   (setq flycheck-gcc-language-standard "c++11")
       (c-set-offset 'inclass 'my/c++-lineup-inclass)
	   (message "Loaded my c++-mode")
       )

(add-hook 'c++-mode-hook 'my/c++-mode-hook)

;; Even if the file extension is just .c or .h, assume it is a C++ file:
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))

;;__________________________________________________________
;; Cuda
(use-package cuda-mode :ensure t
  :mode "\\.cu\\'")

;;__________________________________________________________
;; OpenCL Mode
(use-package opencl-mode :ensure t
  :mode "\\.cl\\'")

;;__________________________________________________________
;; Markdown mode
(use-package markdown-mode :ensure t
  :mode "\\.md\\'"
  :config
  (flyspell-mode 1))

;;__________________________________________________________
;; Restructured text
(use-package rst-mode
  :mode "\\.rst\\'"
  :config
  (use-package sphinx-mode :ensure t
    :hook rst-mode)
  (flyspell-mode 1))

;;__________________________________________________________
;; Makefile
(use-package makefile-mode
  :mode (".*Makefile.*" "\\.mak"))

;;__________________________________________________________
;; ruby-mode
(use-package ruby-mode :ensure t
  :mode ("\\.rb\\'" "\\.rjs\\'" "\\Rakefile\\'" "\\Gemfile\\'")
  :config
  (use-package ruby-tools :ensure t)
  (use-package ruby-electric :ensure t
    :hook ruby-electric-mode
    )
  (setq-default ruby-indent-level 2))

;;__________________________________________________________
;; Julia Mode
(use-package julia-mode :ensure t
  :mode "\\.jl\\'"
  :config
  (use-package flycheck-julia :ensure t
    :commands (flycheck-julia-setup)
    :init
    (add-hook 'julia-mode-hook #'flycheck-mode)))

;;__________________________________________________________
;; Rust Mode
(use-package rust-mode :ensure t
  :mode "\\.rs\\'"
  :config
  (use-package flycheck-rust :ensure t
    :commands (flycheck-rust-setup)
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;;__________________________________________________________
;; D languaje
(use-package d-mode :ensure t :mode "\\.d\\'")

;;__________________________________________________________
;; Go languaje
(use-package go-mode :ensure t
  :mode "\\.go\\'"
  :config
  (use-package company-go :ensure t
	:after company
    :config
    (add-to-list 'company-backends 'company-go))

  (use-package go-snippets :ensure t))

;;__________________________________________________________
;; lua language
(use-package lua-mode :ensure t
  :mode "\\.lua\\'"
  :config
  (use-package company-lua :ensure t
	:after company
	:config
	(add-to-list 'company-backends 'company-lua)))

;;__________________________________________________________
;; systemd mode
(use-package systemd :ensure t
  :mode ("\\.service\\'" "\\.timer\\'" "\\.target\\'"
		 "\\.mount\\'" "\\.socket\\'" "\\.slice\\'"
		 "\\.automount\\'"))

;;__________________________________________________________
;; DOS batch files
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;;__________________________________________________________
;; qt-pro-mode
;; Use for Qt's .pro and .pri files
(use-package qt-pro-mode :ensure t
  :mode ("\\.pr[io]\\'")
  :init
  (add-to-list 'auto-mode-alist '("\\.moc\\'" . c++-mode)) ;; Treat .moc files (Qt) as C++
  (add-to-list 'auto-mode-alist '("\\.ui\\'" . xml-mode))  ;; Treat .ui files (Qt) as XML
  )

;;__________________________________________________________
;; javascript-mode
(use-package js2-mode :ensure t
  :mode ("\\.js\\'")
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode))

;;__________________________________________________________
;; xml-mode
(use-package xml-mode
  :mode ("\\.ipe\\'" "\\.qrc\\'" "\\.svn\\'"))

;;__________________________________________________________
;; Completion
'(completion-ignored-extensions
  (quote ("CVS/" ".o" "~" ".bin" ".lbin" ".fasl" ".ufsl" ".a" ".ln" ".blg" ".bbl"
          ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".class" ".fas"
          ".lib" ".x86f" ".sparcf" ".lo" ".la" ".toc" ".log" ".aux" ".cp" ".fn"
          ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".lbl"
          ".out" ".brf" ".ncb" ".sln" ".suo" ".vcproj.AD.ghali.user" ".idb" ".pdb"
          ".synctex.gz" ".svn")))

;;__________________________________________________________
;; splitting
(setq split-width-threshold 110)        ;; Original value 240 ancho minimo limite para split vertical

;; Move split keybindings
(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings)        ;; Move between panes S-arrow
  (setq-default windmove-wrap-around t) ;; Cyclic bound mode
  )

;; (add-hook 'term-setup-hook
;;   '(lambda ()
;;      (define-key function-key-map "\e[1;3A" [M-up])
;;      (define-key function-key-map "\e[1;3B" [M-down])
;;      (define-key function-key-map "\e[1;3C" [M-right])
;;      (define-key function-key-map "\e[1;3D" [M-left])
;; 	 (define-key function-key-map "\e[1;2A" [S-up])
;;      (define-key function-key-map "\e[1;2B" [S-down])
;;      (define-key function-key-map "\e[1;2C" [S-right])
;;      (define-key function-key-map "\e[1;2D" [S-left])
;; 	 )
;;   )

(when (fboundp 'winner-mode) (winner-mode 1))   ;; recuperar Split configuration con C-c left/right

;;__________________________________________________________
;; Confirmation for to exit emacs
(defalias 'yes-or-no-p 'y-or-n-p)     ;; Reemplazar "yes" por "y" en el prompt
;;(fset 'yes-or-no-p 'y-or-n-p)       ;; Igual que el anterior
(setq confirm-kill-emacs 'y-or-n-p)   ;; Puede ser 'nil o 'y-or-n-p

;;__________________________________________________________
;; Lines enabling gnuplot-mode
(use-package gnuplot-mode :ensure t
  :mode ("\\.gp\\'" "\\.gpl\\'" "\\.plt\\'"))

;;__________________________________________________________
;; Auto completamiento
(use-package yasnippet :ensure t
  :diminish
  :after company
  :init
  (yas-global-mode 1)
  (yas-reload-all)
  :config
  (use-package yasnippet-snippets :ensure t)

  (defun my/company-yasnippet () "Company-Yasnippet completion"
	(interactive)
	(company-abort)
	(call-interactively 'company-yasnippet))

  (define-key company-active-map (kbd "M-/") 'my/company-yasnippet)
  (global-set-key (kbd "M-/") 'company-yasnippet))

(use-package company :ensure t
  :defer t
  :bind (("M-RET" . company-complete))
  :init
  (global-company-mode)
  :config
  (define-key company-mode-map (kbd "M-RET") 'company-complete)
  (define-key company-active-map (kbd "M-RET") 'company-other-backend)


  (set-face-attribute 'company-tooltip nil        ;; dialog face
					  :background "brightblack" :foreground "white")
  (set-face-attribute 'company-tooltip-common nil ;; common part face
					  :inherit 'company-tooltip :foreground "green")
  (set-face-attribute 'company-tooltip-selection nil ;; selection face
					  :background "blue" :weight 'ultra-bold)
  (set-face-attribute 'company-scrollbar-bg nil   ;; scroll bar face bg
					  :background "brightblack")
  (set-face-attribute 'company-scrollbar-fg nil   ;; scroll bar face fg
					  :background "blue")

  (setq company-idle-delay 0   ;; no delay for autocomplete
		company-minimum-prefix-length 2
		company-tooltip-limit 20

		company-show-numbers t)

  (use-package company-quickhelp :ensure t
	:after company
	:config
	(company-quickhelp-mode)))

;;__________________________________________________________
;; rtags config (C Completion)
;; (use-package rtags
;;   :config
;;   (defun my/rtags-common-hook () "My rtags loader."
;; 		 (setq rtags-path "/usr/bin")
;;  		 (rtags-start-process-unless-running)
;;  		 (setq rtags-autostart-diagnostics t)
;;  		 (rtags-diagnostics)
;;  		 (setq rtags-completions-enabled t)
;; 		 (setq rtags-display-result-backend 'ivy)
;; 		 ;;(setq rtags-display-result-backend 'helm)
;; 
;; 		 (define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
;; 		 (define-key c-mode-base-map (kbd "M-/") (function rtags-find-references-at-point))
;; 		 (rtags-enable-standard-keybindings)
;; 
;;  		 (use-package company-rtags
;; 		   :after company
;;  		   :config
;; 		   (add-to-list 'company-backends 'company-rtags))
;; 
;; 
;;  		 (use-package flycheck-rtags
;;  		   :after flycheck
;;  		   :config
;;  		   (defun my/flycheck-rtags-hook () "My flycheck-rtags hook."
;;  				  (flycheck-select-checker 'rtags)
;;  				  (setq-local rtags-periodic-reparse-timeout 10)
;; 				  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate.
;; 				  (setq-local flycheck-check-syntax-automatically nil)
;;  				  (setq rtags-enable-unsaved-reparsing t)
;;  				  (message "Loaded flycheck-rtags"))
;;  		   (add-hook 'rtags-mode-hook #'my/flycheck-rtags-hook))
;; 		 (message "Loading my rtags-common-hook")
;; 		 )
;; 
;;   (add-hook 'c-mode-common-hook #'my/rtags-common-hook))

;;__________________________________________________________
;; Irony config (C completions)
(use-package irony :ensure t
  :diminish
  :defer t
  :preface
  (defun my/irony-mode-hook () "My irony mode hook."
		 (unless (and buffer-file-name (file-remote-p buffer-file-name))
		   ;;(irony-mode t)
		   (irony-cdb-autosetup-compile-options)

		   (use-package company-irony :ensure t
			 :after company
			 :config
			 (use-package company-irony-c-headers :ensure t)

			 (add-to-list 'company-backends
						  '(company-irony-c-headers company-irony))

			 (company-irony-setup-begin-commands)
			 (message "Loaded my company-irony"))

		   ;;define-key irony-mode-map [remap completion-at-point]
		   ;; 'irony-completion-at-point-async)
		   ;;define-key irony-mode-map [remap complete-symbol]
		   ;; 'irony-completion-at-point-async)

		   (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
		   (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)

		   (use-package flycheck-irony :ensure t
			 :after flycheck
			 :config
			 (add-hook 'irony-mode-hook #'flycheck-irony-setup)
			 (message "Loaded my flycheck-irony"))

		   (use-package irony-eldoc :ensure t
			 :config
			 (add-hook 'irony-mode-hook #'irony-eldoc))

		   (message "Loaded my-Irony-Mode-hook")))
  :init
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'my/irony-mode-hook))

;;__________________________________________________________
;; ycm (you complete me C completion)
;; (use-package ycmd :ensure t
;;   :init (add-hook 'c-mode-common-hook #'ycmd-mode)
;;   :config
;; 
;;   (set-variable 'ycmd-server-command '("python2" "/home/ergo/gits/ycmd/ycmd"))
;;   (set-variable 'ycmd-global-config	(expand-file-name "~/.emacs.d/ycm_conf.py"))
;; 
;;   ;;(set-variable 'ycmd-extra-conf-whitelist '("~/Repos/*"))
;; 
;;   (use-package company-ycmd :ensure t
;;     :config (add-hook 'ycmd-mode-hook #'company-ycmd-setup))
;; 
;;   (use-package flycheck-ycmd :ensure t
;; 	:config (add-hook 'ycmd-mode-hook #'flycheck-ycmd-setup)))
;; 
;; (use-package eldoc :ensure t
;;   :diminish eldoc-mode
;;   :init (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

;;__________________________________________________________
;; Chequeo de syntaxis
(use-package flycheck :ensure t
  :init (global-flycheck-mode)
  :config

  ;;  (require 'flycheck-cstyle)
  ;;  (flycheck-cstyle-setup)
  ;;  (flycheck-add-next-checker 'c/c++-clang '(warning . cstyle))

  ;;  (require 'flycheck-clang-analyzer)
  ;;  (flycheck-clang-analyzer-setup)

  (setq-default flycheck-display-errors-delay 1)

  (use-package flycheck-popup-tip :ensure t
	:after flycheck
	:config
	(add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode))

  (use-package flycheck-status-emoji :ensure t
	:after flycheck
	:config (add-hook 'flycheck-mode-hook #'flycheck-status-emoji-mode))

  (use-package flycheck-color-mode-line :ensure t
	:init
	(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;;__________________________________________________________
;; Function arguments show

(use-package eldoc :ensure t
  :diminish
  :config
  (eldoc-mode t))

;;__________________________________________________________
;; Chequeo de gramatica
(use-package langtool :ensure t
  :defer t
  :config
  (setq langtool-default-language "en")
  (setq langtool-language-tool-jar "/home/ergo/gits/languagetool/languagetool-standalone/target/LanguageTool-4.2-SNAPSHOT/LanguageTool-4.2-SNAPSHOT/languagetool-commandline.jar"))

;;__________________________________________________________
;; EMMS mode.
(use-package emms :ensure t
  :defer t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq-default emms-source-file-default-directory "~/almacen/Musica/") )

;;__________________________________________________________
;; Email mode for mutt
;;__________________________________________________________
;; bbdb
(use-package abbrev :diminish)

(use-package bbdb :ensure t
  :defer t
  :config
  (bbdb-insinuate-message)
  (use-package bbdb- :ensure t)
  (use-package bbdb-handy :ensure t))

(use-package mail-mode
  :mode "/mutt"
  :config
  (abbrev-mode 1)
  (mail-abbrevs-setup)
  )

;; Asocia buffers que empiecen con messaje mode
(use-package message-mode
  :mode ("mutt-Ergus-*" "draft")
  :config
  (auto-fill-mode t)
  (flyspell-mode t)
  (abbrev-mode t)
  (mail-abbrevs-setup)
  )

;; Autocompleta direcciones
(use-package notmuch :ensure t
  :defer t
  :config
  (require 'notmuch-address)
  (setq notmuch-address-command "/home/ergo/gits/notmuch-addrlookup-c/notmuch-addrlookup"))

;;__________________________________________________________
;; Latex mode

(use-package tex-site :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq LaTeX-babel-hyphen nil
		TeX-auto-save t
		TeX-parse-self t
		LaTeX-always-use-Biber t
		TeX-save-query nil      ;; don't prompt for saving the .tex file
		TeX-newline-function 'reindent-then-newline-and-indent
		TeX-PDF-mode t
		TeX-source-correlate-method 'synctex
		TeX-source-correlate-mode t
		TeX-source-correlate-start-server t
		)
  (setq-default TeX-master nil)

  (setq LaTeX-fill-break-at-separators (quote (\\\( \\\[ \\\])))
  (flyspell-mode t)
  (flyspell-buffer)
  (turn-on-auto-fill)
  (visual-line-mode)
  ;;(LaTeX-math-mode)

  (use-package reftex  ;; Reftex for cross references
	:config
	(add-hook 'LaTeX-mode-hook #'turn-on-reftex)   ; with AUCTeX LaTeX mode

	(reftex-isearch-minor-mode)
	(setq reftex-plug-into-AUCTeX t
		  reftex-cite-prompt-optional-args t   ; Prompt for empty optional arguments in cite
		  reftex-cite-format 'biblatex
		  reftex-plug-into-AUCTeX t
		  reftex-insert-label-flags '(t t)
		  reftex-save-parse-info t
		  reftex-enable-partial-scans t
		  reftex-use-multiple-selection-buffers t
		  )

	(use-package company-reftex :ensure t
	  :after company
	  :config
	  (add-to-list 'company-backends '(company-reftex-labels
									   company-reftex-citations)))
	)

  (use-package company-math :ensure t
	:after company
	:config
	(add-to-list 'company-backends '(company-math-symbols-latex
									 company-latex-commands))
	)

  (use-package company-auctex :ensure t
	:after company
	:config
	(company-auctex-init)
	)

  (add-to-list 'TeX-command-list
			   '("Makeglossaries" "makeglossaries %s" TeX-run-command nil
				 (latex-mode)
				 :help "Run makeglossaries script, which will choose xindy or makeindex") t)

  (add-to-list 'TeX-output-view-style '("^pdf$" "." "evince %o %(outpage)"))
  )

;;__________________________________________________________
;;bibtex mode set use biblatex
(use-package bibtex-mode
  :mode "\\.bib\\'"
  :config
  (bibtex-set-dialect 'biblatex)
  (use-package company-bibtex :ensure t
	:config
	(add-to-list 'company-backends 'company-bibtex)))

;;__________________________________________________________
;; Python mode
(use-package python-mode :ensure t
  :mode ("\\.py" . python-mode)
  :config

  (use-package company-jedi :ensure t
	:after company
	:config
	(add-to-list 'company-backends 'company-jedi))

  (use-package elpy :ensure t
	:init
	(add-hook 'python-mode-hook 'elpy-enable)
	:config
	(setq elpy-rpc-python-command "python3"
		  elpy-rpc-backend "jedi"
		  python-check-command "pyflakes"
		  python-shell-interpreter "ipython3"
		  python-shell-interpreter-args "-i --simple-prompt")

	(use-package flycheck-pycheckers :ensure t
	  :after flycheck
	  :config
	  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))))

;;__________________________________________________________
;; IDO siempre (probare un tiempo con helm/ivy)
;;(require 'ido)
;;(ido-mode t)

;;__________________________________________________________
;; Helm (probare un tiempo con helm/ivy)
;;(use-package helm :ensure t
;;  :bind (("M-x" . helm-M-x)
;;         ("C-x C-f" . helm-find-files)
;;         ("C-x f" . helm-recentf)
;;         ("C-SPC" . helm-dabbrev)
;;         ("M-y" . helm-show-kill-ring)
;;         ("C-x b" . helm-buffers-list))
;;  :bind (:map helm-map
;;	      ("M-i" . helm-previous-line)
;;	      ("M-k" . helm-next-line)
;;	      ("M-I" . helm-previous-page)
;;	      ("M-K" . helm-next-page)
;;	      ("M-h" . helm-beginning-of-buffer)
;;		  ("M-H" . helm-end-of-buffer))
;;  :config
;;  (setq-default helm-display-function 'helm-default-display-buffer)
;;  (setq helm-split-window-in-side-p t)
;;  (setq helm-buffers-fuzzy-matching t)
;;  (helm-mode 1))
;;
;;(use-package helm-descbinds :ensure t
;;  :bind ("C-h b" . helm-descbinds))
;;
;;(use-package helm-files
;;  :bind (:map helm-find-files-map
;;			  ("M-i" . nil)
;;			  ("M-k" . nil)
;;			  ("M-I" . nil)
;;			  ("M-K" . nil)
;;			  ("M-h" . nil)
;;			  ("M-H" . nil)))
;;
;;(use-package helm-swoop :ensure t
;;  :bind (("M-m" . helm-swoop)
;;		 ("M-M" . helm-swoop-back-to-last-point))
;;  :init
;;  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

;;__________________________________________________________
;; Dired-mode settings (file manager)
(use-package dired
  :config
  (setq dired-recursive-copies 'top  ;; Always ask recursive copy
		dired-recursive-deletes 'top ;; Always ask recursive delete
		dired-dwim-target t)         ;; Copy in split mode with p
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Open in the same buffer in dired mode
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive)
										 (find-alternate-file ".."))); was dired-up-directory
  (use-package dired-x
	:config
	(setq dired-guess-shell-alist-user
		  (list
		   (list "\\.\\(ps\|ps.gz\|eps\|eps.gz\|pdf\|PDF\\)$" "evince")
		   (list "\\.\\(rgb\|tiff\|tif\|xbm\|gif\|pgm\|ppm\|bmp\|tga\\)$" "eog ")
		   (list "\\.\\(ppm\|gif\|png\|jpg\|JPG\\)$" "eog")
		   (list "\\.\\(avi\|wav\|flv\|mov\|3gp\\)$" "vlc")
		   )))

  (use-package dired-sidebar :ensure t
	:commands (dired-sidebar-toggle-sidebar)
	:config
	(setq dired-sidebar-use-term-integration t
		  dired-sidebar-theme 'nerd
		  dired-sidebar-subtree-line-prefix " ."
		  dired-sidebar-use-custom-font t)))

(use-package ibuffer :ensure t
  :bind ("C-x C-b" . ibuffer)
  :config
  (use-package ibuffer-sidebar :ensure t
	:commands (ibuffer-sidebar-toggle-sidebar))

  (use-package ibuffer-tramp :ensure t
	:config
	(defun my/ibuffer-tramp-hook () "ibuffer tram hook"
		   (ibuffer-tramp-set-filter-groups-by-tramp-connection)
		   (ibuffer-do-sort-by-alphabetic))
	(add-hook 'ibuffer-hook 'my/ibuffer-tramp-hook))

  (use-package ibuffer-projectile :ensure t
	:after projectile
	:config
	(defun my/ibuffer-projectile-hook () "My ibuffer-projectile-hook."
		   (ibuffer-projectile-set-filter-groups))
	(add-hook 'ibuffer-hook 'my/ibuffer-projectile-hook))
  )


;; Sidebar Dired+ibuffer (de emacs defaults)
(defun my/sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(global-set-key (kbd "M-s") 'my/sidebar-toggle)

;;__________________________________________________________
;; Alternative sidebar (better for projects)

(use-package projectile :ensure t
  :config
  (projectile-mode t))

;; (use-package treemacs :ensure t
;;   :commands treemacs
;;   :defer t
;;   :config
;;   (setq treemacs-collapse-dirs        (if (executable-find "python") 3 0)
;; 		treemacs-follow-after-init    t
;; 		treemacs-sorting              'alphabetic-desc)
;; 
;;   (treemacs-follow-mode t)
;;   (treemacs-filewatch-mode t)
;;   (treemacs-git-mode 'simple)
;; 
;;   (use-package treemacs-projectile :ensure t
;; 	:after treemacs projectile)
;;   )


;;__________________________________________________________
;; Ivy (probare un tiempo con helm/ivy)
(use-package ivy :ensure t
  :diminish
  :bind ("C-c C-r" . ivy-resume)
  :config

  (ivy-mode 1)

  (setq ivy-use-virtual-buffers t
		ivy-count-format "(%d/%d) "
		ivy-wrap t
		enable-recursive-minibuffers t
		ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (amx . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)))

  (use-package swiper :ensure t
	:bind ("C-s" . swiper)
	:config
	(set-face-attribute 'swiper-line-face nil
						:background "white" :foreground "black"
						:weight 'ultra-bold))

  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel :ensure t
  :diminish
  :config
  (counsel-mode 1)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c l") 'counsel-locate)

  (use-package counsel-tramp :ensure t
	:after exec-path-from-shell
	:config
	(setq tramp-default-method "ssh")
	(define-key global-map (kbd "C-c s") 'counsel-tramp))

  (use-package counsel-gtags :ensure t
	:diminish
	:config
	(defun my/counsel-gtags-hook () "My counsel-gtags mode hook"
		   (counsel-gtags-mode 1)
		   (add-to-list 'company-backends 'company-gtags)

		   (define-key counsel-gtags-mode-map (kbd "C-c d") 'counsel-gtags-find-definition)
		   (define-key counsel-gtags-mode-map (kbd "C-c r") 'counsel-gtags-find-reference)
		   (define-key counsel-gtags-mode-map (kbd "C-c s") 'counsel-gtags-find-symbol)
		   (define-key counsel-gtags-mode-map (kbd "C-c <") 'counsel-gtags-go-backward)
		   (define-key counsel-gtags-mode-map (kbd "C-c >") 'counsel-gtags-go-forward)
		   (message "Loading my gtags mode hook"))
	(add-hook 'c-mode-common-hook 'my/counsel-gtags-hook))

  (use-package counsel-projectile :ensure t
	:after projectile
	:config
	(counsel-projectile-mode t)
	)
  )

(use-package imenu-anywhere :ensure t
  :bind (("C-c i" . imenu-anywhere)))

;;__________________________________________________________
;; Historical completion
(use-package historian :ensure t
  :config
  (use-package ivy-historian :ensure t
	:after ivy
	:config (ivy-historian-mode t)))

(use-package smex :ensure t)

;;(use-package amx :ensure t
;;  :config (amx-mode t))

;;__________________________________________________________
;; Magit
(use-package magit :ensure t
  :commands magit-status
  )

;;______________________________________
;; Git commit
(use-package git-commit :ensure t
  :commands global-git-commit-mode
  :mode ("COMMIT_EDITMSG")
  :init
  (setq git-commit-summary-max-length 50
        fill-column 72)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

;;__________________________________________________________
;; Ensamblador nasm
(use-package nasm-mode :ensure t
  :mode ("\\.asm\\'" "\\.s\\'"))

;;__________________________________________________________
;; CMake
(use-package cmake-mode :ensure t
  :after company
  :mode ("/CMakeLists\\.txt\\'" "\\.cmake\\'")
  :init
  (use-package cmake-font-lock :ensure t
	:init
	(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
	)
  (add-to-list 'company-backends 'company-cmake))

;;__________________________________________________________
;; Cobol
(use-package cobol-mode :ensure t
  :mode ("\\.cobc\\'" "\\.cob\\'" "\\.cbl\\'" "\\.cpy\\'"))

;;__________________________________________________________
;; path
(use-package exec-path-from-shell :ensure t
  :config
  (exec-path-from-shell-initialize))

;;__________________________________________________________
;; Better shell (for ssh)
(use-package better-shell :ensure t
    :bind (("C-'" . better-shell-shell)
           ("C-;" . better-shell-remote-open)))

;;__________________________________________________________
;; ssh
(use-package tramp :ensure t
  :config
  (use-package tramp-term :ensure t)

  (setq tramp-default-method "ssh")
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/sshd?_config\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/known_hosts\\'" . ssh-known-hosts-mode))
  (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

;;__________________________________________________________
;; Jabber (for gmail)
(use-package jabber :ensure t
  :defer t
  :config
  (defun jabber () "My jabber loader function."
		 (interactive)
		 (jabber-connect)
		 (switch-to-buffer "*-jabber-*")
		 )

  (setq jabber-account-list
		'(
		  ("kratsbinovish@gmail.com"
		   (:network-server . "talk.google.com")
		   (:connection-type . ssl)
		   (:password . "***")
		   )
		  )
		)
  )

;;__________________________________________________________
;;; EXWM (emacs windows manager, a desktop)
(use-package exwm :ensure t
  :if (string= (getenv "XDG_CURRENT_DESKTOP") "exwm")
  :demand t
  :config
  (setq exwm-workspace-show-all-buffers t ;; share exwm buffers in all workspaces
		exwm-layout-show-all-buffers t)

  (use-package exwm-randr
    :config
	(defun my/exwm-randr-screen-change-hook () "My screen config hook"
		   ;; get number of monitors
		   (setq exwm-workspace-number
				 (string-to-number
				  (shell-command-to-string
				   "xrandr -q| grep -c \" connected\""))) ;; Number of monitors
		   
		   (message "Configure %d monitors" exwm-workspace-number)
		   (cond ((eq exwm-workspace-number 3)
				  (setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "DP1" 2 "DP2"))
				  (start-process-shell-command "xrandr" nil
											   "xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1366x768 --pos 554x1080 --rotate normal --output DP1 --mode 1680x1050 --pos 1920x304 --rotate normal --output HDMI2 --off --output HDMI1 --off --output DP2 --mode 1920x1080 --pos 0x0 --rotate normal"
											   ))
				 ((eq exwm-workspace-number 2)
				  (setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "HDMI1"))
				  (start-process-shell-command "xrandr" nil
											   "xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1366x768 --pos 0x312 --rotate normal --output DP1 --off --output HDMI2 --off --output HDMI1 --mode 1920x1080 --pos 1366x0 --rotate normal --output DP2 --off"
											   ))
				 )
		   )
    (add-hook 'exwm-randr-screen-change-hook 'my/exwm-randr-screen-change-hook)
	(exwm-randr-enable))

	(use-package exwm-systemtray
	  :config
	  (exwm-systemtray-enable))  ;; exwm system tray

	(add-hook 'exwm-manage-finish-hook
			  (lambda ()
				(when-let ((target (cdr (assoc exwm-class-name exwm-workspace-window-assignments))))
				  (exwm-workspace-move-window target))))

	(use-package desktop-environment :ensure t
	  :config
	  (desktop-environment-mode))

	(use-package symon :ensure t
	  :config
	  (symon-mode))

	(use-package exwm-config
	  :config
	  (setq exwm-workspace-number 4)
	  ;; Make class name the buffer name
	  (add-hook 'exwm-update-class-hook
				(lambda () (exwm-workspace-rename-buffer exwm-class-name)))

	  (exwm-input-set-key (kbd "s-r") #'exwm-reset) ;;Reset
	  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch) ;;Switch workspace
	  (exwm-input-set-key (kbd "s-&") #'counsel-linux-app) ;;Switch workspace

	  (dotimes (i 10) ;; 's-N': Switch to certain workspace
		(exwm-input-set-key (kbd (format "s-%d" i))
							`(lambda ()
							   (interactive)
							   (exwm-workspace-switch-create ,i))))

	  ;; Line-editing shortcuts
	  (setq exwm-input-simulation-keys
			'(([?\C-b] . [left])
			  ([?\C-f] . [right])
			  ([?\C-p] . [up])
			  ([?\C-n] . [down])
			  ([?\C-a] . [home])
			  ([?\C-e] . [end])
			  ([?\M-v] . [prior])
			  ([?\C-v] . [next])
			  ([?\C-d] . [delete])
			  ([?\C-k] . [S-end delete])))
	  ;; Enable EXWM
	  (exwm-enable)
	  (exwm-config-misc))
	)

;;__________________________________________________________
;;; org-gcal (google calendar, not configured now)
(use-package calfw :ensure t
  :defer t
  :config
  (use-package calfw-org :ensure t)
  (setq cfw:org-overwrite-default-keybinding t)

  (use-package calfw-ical :ensure t)
  (use-package calfw-gcal :ensure t)

  (defun mycalendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      ;; (cfw:org-create-source "Green")  ; orgmode source
      (cfw:ical-create-source "gcal" "https://somecalnedaraddress" "IndianRed") ; devorah calender
      )))

  (setq cfw:org-overwrite-default-keybinding t)
  )

(provide 'init)
;;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
