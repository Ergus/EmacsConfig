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
  :config
  (setq paradox-spinner-type 'progress-bar))

;;__________________________________________________________
;; Benchmark-init
(use-package benchmark-init :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;__________________________________________________________
;; To put all my lisp scripts
(defvar mylisp-dir (expand-file-name "lisp" user-emacs-directory))

(unless (file-exists-p mylisp-dir)
  (make-directory mylisp-dir)
  (message "Creating %s" mylisp-dir))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;;__________________________________________________________
;;  The Colors (I want to change this for a real theme, there are maaaaany)

(defun my/colors () "Define my color theme."
	   (defconst myblack         "#000000" "Color red")
	   (defconst myred           "#cd0000" "Color red")
	   (defconst mygreen         "#00cd00" "Color green")
	   (defconst myyellow        "#cdcd00" "Color yellow")
	   (defconst myblue          "#0000ee" "Color blue")
	   (defconst mymagenta       "#cd00cd" "Color magenta")
	   (defconst mycyan          "#00cdcd" "Color cyan")
	   (defconst mywhite         "#e5e5e5" "Color white")
	   (defconst mybrightblack   "#7f7f7f" "Color brightblack")
	   (defconst mybrightred     "#ff0000" "Color brightred")
	   (defconst mybrightgreen   "#00ff00" "Color brightgreen")
	   (defconst mybrightyellow  "#ffff00" "Color brightyellow")
	   (defconst mybrightblue    "#5c5cff" "Color brightblue")
	   (defconst mybrightmagenta "#ff00ff" "Color brightmagenta")
	   (defconst mybrightcyan    "#00ffff" "Color brightcyan")
	   (defconst mybrightwhite   "#ffffff" "Color brightwhite")

       (set-background-color myblack)
       (set-foreground-color mywhite)

       ;;(set-face-foreground 'bold "LightGoldenrod")
       ;;(set-face-foreground 'bold-italic "grey20")
       ;;(set-face-foreground 'italic "yellow3")

       (set-face-foreground 'font-lock-preprocessor-face mymagenta)     ;; Preprocessor

       (set-face-foreground 'font-lock-comment-face myblue)             ;; Comentarios
       (set-face-foreground 'font-lock-doc-face myblue)                 ;; Documentation

       (set-face-foreground 'font-lock-string-face myred)           	;; Strings
       (set-face-foreground 'font-lock-function-name-face mywhite)  	;; Funciones
       (set-face-foreground 'font-lock-variable-name-face mywhite)  	;; Variables
       (set-face-foreground 'font-lock-constant-face mymagenta)   	    ;; Constates y Clases

       (set-face-foreground 'font-lock-type-face mygreen)               ;; Tipos (int, float)
       (set-face-foreground 'font-lock-keyword-face mybrightyellow)     ;; Keywords (for, if)
       (set-face-foreground 'font-lock-builtin-face mygreen)            ;; Keywords (for, if)

       (set-face-attribute 'highlight nil :foreground myred)

       (set-face-attribute 'secondary-selection nil :background mybrightblue :foreground myblue)

       ;; search C-s, resalta lo que encuentra
       (set-face-attribute 'isearch nil :background mygreen
						   :foreground mybrightwhite :weight 'bold)            ;; Search
       (set-face-attribute 'region nil :inherit nil :background mybrightblack) ;; Seleccion C-space

       (set-face-attribute 'line-number nil :foreground mybrightblack)         ;; numero de linea
	   (set-face-attribute 'line-number-current-line nil :foreground mygreen)  ;; resalta la linea actual
       )

(my/colors)

;;__________________________________________________________
;; Config file not here to not track it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  (message "Creating %s" custom-file))

(load custom-file)

;;__________________________________________________________
;; Internal options

(global-font-lock-mode t)      ;; Use font-lock everywhere.

(savehist-mode t)              	 ;; Historial
(auto-compression-mode t)      	 ;; Uncompress on the fly:
(show-paren-mode t)            	 ;; Highlight couple parentesis
(auto-revert-mode t)             ;; Autoload files changed in disk
(global-display-line-numbers-mode) ;; line numbers on the left
(delete-selection-mode)          ;; Sobreescribe seleccion al pegar
(menu-bar-mode -1)               ;; Quitar barra superior (no la uso)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode -1))

(setq-default vc-follow-symlinks nil	            ;; Open links not open
			  transient-mark-mode t     ;; Highlight marked region
			  line-number-mode t        ;; Display line numbers
			  column-number-mode t      ;; Display column numbers
			  tab-always-indent 't      ;; make tab key do indent only
			  initial-scratch-message "Welcome Jimmy!!"
			  ring-bell-function 'ignore
			  user-full-name "Jimmy Aguilar Mena"
			  inhibit-startup-message t
			  inhibit-startup-screen t
			  tab-width 4               ;; Tabulador a 4
			  make-backup-files nil     ;; Sin copias de seguridad
			  auto-save-list-file-name  nil
			  auto-save-default         nil
			  create-lockfiles nil      ;; No lock files, goot for tramp
			  visible-bell nil          ;; Flash the screen (def)
			  scroll-step 1             ;; Scroll one by one
			  ;;scroll-preserve-screen-position 1
			  scroll-conservatively 100000
			  scroll-margin 0
			  fill-column 80            ;; default is 70
			  tooltip-mode t            ;; Tool tip in the echo
			  confirm-kill-processes    nil ;; no ask for confirm kill processes on exit
			  font-lock-maximum-decoration t
			  )

;;__________________________________________________________
;;Packages options
;;__________________________________________________________

;;__________________________________________________________
;; Two options for diffs
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
;; Mocp and multi-term music player
(use-package multi-term :ensure t
  :config
  (autoload 'mocp "mocp" "mocp in emacs" t))

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
;; winum (windows number) for spaceline
(use-package winum :ensure t
  :config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode))

;;__________________________________________________________
;; Status bar (mode line in emacs) two options to chose

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

  (set-face-attribute 'mode-line nil :background myblue :foreground mywhite))

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

(set-cursor-color "white")        ;; Set cursor and mouse colours

;;__________________________________________________________
;; Multiple Cursors
(global-unset-key (kbd "C-c <down-mouse-1>"))
(use-package multiple-cursors  :ensure t ;; Multiple cursors package
  :bind (("C-c m" . mc/edit-lines)
		 ("C-c <down>" . mc/mark-next-like-this)
		 ("C-c <up>" . mc/mark-previous-like-this)
         ("C-c <mouse-1>" . mc/add-cursor-on-click)))

;;__________________________________________________________
;; My program's mode hooks

(use-package whitespace-mode
  :hook prog-mode
  :init
  (setq whitespace-style '(face trailing)))

(defun my/prog-mode-hook () "Some hooks only for prog mode."
	   (which-function-mode t)     		  ;; Shows the function in spaceline
	   (electric-pair-mode t)      		  ;; Autoannadir parentesis
	   (electric-indent-mode t)    		  ;; Corrige indentacion con tab o enter (now default)
	   )

(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;;__________________________________________________________
;; 80 Column rules
(use-package fill-column-indicator :ensure t
  :config
  (setq	fci-rule-color "#7f7f7f7f7f7f"
		fci-rule-character ?\u2502)
  (add-hook 'prog-mode-hook 'fci-mode))

;;__________________________________________________________
;; Undo tree
(use-package undo-tree :ensure t
  :diminish
  :init (global-undo-tree-mode))

;;__________________________________________________________
;; Mark column 80 when crossed
(use-package column-enforce-mode :ensure t
  :diminish
  :hook prog-mode
  :config
  (column-enforce-mode t)
  (setq column-enforce-comments nil)
  (set-face-attribute 'column-enforce-face nil :background mybrightblack))

;;__________________________________________________________
;; Lineas de Indentado
(use-package highlight-indent-guides :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face mybrightblack))

;;__________________________________________________________
;; Resalta parentesis entorno al cursor
(use-package highlight-parentheses :ensure t
  :diminish
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)

  (set-face-attribute 'hl-paren-face nil :weight 'bold)

  (setq hl-paren-colors
		(quote
		 ("#00ff00" "#00ffff" "#ff0000" "#cd0000"))))

;;__________________________________________________________
;; Resalta scopes entorno al cursor
(use-package highlight-blocks :ensure t
  :config
  ;;(define-key function-key-map "\e[1;5R" [C-f3])
  (global-set-key (kbd "C-c b") 'highlight-blocks-now)

  (set-face-attribute 'highlight-blocks-depth-2-face nil :background "#262626") ;; gray15
  (set-face-attribute 'highlight-blocks-depth-3-face nil :background "#333333") ;; gray20
  (set-face-attribute 'highlight-blocks-depth-4-face nil :background "#404040") ;; gray25
  (set-face-attribute 'highlight-blocks-depth-5-face nil :background "#4d4d4d")
  (set-face-attribute 'highlight-blocks-depth-6-face nil :background "#595959")
  (set-face-attribute 'highlight-blocks-depth-7-face nil :background "#666666")
  (set-face-attribute 'highlight-blocks-depth-8-face nil :background "#737373")
  (set-face-attribute 'highlight-blocks-depth-9-face nil :background "#7f7f7f"))

(use-package highlight-escape-sequences :ensure t
  :config
  (add-hook 'prog-mode-hook 'hes-mode)
  (set-face-attribute 'hes-escape-backslash-face nil :foreground mymagenta)
  (set-face-attribute 'hes-escape-sequence-face nil :foreground mymagenta))

(use-package highlight-numbers :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (set-face-attribute 'highlight-numbers-number nil :foreground myred))

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
				("C-c ." . flyspell-popup-correct)
				("C-M-i" .  nil)
				))

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
;; C common mode (for all c-like languajes)

(defun my/c-mode-common-hook () "My hook for C and C++."
	   ;; ;; Spaces around operators
	   ;; (use-package electric-operator :ensure t
	   ;; 	 :config
	   ;; 	 (defun my/electric-operator-mode () "electric-operator-mode"
	   ;; 			(electric-operator-mode t))
	   ;; 	 (add-hook 'prog-mode-hook 'my/electric-operator-mode))

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
	   (c-set-offset 'access-label '-)
	   (c-set-offset 'inline-open 0)
       (c-set-offset 'inclass 'my/c++-lineup-inclass)
	   (message "Loaded my c++-mode"))

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
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
	:config
	(flyspell-mode t))

;;__________________________________________________________
;; Restructured text
(use-package rst-mode
  :mode "\\.rst\\'"
  :config
  (use-package sphinx-mode :ensure t
    :hook rst-mode)
  (flyspell-mode t))

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
;; Ocaml Mode
(use-package caml-mode :ensure caml
  :mode "\\.ml\\'")

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
(setq split-width-threshold 90)        ;; Original value 240 ancho minimo limite para split vertical

;; Move split keybindings
(use-package windmove
  :config
  (windmove-default-keybindings)        ;; Move between panes S-arrow
  (setq-default windmove-wrap-around t) ;; Cyclic bound mode
  )

(when (fboundp 'winner-mode) (winner-mode 1))   ;; recuperar Split configuration con C-c left/right

;;__________________________________________________________
;; Confirmation for to exit emacs
(defalias 'yes-or-no-p 'y-or-n-p)     ;; Reemplazar "yes" por "y" en el prompt
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
  :init (global-company-mode)
  :config
  (define-key company-mode-map (kbd "M-RET") 'company-complete)
  (define-key company-active-map (kbd "M-RET") 'company-other-backend)


  (set-face-attribute 'company-tooltip nil        ;; dialog face
					  :background mybrightblack :foreground mywhite)
  (set-face-attribute 'company-tooltip-common nil ;; common part face
					  :inherit 'company-tooltip :foreground mygreen)
  (set-face-attribute 'company-tooltip-selection nil ;; selection face
					  :background myblue :weight 'ultra-bold)
  (set-face-attribute 'company-scrollbar-bg nil   ;; scroll bar face bg
					  :background mybrightblack)
  (set-face-attribute 'company-scrollbar-fg nil   ;; scroll bar face fg
					  :background myblue)

  (setq company-idle-delay 1.0   ;; no delay for autocomplete
		company-minimum-prefix-length 2
		company-tooltip-limit 20
		company-show-numbers t)

  (use-package company-quickhelp :ensure t
	:after company
	:config
	(company-quickhelp-mode)))

;;__________________________________________________________
;; Irony config (C completions)
(use-package irony :ensure t
  :diminish
  :defer t
  :preface
  (defun my/irony-mode-hook () "My irony mode hook."
		 (unless (and buffer-file-name (file-remote-p buffer-file-name))
		   (irony-cdb-autosetup-compile-options)

		   (use-package company-irony :ensure t
			 :after company
			 :config
			 (use-package company-irony-c-headers :ensure t)

			 (add-to-list 'company-backends
						  '(company-irony-c-headers company-irony))

			 (company-irony-setup-begin-commands)
			 (message "Loaded my company-irony"))

		   (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
		   (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)

		   (use-package flycheck-irony :ensure t
			 :after flycheck
			 :config
			 (add-hook 'irony-mode-hook #'flycheck-irony-setup)
			 (message "Loaded my flycheck-irony"))

		   (use-package irony-eldoc :ensure t
			 :config
			 (add-hook 'irony-mode-hook 'irony-eldoc))

		   (message "Loaded my-Irony-Mode-hook")))
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook #'my/irony-mode-hook))

;;__________________________________________________________
;; Chequeo de syntaxis
(use-package flycheck :ensure t
  :init (global-flycheck-mode)
  :config

  (setq-default flycheck-display-errors-delay 1)

  (use-package flycheck-popup-tip :ensure t
	:after flycheck
	:config
	(add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode))

;;  (use-package flycheck-status-emoji :ensure t
;;	:after flycheck
;;	:config (add-hook 'flycheck-mode-hook #'flycheck-status-emoji-mode))

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
  (setq langtool-language-tool-jar "~/gits/languagetool/languagetool-standalone/target/LanguageTool-4.3-SNAPSHOT/LanguageTool-4.3-SNAPSHOT/languagetool-commandline.jar"))

;;__________________________________________________________
;; EMMS mode.
(use-package emms :ensure t
  :defer t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq-default emms-source-file-default-directory "~/almacen/Musica/"))

;;__________________________________________________________
;; Email mode for mutt
;;__________________________________________________________
(use-package abbrev :diminish)

(use-package mail-mode
  :mode ("/neomut")
  :init
  (defun my/mail-mode-hook () "My mail mode hook"
		 (turn-on-auto-fill)
		 (mail-abbrevs-setup)
		 (message "Loaded Mail-mode"))
  (add-hook 'mail-mode-hook 'my/mail-mode-hook))

;; Asocia buffers que empiecen con messaje mode
(use-package message-mode
  :mode ("neomutt-Ergus-" "draft")
  :init
  (defun my/message-mode-hook () "My mail mode hook"
		 (turn-on-auto-fill)
		 (mail-abbrevs-setup)
		 (flyspell-mode t)
		 (message "Loaded Message-mode"))
  (add-hook 'message-mode-hook 'my/message-mode-hook))

;; Autocompleta direcciones
(use-package notmuch :ensure t
  :init
  (setenv "NOTMUCH_CONFIG" "/home/ergo/almacen/mail/notmuch-config")
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  (setq notmuch-init-file "~/almacen/mail/notmuch-config")
  (use-package notmuch-address :ensure notmuch
	:config
	(setq notmuch-address-command "~/gits/notmuch-addrlookup-c/notmuch-addrlookup")
	(notmuch-address-message-insinuate))

  (use-package notmuch-company :ensure notmuch
	:config
	(add-to-list 'company-backends 'notmuch-company)))

;;__________________________________________________________
;; Latex mode

(use-package latex :ensure auctex
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
		TeX-source-correlate-start-server t)

  (setq-default TeX-master nil)

  (setq LaTeX-fill-break-at-separators (quote (\\\( \\\[ \\\])))
  (flyspell-mode t)
  (flyspell-buffer)
  (turn-on-auto-fill)
  (visual-line-mode)

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
		  reftex-use-multiple-selection-buffers t)

	(use-package company-reftex :ensure t
	  :after company
	  :config
	  (add-to-list 'company-backends '(company-reftex-labels
									   company-reftex-citations))))

  (use-package company-math :ensure t
	:after company
	:config
	(add-to-list 'company-backends '(company-math-symbols-latex
									 company-latex-commands)))

  (use-package company-auctex :ensure t
	:after company
	:config
	(company-auctex-init))

  (add-to-list 'TeX-command-list
			   '("Makeglossaries" "makeglossaries %s" TeX-run-command nil
				 (latex-mode)
				 :help "Run makeglossaries script, which will choose xindy or makeindex") t)

  (add-to-list 'TeX-output-view-style '("^pdf$" "." "evince %o %(outpage)")))

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
	:commands elpy-enable
	:init
	(with-eval-after-load 'python (elpy-enable))
	(add-hook 'elpy-mode-hook 'turn-on-auto-fill)
	:config
	(setq elpy-rpc-python-command "python3"
		  elpy-rpc-backend "jedi"
		  python-check-command "pyflakes"
		  python-shell-interpreter "python3"
		  python-shell-interpreter-args "-i --simple-prompt"))

  (use-package flycheck-pycheckers :ensure t
	:after flycheck
	:config
	(add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

  (use-package ipython-shell-send :ensure t))

;;__________________________________________________________
;; Dired-mode settings (file manager)
(use-package dired
  :config
  (setq dired-recursive-copies 'top  ;; Always ask recursive copy
		dired-recursive-deletes 'top ;; Always ask recursive delete
		dired-dwim-target t)         ;; Copy in split mode with p
  (put 'dired-find-alternate-file 'disabled nil)

;  ;; Open in the same buffer in dired mode
;  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
;  (define-key dired-mode-map (kbd "^") (lambda () (interactive)
;										 (find-alternate-file ".."))); was dired-up-directory
  (use-package dired-x
	:config
	(setq dired-guess-shell-alist-user
		  (list
		   (list "\\.\\(ps\|ps.gz\|eps\|eps.gz\|pdf\|PDF\\)$" "evince")
		   (list "\\.\\(rgb\|tiff\|tif\|xbm\|gif\|pgm\|ppm\|bmp\|tga\\)$" "eog ")
		   (list "\\.\\(ppm\|gif\|png\|jpg\|JPG\\)$" "eog")
		   (list "\\.\\(avi\|wav\|flv\|mov\|3gp\\)$" "vlc"))))

  (use-package dired-sidebar :ensure t
	:commands (dired-sidebar-toggle-sidebar)
	:config
	(setq ;;dired-sidebar-use-term-integration t
	      ;;dired-sidebar-theme 'nerd
	      dired-sidebar-subtree-line-prefix ".")))

;;__________________________________________________________
;; ibuffer
(use-package ibuffer :ensure t
  :bind ("C-x C-b" . ibuffer)
  :init
  (defalias 'list-buffers 'ibuffer) ; make ibuffer default
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
	(add-hook 'ibuffer-hook 'my/ibuffer-projectile-hook)))


;; Sidebar Dired+ibuffer (de emacs defaults)
(defun my/sidebar-toggle () "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
	   (interactive)
	   (ibuffer-sidebar-toggle-sidebar)
	   (dired-sidebar-toggle-sidebar)
	   )

(global-set-key (kbd "C-c s") 'my/sidebar-toggle)

;;__________________________________________________________
;; neotree
(use-package neotree :ensure t
  :bind ("C-c e" . neotree-toggle))

;;__________________________________________________________
;; Ivy (probare un tiempo con helm/ivy)
(use-package headlong :ensure t
  :defer t)

(use-package ivy :ensure t
  :diminish
  :bind ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode t)

  (set-face-attribute 'minibuffer-prompt nil :foreground mycyan) ;; prompt minibuffer
  (set-face-attribute 'ivy-current-match nil :inherit nil
					  :background mybrightblack :weight 'ultra-bold)

  (setq ivy-use-virtual-buffers t    ;;
		ivy-count-format "(%d/%d) "
		ivy-display-style 'fancy
		ivy-height 5
		ivy-wrap t                   ;; cycle in minibuffer
		enable-recursive-minibuffers t)

  (use-package swiper :ensure t
	:bind (("C-s" . swiper)
		   ("C-r" . swiper))
	:config
	(set-face-attribute 'swiper-line-face nil :inherit nil
						:background mybrightblack :weight 'bold)
	)

  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

(use-package counsel :ensure t
  :diminish
  :config
  (counsel-mode t)
  (define-key counsel-mode-map (kbd "C-c k") 'counsel-ag)
  (define-key counsel-mode-map (kbd "C-c g") 'counsel-git)
  (define-key counsel-mode-map (kbd "C-c r") 'counsel-git-grep)
  (define-key counsel-mode-map (kbd "C-c l") 'counsel-locate)

  (use-package counsel-tramp :ensure t
	:after exec-path-from-shell
	:config
	(setq tramp-default-method "ssh"))

  (use-package counsel-gtags :ensure t
	:diminish
	:config
	(defun my/counsel-gtags-hook () "My counsel-gtags mode hook"
		   (counsel-gtags-mode 1)
		   (add-to-list 'company-backends 'company-gtags)

		   (define-key counsel-gtags-mode-map (kbd "C-c g d") 'counsel-gtags-find-definition)
		   (define-key counsel-gtags-mode-map (kbd "C-c g r") 'counsel-gtags-find-reference)
		   (define-key counsel-gtags-mode-map (kbd "C-c g f") 'counsel-gtags-find-symbol)
		   (define-key counsel-gtags-mode-map (kbd "C-c g <") 'counsel-gtags-go-backward)
		   (define-key counsel-gtags-mode-map (kbd "C-c g >") 'counsel-gtags-go-forward)
		   (message "Loading my counsel gtags mode hook"))
	(add-hook 'c-mode-common-hook 'my/counsel-gtags-hook))

  (use-package counsel-projectile :ensure t
	:after projectile
	:config
	(counsel-projectile-mode t))

  (use-package counsel-notmuch :ensure t))

(use-package imenu-anywhere :ensure t
  :bind (("C-c i" . imenu-anywhere)))


(use-package dumb-jump :ensure t
  :bind (("C-c j o" . dumb-jump-go-other-window)
         ("C-c j j" . dumb-jump-go)
         ("C-c j i" . dumb-jump-go-prompt)
         ("C-c j x" . dumb-jump-go-prefer-external)
         ("C-c j z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  ;; (setq dumb-jump-selector 'helm)
  )

;;__________________________________________________________
;; Historical completion
(use-package historian :ensure t
  :config
  (use-package ivy-historian :ensure t
	:after ivy
	:config (ivy-historian-mode t)))

;;__________________________________________________________
;; Complete history
(use-package amx :ensure t)

;;__________________________________________________________
;; Magit
(use-package magit :ensure t
  :commands magit-status
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;;______________________________________
;; Git commit
(use-package git-commit :ensure t
  :init
  (global-git-commit-mode t)
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
  :config
  (use-package cmake-font-lock :ensure t
	:config
	(add-hook 'cmake-mode-hook 'cmake-font-lock-activate))
  (add-to-list 'company-backends 'company-cmake))

;;__________________________________________________________
;; Cobol
(use-package cobol-mode :ensure t
  :mode ("\\.cobc\\'" "\\.cob\\'" "\\.cbl\\'" "\\.cpy\\'"))

;;__________________________________________________________
;; path
(defun shell-command-on-buffer (command)
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command t))

;;__________________________________________________________
;; Better shell (for ssh)
(use-package better-shell :ensure t
  :bind ("C-c t" . better-shell-shell))

;;__________________________________________________________
;; ssh
(use-package tramp :ensure t
  :config
  (use-package tramp-term :ensure t)

  (setq tramp-verbose 9
		tramp-default-method "ssh"
		tramp-change-syntax 'simplified
		tramp-use-ssh-controlmaster-options nil
		tramp-persistency-file-name "~/.emacs.d/tramp"))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

;;__________________________________________________________
;;; Google calendar (view only)

(use-package org :ensure t
  :mode ("\\.org$" . org-mode)
  :config
  (use-package org-bullets :ensure t
	:hook (org-mode . org-bullets-mode))
  )


;;__________________________________________________________
;;; Google calendar (view only)

(use-package calfw :ensure t
  :commands (my/calendar)
  :config
  (use-package calfw-org :ensure t)
  (use-package calfw-ical :ensure t)
  (use-package calfw-gcal :ensure t)

  (setq cfw:org-overwrite-default-keybinding t)

  (defun my/calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:open-ical-calendar my/gmailcal "Red")
      )))
  (setq cfw:org-overwrite-default-keybinding t))



;; Some editing commands

;;(global-set-key (read-kbd-macro "<M-DEL>") )

;;(global-set-key (kbd "M-t") (backward-kill-word))

;;__________________________________________________________
;; Move current line up and down Shift+arrow

(use-package move-text :ensure t)

(defvar my/keys-minor-mode-map
  (let ((map (make-sparse-keymap)))

	(if (display-graphic-p)
		  (define-key input-decode-map (kbd "C-i") (kbd "<up>")))

	;;(define-key map "\e[A"  'previous-line)   		  ;; C-i aleas de <up> in xterm
	(define-key map (kbd "C-k") 'next-line)       		;; replaces kill-line
	(define-key map (kbd "C-l") 'forward-char)    		;; replaces recenter-top-bottom
	(define-key map (kbd "C-j") 'backward-char)   		;; replaces electric-newline-and-maybe-indent

	(define-key map (kbd "M-i")  'backward-paragraph)    ;; M-i aleas de C-<up> in xterm
	(define-key map (kbd "M-k") 'forward-paragraph)  	;; replaces kill-sentence
	(define-key map (kbd "M-l") 'right-word)         	;; replaces downcase-word
	(define-key map (kbd "M-j") 'left-word)          	;; replaces indent-new-comment-line

	(define-key map (kbd "C-n") 'kill-line)          	;; Before C-k
	(define-key map (kbd "M-n") 'kill-sentence)       	;; Before M-k

	(define-key map (kbd "C-M-i") 'move-text-up)
	(define-key map (kbd "C-M-k") 'move-text-down)

	(require 'windmove)
	(define-key map (kbd "C-x i") 'windmove-up)    ;; replaces insert-file
	(define-key map (kbd "C-x k") 'windmove-down)  ;; replaces kill-buffer
	(define-key map (kbd "C-x l") 'windmove-right) ;; replaces count-lines-page
	(define-key map (kbd "C-x j") 'windmove-left)  ;; replaces nothing

    map)
  "My-keys-minor-mode key map.")

;;__________________________________________________________
;; Move current line up and down Shift+arrow
(use-package move-text :ensure t
  :bind(("C-M-<up>" . move-text-up)
        ("C-M-<down>" . move-text-down))
  :init
  (global-set-key (kbd "C-M-<left>") (lambda () (interactive) (transpose-words -1)))
  (global-set-key (kbd "C-M-<right>") (lambda () (interactive) (transpose-words 1)))
  (global-set-key (kbd "C-t") (lambda () (interactive) (transpose-chars -1)))
  (global-set-key (kbd "M-t") (lambda () (interactive) (transpose-chars 1))))

(define-minor-mode my/keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value nil
  :lighter " my/keys"
  :global t)

;;(my/keys-minor-mode t)

(provide 'init)
;;; init.el ends here
