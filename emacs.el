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
(require 'package)		;; You might already have this line
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
;;	The Colors (I want to change this for a real theme, there are maaaaany)

(defun my/colors () "Define my color theme."
	   (defconst myblack		 "#000000" "Color red")
	   (defconst myred			 "#cd0000" "Color red")
	   (defconst mygreen		 "#00cd00" "Color green")
	   (defconst myyellow		 "#cdcd00" "Color yellow")
	   (defconst myblue			 "#0000ee" "Color blue")
	   (defconst mymagenta		 "#cd00cd" "Color magenta")
	   (defconst mycyan			 "#00cdcd" "Color cyan")
	   (defconst mywhite		 "#e5e5e5" "Color white")
	   (defconst mybrightblack	 "#7f7f7f" "Color brightblack") ;; 
	   (defconst mybrightred	 "#ff0000" "Color brightred")
	   (defconst mybrightgreen	 "#00ff00" "Color brightgreen")
	   (defconst mybrightyellow	 "#ffff00" "Color brightyellow")
	   (defconst mybrightblue	 "#5c5cff" "Color brightblue")
	   (defconst mybrightmagenta "#ff00ff" "Color brightmagenta")
	   (defconst mybrightcyan	 "#00ffff" "Color brightcyan")
	   (defconst mybrightwhite	 "#ffffff" "Color brightwhite")

	   (set-background-color myblack)
	   (set-foreground-color mywhite)

	   ;;(set-face-foreground 'bold "LightGoldenrod")
	   ;;(set-face-foreground 'bold-italic "grey20")
	   ;;(set-face-foreground 'italic "yellow3")

	   (set-face-foreground 'font-lock-preprocessor-face mymagenta)		;; Preprocessor

	   (set-face-foreground 'font-lock-comment-face myblue)				;; Comentarios
	   (set-face-foreground 'font-lock-doc-face myblue)					;; Documentation

	   (set-face-foreground 'font-lock-string-face myred)				;; Strings
	   (set-face-foreground 'font-lock-function-name-face mywhite)		;; Funciones
	   (set-face-foreground 'font-lock-variable-name-face mywhite)		;; Variables
	   (set-face-foreground 'font-lock-constant-face mymagenta)			;; Constates y Clases

	   (set-face-foreground 'font-lock-type-face mygreen)				;; Tipos (int, float)
	   (set-face-foreground 'font-lock-keyword-face myyellow)	  ;; Keywords (for, if)
	   (set-face-foreground 'font-lock-builtin-face mygreen)			;; Keywords (for, if)

	   (set-face-attribute 'highlight nil :background mybrightblack :foreground nil)

	   (set-face-attribute 'secondary-selection nil :background mybrightblue
						   :foreground myblue :weight 'bold)

	   ;; search C-s, resalta lo que encuentra
	   (set-face-attribute 'isearch nil :background myblue
						   :foreground mywhite :weight 'ultrabold)			   ;; Search

	   (set-face-attribute 'region nil :inherit nil :background mybrightblack) ;; Seleccion C-space

	   (set-face-attribute 'line-number nil :foreground mybrightblack)		   ;; numero de linea
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

(setq show-paren-delay 0)
(show-paren-mode t)				 ;; Highlight couple parentesis

(global-font-lock-mode t)	   ;; Use font-lock everywhere.
(savehist-mode t)				 ;; Historial
(auto-compression-mode t)		 ;; Uncompress on the fly:
(auto-revert-mode t)			 ;; Autoload files changed in disk
(global-display-line-numbers-mode t) ;; line numbers on the left
(delete-selection-mode t)		 ;; Sobreescribe seleccion al pegar
(electric-indent-mode -1)
(menu-bar-mode -1)				 ;; Quitar barra superior (no la uso)
(tool-bar-mode -1)				 ;; Quitar barra superior (no la uso)
(size-indication-mode t)
(scroll-bar-mode -1)
(transient-mark-mode t)
(tooltip-mode -1)			;; Tool tip in the echo

(setq-default vc-follow-symlinks nil	;; Open links not open
			  line-number-mode t		;; Display line numbers
			  column-number-mode t		;; Display column numbers
			  tab-always-indent 't		;; make tab key do indent only
			  initial-scratch-message ";; Welcome Jimmy!!"
			  ring-bell-function 'ignore
			  user-full-name "Jimmy Aguilar Mena"
			  inhibit-startup-message t
			  inhibit-startup-screen t
			  tab-width 4				;; Tabulador a 4
			  make-backup-files nil		;; Sin copias de seguridad
			  auto-save-list-file-name	nil
			  auto-save-default			nil
			  create-lockfiles nil		;; No lock files, good for tramp
			  visible-bell nil			;; Flash the screen (def)
			  ;;scroll-preserve-screen-position nil ;; Cursor keeps screen position (default nil)
			  ;; scroll-step 1			;; Scroll one by one (better conservatively)
			  scroll-conservatively 100000
			  ;; scroll-margin 0			   ;; lines at top or button to scroll
			  fill-column 80				   ;; default is 70
			  confirm-kill-processes	nil	   ;; no ask for confirm kill processes on exi
			  font-lock-maximum-decoration t
			  display-line-numbers-widen t	   ;; keep line numbers inside a narrow buffers
			  display-line-numbers-width 4	   ;; Minimum line number width
			  ;; split-width-threshold 160	   ;; Original value 240 ancho minimo limite para split vertical
			  visible-cursor nil
			  ;; split-width-threshold 180
			  ;; kill-whole-line t
			  ;; load-prefer-newer t
			  read-key-delay 0.005
			  mouse-scroll-delay 0
			  )



(put 'narrow-to-region 'disabled nil)		   ;; Enable narrow commands
;;__________________________________________________________
;;Packages options
;;__________________________________________________________

;;__________________________________________________________
;; ssh
(use-package tramp :ensure t
  :defer t
  :config
  (setq compilation-scroll-output 'first-error)
  (use-package tramp-term :ensure t)

  (setq tramp-default-method "ssh"
		;;tramp-change-syntax 'simplified
		tramp-use-ssh-controlmaster-options nil
		tramp-completion-reread-directory-timeout t
		tramp-persistency-file-name "~/.emacs.d/tramp"))

(use-package ssh-config-mode :ensure t
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
		 ("/sshd?_config\\'" . ssh-config-mode)
		 ("/known_hosts\\'" . ssh-known-hosts-mode)
		 ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))


;;__________________________________________________________
;; cua rectangles

;; (use-package cua-base
;;   :config
;;   (cua-selection-mode t))			 ;; Better rectangle selection

(use-package gdb
  :commands gdb
  :init
  (setq-default gdb-many-windows t)
  )

;;__________________________________________________________
;; Two options for diffs
(use-package ediff
  :commands (ediff
			 ediff-files
			 ediff-files3
			 ediff-buffers
			 ediff-buffers3)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
		ediff-split-window-function 'split-window-horizontally)
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
  :commands (multi-term-dedicated-open
			 multi-term))

;;__________________________________________________________
;; Keys
(use-package bind-key :ensure t)

;;__________________________________________________________
;; which-key
(use-package which-key :ensure t
  :diminish
  :config
  (setq which-key-separator ": "
		which-key-idle-delay 0.4)
  (which-key-mode t)
  (which-key-add-key-based-replacements
	"C-c s" "sidebars"
	"C-x r" "rectangle-register"
	"C-x n" "narrow"
	"C-x a" "abrev"))

;;__________________________________________________________
;; Status bar (mode line in emacs) two options to chose

;; (use-package spaceline :ensure t
;; 	 :demand t
;; 	 ;;:init
;; 	 ;;(require 'spaceline-config)

;; 	 :config
;; 	 (if (display-graphic-p)
;; 	  (setq powerline-default-separator 'arrow-fade)
;; 	(setq powerline-default-separator 'utf-8))


;; 	 (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
;; 	 ;;(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;; 	 (spaceline-emacs-theme)

;; 	 (set-face-attribute 'mode-line nil :background myblue :foreground mywhite)
;; )

(use-package smart-mode-line :ensure t
  :config
  (use-package smart-mode-line-powerline-theme :ensure t
	:config
	(setq sml/theme 'powerline)
	)
  (setq sml/no-confirm-load-theme t
		sml/name-width 40)
  (sml/setup))

;; (setq-default mode-line-format
;;           (list
;;            "%m: %b: (l:%2l,c:%2C) %I(%2p)"
;;            ))



;;__________________________________________________________
;; Clipboard copy and paste with: M-w & C-c v

(defun my/xclipboard () "Define my clipboard functions with xsel."

	   (defun my/xcopy (beg end &optional region) "Copies selection to x-clipboard and ."
			  (interactive (list (mark) (point)
								 (prefix-numeric-value current-prefix-arg)))

			  (when (region-active-p)
				(shell-command-on-region (region-beginning) (region-end) "xsel -i -b"))

			  (copy-region-as-kill beg end region)

			  (if (called-interactively-p 'interactive)
				  (indicate-copied-region)))

	   (defun my/xyank (&optional arg) "Pastes from x-clipboard."
			  (interactive "*P")

			  (setq yank-window-start (window-start))
			  (setq this-command t)
			  (push-mark)
			  (insert-for-yank (shell-command-to-string "xsel -o -b"))
			  (if (consp arg) ;; change mark and point if universal arg
				  (goto-char (prog1 (mark t)
							   (set-marker (mark-marker) (point) (current-buffer)))))
			  (if (eq this-command t)
				  (setq this-command 'my/xyank))
			  nil)

	   (defun my/xkill (beg end &optional region) "Copies selection to x-clipboard and ."
			  (interactive (list (mark) (point)
								 (prefix-numeric-value current-prefix-arg)))

			  (when (use-region-p)
				(shell-command-on-region (region-beginning) (region-end) "xsel -i -b"))

			  (kill-region beg end region))

	   (global-set-key (kbd "M-w") 'my/xcopy)
	   (global-set-key (kbd "C-S-v") 'my/xpaste)
	   (global-set-key (kbd "C-w") 'my/xkill)

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
	   (global-set-key [mouse-2] 'aberrant-paste)

	   (use-package whole-line-or-region :ensure t)

	   (defun my/whole-line-or-region-xcopy (prefix)
		 "Copy region or PREFIX whole lines."
		 (interactive "p")
		 (whole-line-or-region-call-with-region 'my/xcopy prefix t))

	   (defun my/whole-line-or-region-xkill (prefix)
		 "Copy region or PREFIX whole lines."
		 (interactive "p")
		 (whole-line-or-region-call-with-region 'my/xkill prefix t))

	   (global-set-key (kbd "M-w") 'my/whole-line-or-region-xcopy)
	   (global-set-key (kbd "C-w") 'my/whole-line-or-region-xkill)
	   (global-set-key (kbd "C-y") 'whole-line-or-region-yank)
	   )

(unless (display-graphic-p)
  (if (executable-find "xsel")
	  (my/xclipboard)
	(message "No xsel in your path, install it in your system!!!"))
  ;;(define-key function-key-map "\eC-BackSpace"	 [C-backspace])
  ;;(define-key function-key-map "\eC-S-BackSpace" [C-S-backspace])
  )

;;__________________________________________________________
;;	Seleccionar con el mouse
(use-package mouse
  :config
  (xterm-mouse-mode t)			  ;; mover el cursor al click
  (defun track-mouse (e))
  (setq-default mouse-sel-mode t) ;; Mouse selection
  (set-mouse-color "white")		  ;; Flechita del mouse en blanco
  (mouse-wheel-mode t)			  ;; scrolling con el mouse
  )

(global-set-key [drag-mouse-2] 'mouse-yank-at-click)

(set-cursor-color "white")		  ;; Set cursor and mouse colours

;;__________________________________________________________
;; Multiple Cursors
(global-unset-key (kbd "C-c <down-mouse-1>"))
(use-package multiple-cursors  :ensure t ;; Multiple cursors package
  :bind (("C-c m m" . mc/edit-lines)
		 ("C-c m r" . mc/mark-all-in-region)
		 ("C-c m i" . mc/mark-more-like-this-extended)
		 ("C-c m a" . mc/mark-all-like-this)
		 ("C-c m w" . mc/mark-all-words-like-this)
		 ("C-c m n" . mc/mark-next-like-this)
		 ("C-c m p" . mc/mark-previous-like-this)
		 ("C-c m <mouse-1>" . mc/add-cursor-on-click))
  :init
  (which-key-add-key-based-replacements "C-c m" "multiple-cursors")
  )

;;__________________________________________________________
;; My program's mode hooks

(use-package whitespace-mode
  :hook prog-mode
  :init
  (setq whitespace-style '(face trailing)))

(use-package clean-aindent-mode :ensure t
  :hook prog-mode
  :bind ("RET" . newline-and-indent)
  :config
  (clean-aindent-mode t)
  (setq clean-aindent-is-simple-indent t))

;; (defun my/prog-mode-hook () "Some hooks only for prog mode."
;; 	   ;;(electric-pair-mode t)			  ;; Autoannadir parentesis
;; 	   (which-function-mode t)			  ;; Shows the function in spaceline
;; 	   )

;; (add-hook 'prog-mode-hook 'my/prog-mode-hook)

;;__________________________________________________________
;; 80 Column rules
(use-package fill-column-indicator :ensure t
  :hook (prog-mode	. fci-mode)
  :config
  (setq fci-rule-color "#7f7f7f7f7f7f"
		fci-rule-character ?\u2502))

;;__________________________________________________________
;; Undo tree
(use-package undo-tree :ensure t
  :diminish
  :init (global-undo-tree-mode))

;;__________________________________________________________
;; Mark column 80 when crossed
;; (use-package column-enforce-mode :ensure t
;;	 :diminish
;;	 :hook prog-mode
;;	 :config
;;	 (column-enforce-mode t)
;;	 (setq column-enforce-comments nil)
;;	 (set-face-attribute 'column-enforce-face nil :background mybrightblack))

;;__________________________________________________________
;; Lineas de Indent
(use-package highlight-indent-guides :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
		highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face mybrightblack))

;;__________________________________________________________
;; Resalta parentesis entorno al cursor
;; (use-package highlight-parentheses :ensure t
;;	 :diminish
;;	 :hook (prog-mode . highlight-parentheses-mode)
;;	 :config
;;	 (set-face-attribute 'hl-paren-face nil :weight 'bold)
;;	 (setq hl-paren-delay 0.05
;;		hl-paren-colors	'("#00ff00" "#00ffff" "#ff0000" "#cd0000")))

;;__________________________________________________________
;; Resalta scopes entorno al cursor
(use-package highlight-blocks :ensure t
  :commands (highlight-blocks-now highlight-blocks-mode)
  :bind (("C-c b n" . highlight-blocks-now)
		 ("C-c b m" . highlight-blocks-mode))
  :init
  (which-key-add-key-based-replacements "C-c b" "highlight-blocks")
  :config
  (set-face-attribute 'highlight-blocks-depth-2-face nil :background "#262626") ;; gray15
  (set-face-attribute 'highlight-blocks-depth-3-face nil :background "#333333") ;; gray20
  (set-face-attribute 'highlight-blocks-depth-4-face nil :background "#404040") ;; gray25
  (set-face-attribute 'highlight-blocks-depth-5-face nil :background "#4d4d4d")
  (set-face-attribute 'highlight-blocks-depth-6-face nil :background "#595959")
  (set-face-attribute 'highlight-blocks-depth-7-face nil :background "#666666")
  (set-face-attribute 'highlight-blocks-depth-8-face nil :background "#737373")
  (set-face-attribute 'highlight-blocks-depth-9-face nil :background "#7f7f7f"))

(use-package highlight-escape-sequences :ensure t
  :hook (prog-mode . hes-mode)
  :config
  (set-face-attribute 'hes-escape-backslash-face nil :foreground mymagenta)
  (set-face-attribute 'hes-escape-sequence-face nil :foreground mymagenta))

(use-package highlight-numbers :ensure t
  :hook (prog-mode . highlight-numbers-mode)
  :config
  (set-face-attribute 'highlight-numbers-number nil :foreground myred))

;;__________________________________________________________
;; Flyspell (Orthography)
(use-package flyspell :ensure t
  :diminish
  :hook (;;(prog-mode . flyspell-prog-mode)
		 (text-mode . flyspell-mode))
  :bind (:map flyspell-mode-map
			  ("C-M-i" .  nil)
			  ("C-;" .	nil)
			  ("C-," .	nil)
			  ("C-." .	nil)
			  ("C-c $" .  nil)
			  ("C-c f r" . flyspell-region)
			  ("C-c f b" . flyspell-buffer)
			  ("C-c f n" . flyspell-goto-next-error)
			  )
  :config
  (which-key-add-key-based-replacements "C-c f" "flyspell")

  (use-package flyspell-correct-ido :ensure flyspell-correct
	:diminish
	:bind ("C-c f f" . flyspell-correct-wrapper)
	:init
	(setq flyspell-correct-interface 'flyspell-correct-ivy)))


;;__________________________________________________________
;; {c/c++}-mode
;;__________________________________________________________

;;__________________________________________________________
;; Indent with tabs align with spaces
;; (use-package smart-tabs-mode :ensure t
;;	 :hook (prog-mode)
;;	 :config
;;	 (smart-tabs-insinuate 'c 'c++))

;;__________________________________________________________
;; ycmd Mode

;; (use-package ycmd :ensure t
;;	 :hook ((c-mode . ycmd-mode)
;;		 (c++-mode . ycmd-mode))
;;	 :config
;;	 (setq ycmd-server-command '("python" "/home/ergo/gits/ycmd/ycmd"))
;;	 (setq ycmd-global-config "/home/ergo/gits/ycmd/.ycm_extra_conf.py")

;;	 (use-package company-ycmd :ensure t
;;	:config
;;	(company-ycmd-setup))

;;	 (use-package flycheck-ycmd :ensure t
;;	:config
;;	(flycheck-ycmd-setup))

;;	 (use-package ycmd-eldoc
;;	:config
;;	(ycmd-eldoc-setup)))

;;__________________________________________________________
;; LSP try for a while

(defun my/lsp-mode-hook () "My lsp mode hook"

  (use-package lsp-mode :ensure t
	:hook ((c++-mode . lsp)
		   (c-mode . lsp))
	:init
	(setq lsp-auto-configure nil
		  lsp-prefer-flymake nil)

	:config
	(require 'lsp-clients)
	;; (use-package lsp-ui :ensure t
	;;   :after flycheck
	;;   :config
	;;   (lsp-ui-mode)
	;;   (use-package lsp-ui-flycheck
	;; 	:config
	;; 	(lsp-ui-flycheck-enable t)))

	(use-package company-lsp :ensure t
	  :after company
	  :config
	  (add-to-list (make-local-variable 'company-backends) 'company-lsp))))

;;__________________________________________________________
;; Irony config (C completions)

(defun my/irony-mode-hook () "My irony-mode Hook.

This is in the hook for c-common mode.	If the file is remote it loads
company-c-headers instead if irony"
  (use-package irony :ensure t
	:diminish
	:config
	(irony-mode)
	(irony-cdb-autosetup-compile-options)

	(use-package company-irony :ensure t
	  :config
	  (use-package company-irony-c-headers :ensure t)

	  (add-to-list (make-local-variable 'company-backends)
				   '(company-irony-c-headers company-irony)))

	(define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
	(define-key irony-mode-map [remap complete-symbol] 'counsel-irony)

	(use-package flycheck-irony :ensure t
	  :after flycheck
	  :config
	  (flycheck-irony-setup))

	(use-package irony-eldoc :ensure t
	  :if eldoc-mode
	  :config
	  (irony-eldoc))))

;;__________________________________________________________
;; C common mode (for all c-like languajes)

(setq-default c-default-style
			  '((java-mode . "java")
				(awk-mode . "awk")
				(other . "linux")))

(defun my/c-mode-common-hook () "My hook for C and C++."

	   (when (and (member major-mode '(c++-mode c-mode arduino-mode))
				  buffer-file-name)

		 (if (string-match-p tramp-file-name-regexp buffer-file-name)
		 	 (use-package company-c-headers :ensure t ;; company-c-headers
			   :after company
			   :config
			   (add-to-list (make-local-variable 'company-backends) 'company-c-headers))

		   (my/lsp-mode-hook)
		   ;;(my/irony-mode-hook)
		   ))

	   (setq c-doc-comment-style
			 '((java-mode . javadoc)
			   (pike-mode . autodoc)
			   (c-mode	  . javadoc)
			   (c++-mode  . javadoc))
			 c-basic-offset 4		  ;; Default is set-from-style
			 indent-tabs-mode t)

	   (c-setup-doc-comment-style)	 ;; update commentd style

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
	   (setq flycheck-gcc-language-standard "c++17"
			 flycheck-clang-language-standard "gnu++17")
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
	:hook ruby-electric-mode)
  (setq-default ruby-indent-level 2))

;;__________________________________________________________
;; Julia Mode
(use-package julia-mode :ensure t
  :mode "\\.jl\\'"
  :config
  (use-package flycheck-julia :ensure t
	:after flycheck
	:config
	(flycheck-julia-setup)))

;;__________________________________________________________
;; Rust Mode
(use-package rust-mode :ensure t
  :mode "\\.rs\\'"
  :config
  (use-package flycheck-rust :ensure t
	:after flycheck
	:config
	(flycheck-rust-setup)))

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
	(add-to-list (make-local-variable 'company-backends) 'company-go))

  (use-package go-snippets :ensure t))

;;__________________________________________________________
;; lua language
(use-package lua-mode :ensure t
  :mode "\\.lua\\'"
  :config
  (use-package company-lua :ensure t
	:after company
	:config
	(add-to-list (make-local-variable 'company-backends) 'company-lua)))

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

;; Move split keybindings
(use-package windmove
  :bind (("C-x <left>" . windmove-left)
		 ("C-x <right>" . windmove-right)
		 ("C-x <up>" . windmove-up)
		 ("C-x <down>" . windmove-down))
  :init
  (which-key-add-key-based-replacements "C-x w" "windmove winner")
  ;;:config
  ;;(windmove-default-keybindings 'meta)  ;; Move between panes S-arrow
  ;;(setq windmove-wrap-around t)		  ;; Cyclic bound mode
  )

;; Undo redo split
(use-package winner-mode
  :bind (("C-x w u" . winner-undo)
		 ("C-x w r" . winner-redo))
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode t))

;; winum (windows number) for spaceline
(use-package winum :ensure t
  :bind (("C-x w 1" . winum-select-window-1)
		 ("C-x w 2" . winum-select-window-2)
		 ("C-x w 3" . winum-select-window-3)
		 ("C-x w 4" . winum-select-window-4)
		 ("C-x w 5" . winum-select-window-5)
		 ("C-x w 6" . winum-select-window-6)
		 ("C-x w 7" . winum-select-window-7)
		 ("C-x w 8" . winum-select-window-8))
  :init
  (winum-mode))

;;__________________________________________________________
;; Confirmation for to exit emacs
(defalias 'yes-or-no-p 'y-or-n-p)	  ;; Reemplazar "yes" por "y" en el prompt
(setq confirm-kill-emacs 'y-or-n-p)	  ;; Puede ser 'nil o 'y-or-n-p

;;__________________________________________________________
;; Lines enabling gnuplot-mode
(use-package gnuplot-mode :ensure t
  :mode ("\\.gp\\'" "\\.gpl\\'" "\\.plt\\'"))

;; __________________________________________________________
;; Emacs lisp
(use-package elisp
  :preface
  (defun my/elisp-mode-hook () "My elisp mode hook"
		 (add-to-list
		  (make-local-variable 'company-backends) 'company-elisp))
  :hook (emacs-lisp-mode . my/elisp-mode-hook))

;;__________________________________________________________
;; Auto completamiento

(use-package company :ensure t
  :bind (:map company-mode-map ("C-c RET" . company-other-backend)
			  :map company-active-map ("C-c RET" . company-other-backend))
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config

  (set-face-attribute 'company-tooltip nil		  ;; dialog face
					  :background mybrightblack :foreground mywhite)
  (set-face-attribute 'company-tooltip-common nil ;; common part face
					  :inherit 'company-tooltip :foreground mygreen)
  (set-face-attribute 'company-tooltip-selection nil ;; selection face
					  :background myblue :weight 'ultra-bold)
  (set-face-attribute 'company-scrollbar-bg nil	  ;; scroll bar face bg
					  :background mybrightblack)
  (set-face-attribute 'company-scrollbar-fg nil	  ;; scroll bar face fg
					  :background myblue)
  (setq company-idle-delay 1.0	 ;; no delay for autocomplete
		company-minimum-prefix-length 2
		;;company-tooltip-limit 20
		;;company-show-numbers t
		company-backends '(company-semantic
						   company-capf		 ;; completion at point
						   company-files	 ;; company files
						   (company-dabbrev-code company-gtags company-keywords)
						   company-dabbrev
						   )))

;; (use-package yasnippet :ensure t
;; 	:diminish
;; 	:hook (prog-mode . yas-minor-mode)
;; 	:bind (:map yas-minor-mode-map ("TAB" . nil)
;; 				("C-c & TAB" . yas-maybe-expand))
;; 	:init
;; 	(which-key-add-key-based-replacements "C-c &" "yasnippet")
;; 	:config
;; 	(use-package yasnippet-snippets :ensure t)
;; 	(add-to-list 'company-backends 'company-yasnippet))


;;__________________________________________________________
;; Chequeo de syntaxis
(use-package flycheck :ensure t
  :diminish
  :if (< (buffer-size) 200000)
  :config
  (flycheck-mode 1)
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (setq-default flycheck-display-errors-delay 1)

  (use-package flycheck-popup-tip :ensure t
	:after flycheck
	:config
	(flycheck-popup-tip-mode))

  (use-package flycheck-color-mode-line :ensure t
	:after flycheck
	:config
	(flycheck-color-mode-line-mode)))

;;__________________________________________________________
;; Function arguments show

;; (use-package eldoc :ensure t
;;	 :diminish
;;	 :config
;;	 (eldoc-mode t))

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
  (require 'emms-player-simple)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (require 'emms-player-vlc)
  (setq emms-player-list '(emms-player-mpg321
						   emms-player-ogg123
						   emms-player-vlc
						   emms-player-vlc-playlist))
  (setq emms-playlist-buffer-name "*Music*"
		emms-playing-time 1
		emms-source-file-default-directory "~/almacen/Musica/"
		emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
		emms-stream-info-backend 'vlc))

;;__________________________________________________________
;; Email mode for mutt
;;__________________________________________________________
(use-package abbrev :diminish)

;; (use-package mail-mode
;;   :mode ()
;;   :config
;;   (setq-local normal-auto-fill-function 'do-auto-fill)
;;   (auto-fill-mode t)
;;   (mail-abbrevs-setup))

;; Asocia buffers que empiecen con messaje mode
(use-package message-mode
  :mode ("/neomut" "neomutt-Ergus-" "draft")
  :init
  (setq mail-header-separator "")
  :config
  (auto-fill-mode t)
  (mail-abbrevs-setup)
  (flyspell-mode t)

  (use-package notmuch-address :ensure notmuch
	:init
	(setenv "NOTMUCH_CONFIG" "/home/ergo/almacen/mail/notmuch-config")
	(setq notmuch-init-file "~/almacen/mail/notmuch-config")
	:config
	(setq notmuch-address-command "~/gits/notmuch-addrlookup-c/notmuch-addrlookup"))

  (use-package notmuch-company :ensure notmuch
	:config
	(add-to-list (make-local-variable 'company-backends) 'notmuch-company)))


;;__________________________________________________________
;; Latex mode

(use-package latex :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq LaTeX-babel-hyphen nil
		TeX-auto-save t
		TeX-parse-self t
		LaTeX-always-use-Biber t
		TeX-save-query nil		;; don't prompt for saving the .tex file
		;;TeX-newline-function 'reindent-then-newline-and-indent
		;;TeX-source-correlate-method 'synctex
		TeX-source-correlate-start-server t)

  (TeX-source-correlate-mode t)
  (setq-default TeX-master nil)

  ;;(setq LaTeX-fill-break-at-separators (quote (\\\( \\\[ \\\])))
  (flyspell-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 1)

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
	  (add-to-list (make-local-variable 'company-backends)
				   '(company-reftex-labels company-reftex-citations))))

  (use-package company-math :ensure t
	:after company
	:config
	(add-to-list (make-local-variable 'company-backends)
				 '(company-math-symbols-latex company-latex-commands)))

  (use-package company-auctex :ensure t
	:after company
	:config
	(company-auctex-init))

  (add-to-list 'TeX-command-list
			   '("Makeglossaries" "makeglossaries %s" TeX-run-command nil
				 (latex-mode)
				 :help "Run makeglossaries script, which will choose xindy or makeindex") t)

  (flyspell-buffer))

;;__________________________________________________________
;;bibtex mode set use biblatex
(use-package bibtex-mode
  :mode "\\.bib\\'"
  :config
  (bibtex-set-dialect 'biblatex)
  (use-package company-bibtex :ensure t
	:config
	(add-to-list (make-local-variable 'company-backends) 'company-bibtex)))

;;__________________________________________________________
;; Python mode
(use-package python-mode :ensure t
  :mode ("\\.py" . python-mode)
  :config
  (use-package company-jedi :ensure t
	:after company
	:config
	(add-to-list (make-local-variable 'company-backends) 'company-jedi))

  (use-package elpy :ensure t
	:config
	(setq python-shell-interpreter "jupyter"
		  python-shell-interpreter-args "console --simple-prompt"
		  python-shell-prompt-detect-failure-warning nil
		  elpy-rpc-python-command "python3"
		  python-check-command "flake8"
		  )
	(add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
	(elpy-enable)

	)

  (use-package flycheck-pycheckers :ensure t
	:after flycheck
	:config
	(setq flycheck-pycheckers-checkers '(pylint flake8 mypy3))
	(flycheck-pycheckers-setup)))

;;__________________________________________________________
;; Dired-mode settings (file manager)
(use-package dired
  :commands dired
  :bind (:map dired-mode-map
			  ("RET" . dired-find-alternate-file)
			  ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :config
  (setq dired-recursive-copies 'top	 ;; Always ask recursive copy
		dired-recursive-deletes 'top ;; Always ask recursive delete
		dired-dwim-target t)		 ;; Copy in split mode with p
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
	:bind ("C-c s d" . dired-sidebar-toggle-sidebar)
	:commands (dired-sidebar-toggle-sidebar)
	:config
	(setq ;;dired-sidebar-use-term-integration t
	 ;;dired-sidebar-theme 'nerd
	 dired-sidebar-subtree-line-prefix ".")))

;;__________________________________________________________
;; ibuffer

(use-package projectile :ensure t
  :bind (:map projectile-mode-map
			  ("C-c p" . projectile-command-map))
  :config
  (projectile-mode t)

  :custom
  (projectile-completion-system 'ido)
  (projectile-file-exists-remote-cache-expire (* 10 60)))

;;__________________________________________________________
;; ibuffer
(use-package ibuffer :ensure t
  :bind ("C-x C-b" . ibuffer)
  :init
  (defalias 'list-buffers 'ibuffer) ; make ibuffer default
  :config
  (use-package ibuffer-sidebar :ensure t
	:bind ("C-c s b")
	:commands (ibuffer-sidebar-toggle-sidebar))

  (use-package ibuffer-tramp :ensure t
	:hook tramp-mode
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
	   (dired-sidebar-toggle-sidebar))

(global-set-key (kbd "C-c s s") 'my/sidebar-toggle)

;;__________________________________________________________
;; neotree
(use-package neotree :ensure t
  :bind ("C-c s n" . neotree-toggle))

;;__________________________________________________________
;; Ivy (probare un tiempo con helm/ivy)
(use-package headlong :ensure t :defer t)

(use-package ivy :ensure t
  :diminish
  :demand
  :bind (("C-c i r" . ivy-resume)
		 :map ivy-minibuffer-map
		 ("TAB" . ivy-partial)
		 ("RET" . ivy-alt-done))
  :config

  ;;(set-face-attribute 'minibuffer-prompt nil :foreground mycyan) ;; prompt minibuffer
  (set-face-attribute 'ivy-current-match nil
					  :background mybrightblack :foreground mygreen :weight 'ultrabold)
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil ;; Espacio entre matches
					  :inherit nil :background mybrightblack)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil ;; primer match
					  :inherit nil :background mybrightblack)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil ;; segundo match
					  :inherit nil :background mybrightblack)


  (setq ivy-use-virtual-buffers t
		ivy-count-format "(%d/%d) "
		ivy-display-style 'fancy
		ivy-pulse-delay nil
		;;ivy-height 5
		;;ivy-wrap t					 ;; cycle in minibuffer
		enable-recursive-minibuffers t)

  (ivy-mode t)

  (use-package ivy-rich :ensure t
	:config
	(ivy-rich-mode 1))

  (use-package swiper :ensure t
	:bind (("C-s" . swiper)
		   ("C-r" . swiper)
		   :map minibuffer-local-map ("C-r" . counsel-minibuffer-history)
		   :map read-expression-map ("C-r" . counsel-expression-history))
	;;:config
	;; (set-face-attribute 'swiper-line-face nil :inherit nil
	;;					:background mybrightblack :weight 'bold)
	))

(use-package imenu-anywhere :ensure t
  :bind ("C-c i i" . ivy-imenu-anywhere)
  :init
  (setq imenu-auto-rescan t))

(use-package imenu-list :ensure t
  :bind ("C-c s i" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-position 'left))

(use-package counsel :ensure t
  :diminish
  :bind (:map counsel-mode-map
			  ("C-c c a" . counsel-ag)
			  ("C-c c a" . 'counsel-ag)
			  ("C-c c i" . 'counsel-imenu)
			  ("C-c c g" . 'counsel-grep)
			  ("C-c c t" . 'counsel-git)
			  ("C-c c r" . 'counsel-git-grep)
			  ("C-c c l" . 'counsel-locate))
  :init
  (counsel-mode t)
  (which-key-add-key-based-replacements "C-c c" "counsel")

  :config
  (use-package counsel-tramp :ensure t
	:after exec-path-from-shell
	:commands counsel-tramp
	:config
	(setq tramp-default-method "ssh"))

  (use-package counsel-gtags :ensure t
	:diminish
	:hook (c-mode-common . counsel-gtags-mode)
	:bind (:map counsel-gtags-mode-map
				("C-c g q" . counsel-gtags-find-definition)
				("C-c g q" . counsel-gtags-find-definition)
				("C-c g r" . counsel-gtags-find-reference)
				("C-c g s" . counsel-gtags-find-symbol)
				("C-c g p" . counsel-gtags-go-backward)
				("C-c g n" . counsel-gtags-go-forward)
				("C-c g c" . counsel-gtags-create-tags)
				("C-c g u" . counsel-gtags-update-tags))
	:init
	(which-key-add-key-based-replacements "C-c g" "counsel-gtags")
	:config
	(counsel-gtags-mode 1)
	(add-to-list (make-local-variable 'company-backends) 'company-gtags))

  (use-package counsel-projectile :ensure t
	:after projectile
	:config
	(counsel-projectile-mode t))
  )

(use-package dumb-jump :ensure t
  :bind (("C-c j 4 n" . dumb-jump-go-other-window)
		 ("C-c j 4 x" . dumb-jump-go-prefer-external-other-window)
		 ("C-c j n" . dumb-jump-go)
		 ("C-c j i" . dumb-jump-go-prompt)
		 ("C-c j x" . dumb-jump-go-prefer-external)
		 ("C-c j p" . dumb-jump-back)
		 ("C-c j q" . dumb-jump-quick-look))
  :init
  (which-key-add-key-based-replacements "C-c j" "dumb-jump")
  ;;:config
  ;;(setq dumb-jump-selector 'ivy)
  )

(use-package hydra :ensure t
  :init
  (which-key-add-key-based-replacements "C-c v" "hydra-vi")
  :config
  ;;(use-package ivy-hydra :ensure t)
  (global-set-key (kbd "C-c v")
				  (defhydra hydra-vi (:pre (set-cursor-color "#e52b50")
										   :post (set-cursor-color "#ffffff")
										   :color amaranth)
					"vi"
					("/" search-forward)
					("l" forward-char)
					("k" next-line)
					("j" backward-char)
					("i" previous-line)
					("m" set-mark-command "mark")
					("a" move-beginning-of-line "beg")
					("e" move-end-of-line "end")
					("b" left-word "pWord")
					("w" right-word "nWord")
					("<" beginning-of-buffer "head")
					(">" end-of-buffer "tail")
					("^" back-to-indentation "indent")
					("d" kill-region "kill")
					("u" undo-tree-undo "undo")
					("p" yank "paste")
					("C-i" backward-paragraph "pPar")
					("C-k" forward-paragraph "nPar")
					("y" kill-ring-save "yank")
					("ESC" nil)
					("C-g" nil)))
  (hydra-set-property 'hydra-vi :verbosity 1))

;;__________________________________________________________
;; Historical completion
(use-package historian :ensure t
  :config
  (use-package ivy-historian :ensure t
	:after ivy
	:config (ivy-historian-mode t)))

;;__________________________________________________________
;; Complete history
(use-package amx :ensure t
  :bind (("M-x" . amx)
		 ("M-X" . amx-major-mode-commands)))

;;__________________________________________________________
;; Magit
(use-package magit :ensure t
  :commands magit-status
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

;;______________________________________
;; Git commit
(use-package git-commit :ensure t
  :mode ("COMMIT_EDITMSG" . global-git-commit-mode)
  :config
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
  :mode ("/CMakeLists\\.txt\\'" "\\.cmake\\'")
  :after company
  :config
  (use-package cmake-font-lock :ensure t
	:config
	(add-hook 'cmake-mode-hook 'cmake-font-lock-activate))
  (add-to-list (make-local-variable 'company-backends) 'company-cmake))

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
  :bind ("C-c C-b" . better-shell-shell))
;;__________________________________________________________
;;; Google calendar (view only)

(use-package org :ensure t
  :mode ("\\.org$" . org-mode)
  :config
  (use-package org-bullets :ensure t
	:hook (org-mode . org-bullets-mode)))

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

;;__________________________________________________________
;; Move current line up and down Shift+arrow
(use-package move-text :ensure t
  :bind(("C-M-<up>" . move-text-up)
		("C-M-<down>" . move-text-down)
		("C-M-<left>" . (lambda () (interactive) (transpose-words -1)))
		("C-M-<right>" . (lambda () (interactive) (transpose-words 1)))
		("M-<left>" . (lambda () (interactive) (transpose-chars -1)))
		("M-<right>" . (lambda () (interactive) (transpose-chars 1)))))

;;__________________________________________________________
;; for having tabs in top
(use-package elscreen :ensure t
  :bind ("C-c t" . elscreen-start)
  :init
  (which-key-add-key-based-replacements	"C-c t" "elscreen")
  :config
  (global-set-key (kbd "C-c t") nil)
  (setq elscreen-prefix-key (kbd "C-c t")
		elscreen-tab-display-kill-screen nil
		elscreen-tab-display-control nil
		elscreen-display-screen-number nil)
  )

;;__________________________________________________________
;; evil mode

(use-package avy :ensure t
  :bind (("C-; r" . avy-resume)
		 ("C-; c" . avy-goto-char)
		 ("C-; i" . avy-goto-char-in-line)
		 ("C-; 2" . avy-goto-char-2)
		 ("C-; t" . avy-goto-char-timer)
		 ("C-; l" . avy-goto-line)
		 ("C-; ;" . avy-goto-word-or-subword-1)
		 ("C-; m r" . avy-move-region)
		 ("C-; m l" . avy-move-line)
		 ("C-; k r" . avy-kill-region)
		 ("C-; k l" . avy-kill-whole-line)
		 ("C-; y r" . avy-kill-ring-save-region)
		 ("C-; y l" . avy-kill-ring-save-whole-line)
		 ("C-; w r" . avy-copy-region)
		 ("C-; w l" . avy-copy-line)
		 ("C-; p" . avy-prev)
		 ("C-; n" . avy-next)
		 ("C-; C-x" . avy-pop-mark)
		 ("C-; e" . avy-goto-end-of-line))
  :init
  (which-key-add-key-based-replacements
	"C-; m" "avy-move"
	"C-; k" "avy-kill"
	"C-; y" "avy-paste"
	"C-; w" "avy-copy")

  (use-package zzz-to-char :ensure t
  :bind(("C-; k u" . zzz-up-to-char)
		("C-; k z" . zzz-to-char)))

  :config
  (setq avy-keys (nconc (number-sequence ?a ?z)	 ;; Order of proposals
						(number-sequence ?A ?Z)
						(number-sequence ?1 ?9)
						'(?0))
		avy-style 'at							 ;; Propose only 1 letter
		avy-background t
		avy-all-windows nil						 ;; Only current window
		avy-case-fold-search nil
		avy-highlight-first t)
  (set-face-attribute 'avy-lead-face nil :background myblack :foreground myred))

(use-package arduino-mode :ensure t
  :mode "\\.ino\\'"
  :config
  (use-package company-arduino :ensure t
	:config
	(company-arduino-turn-on))

  (use-package flycheck-arduino
	:after flycheck
	:config
	(flycheck-arduino-setup))
  )

(use-package expand-region :ensure t
  :bind (("C-c e e" . er/expand-region)
		 ("C-c e w" . er/mark-word)
		 ("C-c e c" . er/mark-comment)
		 ("C-c e s" . er/c-mark-statement)
		 ("C-c e i" . er/mark-inside-pairs)
		 ("C-c e o" . er/mark-outside-pairs)
		 ("C-c e t p" . er/mark-text-paragraph)
		 ("C-c e t s" . er/mark-text-sentence)
		 ("C-c e f" . er/mark-defun)
		 ("C-c e m" . er/mark-email)
		 ("C-c e u" . er/mark-url)
		 ("C-c e \"" . er/mark-outside-quotes)
		 ("C-c e '" . er/mark-inside-quotes)
		 ("C-c e -" . er/contract-region)
		 )
  :init
  (which-key-add-key-based-replacements "C-c e" "expand-region"))


(use-package web-mode :ensure t
  :mode ("\\.html\\'" "\\.php\\'")
  :init
  (setq web-mode-enable-current-element-highlight t
		web-mode-enable-block-face t
		web-mode-enable-auto-expanding t
		web-mode-enable-current-column-highlight t
		web-mode-comment-style 2)
  :config

  (use-package company-web :ensure t
	:config
	(add-to-list (make-local-variable 'company-backends) '(company-web-html)))

  (setq web-mode-engines-alist
		'(("php"	. "\\.phtml\\'")))

  (use-package web-mode-edit-element :ensure t))

(use-package nginx-mode :ensure t
  :commands (nginx-mode)
  :config
  (use-package company-nginx :ensure t
	:hook nginx-mode))

(use-package json-mode :ensure t
  :mode "\\.json\\'")

(use-package sudo-edit :ensure t
  :commands sudo-edit)

(use-package evil :ensure t
  :commands evil-mode
  :init
  (setq evil-esc-delay 0.001
		evil-want-keybinding nil)
  :config
  (setq show-paren-when-point-inside-paren t)	 ;; show parent even when over)

  (use-package evil-collection :ensure t
	:config
	(evil-collection-init))

  ;; (use-package evil-leader :ensure t
  ;; 	:config
  ;; 	(global-evil-leader-mode)
  ;; 	(evil-leader/set-key "e" 'find-file
  ;; 						 "b" 'switch-to-buffer
  ;; 						 "k" 'kill-buffer))
  )


(provide 'init)
;;; init.el ends here
