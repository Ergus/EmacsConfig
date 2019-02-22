;;; init.el --- Emacs Initialization and Configuration
;; Copyright (C) 2018 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;; Code:

(defconst my/start-time (current-time))

(defmacro mt (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))


(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184       ;; Defer Garbage collection
      gc-cons-percentage 1.0)

(add-hook 'emacs-startup-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
			 (garbage-collect)) t)

;;__________________________________________________________
;; Internal options

(setq show-paren-delay 0)
(show-paren-mode t)				 ;; Highlight couple parentesis

(setq-default auto-revert-verbose nil)   ;; not show message when file changes
(global-auto-revert-mode t)		 ;; Autoload files changed in disk

(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(electric-indent-mode -1)
(tooltip-mode -1)			;; Tool tip in the echo
(flymake-mode -1)

(global-font-lock-mode t)	   ;; Use font-lock everywhere.
(savehist-mode t)				 ;; Historial
(auto-compression-mode t)		 ;; Uncompress on the fly:
(global-display-line-numbers-mode t) ;; line numbers on the left
(delete-selection-mode t)		 ;; Sobreescribe seleccion al pegar
(size-indication-mode t)
(transient-mark-mode t)

(setq-default vc-follow-symlinks t	;; Open links not open
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
			  ;;scroll-step 1			;; Scroll one by one (better conservatively)
			  scroll-conservatively 100000
			  ;;scroll-margin 2			   ;; lines at top or button to scroll
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
			  recenter-redisplay nil
			  )

(put 'narrow-to-region 'disabled nil)		   ;; Enable narrow commands

;;__________________________________________________________
;; For using Melpa and Elpa
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")))
(setq package-quickstart t)
(package-initialize)

;;__________________________________________________________
;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package)

  (if init-file-debug
      (setq use-package-verbose t
			use-package-expand-minimally nil
			use-package-compute-statistics t
			debug-on-error t)
	(setq use-package-verbose nil
		  use-package-expand-minimally t)))

(setq use-package-always-ensure t)
;;__________________________________________________________
;; Benchmark-init
(use-package benchmark-init
  :config
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

(use-package diminish)                ;; if you use :diminish
(use-package bind-key)                ;; if you use any :bind variant

(use-package paradox
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config
  (setq paradox-spinner-type 'progress-bar))

;;__________________________________________________________
;; Config file not here to not track it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file)

;;__________________________________________________________
;; To put all my lisp scripts
(defvar mylisp-dir (expand-file-name "lisp" user-emacs-directory))

(unless (file-exists-p mylisp-dir)
  (make-directory mylisp-dir)
  (message "Creating %s" mylisp-dir))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; System Lisp scripts
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;;__________________________________________________________
;;	The Colors (I want to change this for a real theme, there are maaaaany)

(defconst my/colors '((black . "#000000")
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
					  (brightwhite . "#ffffff"))
  "List of colors.")

(defun my/colors () "Define my color theme."

	   (set-background-color (cdr (assoc 'black my/colors)))
	   (set-foreground-color (cdr (assoc 'white my/colors)))

	   (set-face-foreground 'font-lock-preprocessor-face (cdr (assoc 'magenta my/colors)))		;; Preprocessor

	   (set-face-foreground 'font-lock-comment-face (cdr (assoc 'cyan my/colors)))				;; Comentarios
	   (set-face-foreground 'font-lock-doc-face (cdr (assoc 'brightcyan my/colors)))					;; Documentation

	   (set-face-foreground 'font-lock-string-face (cdr (assoc 'red my/colors)))				;; Strings
	   (set-face-foreground 'font-lock-function-name-face (cdr (assoc 'white my/colors)))		;; Funciones
	   (set-face-foreground 'font-lock-variable-name-face (cdr (assoc 'white my/colors)))		;; Variables
	   (set-face-foreground 'font-lock-constant-face (cdr (assoc 'magenta my/colors)))			;; Constates y Clases

	   (set-face-foreground 'font-lock-type-face (cdr (assoc 'green my/colors)))				;; Tipos (int, float)
	   (set-face-foreground 'font-lock-keyword-face (cdr (assoc 'yellow my/colors)))  	        ;; Keywords (for, if)
	   (set-face-foreground 'font-lock-builtin-face (cdr (assoc 'green my/colors)))			    ;; Keywords (for, if)

	   (set-face-attribute 'highlight nil :background (cdr (assoc 'brightblack my/colors)) :foreground nil)

	   (set-face-attribute 'secondary-selection nil :background (cdr (assoc 'brightblue my/colors))
						   :foreground (cdr (assoc 'blue my/colors)) :weight 'bold)

	   ;; search C-s, resalta lo que encuentra
	   (set-face-attribute 'isearch nil :background (cdr (assoc 'blue my/colors))
						   :foreground (cdr (assoc 'white my/colors)) :weight 'ultrabold)			   ;; Search

	   (set-face-attribute 'region nil :inherit nil :background (cdr (assoc 'brightblack my/colors))) ;; Seleccion C-space

	   (set-face-attribute 'line-number nil :foreground (cdr (assoc 'brightblack my/colors)))		   ;; numero de linea
	   (set-face-attribute 'line-number-current-line nil :foreground (cdr (assoc 'green my/colors)))  ;; resalta la linea actual
	   )

(my/colors)


;;__________________________________________________________
;;Packages options
;;__________________________________________________________
;;__________________________________________________________

;;__________________________________________________________
;; ssh
(use-package tramp :ensure nil
  :defer t
  :config
  (setq compilation-scroll-output 'first-error)
  (use-package tramp-term)

  (setq tramp-default-method "ssh"
		;;tramp-change-syntax 'simplified
		tramp-use-ssh-controlmaster-options nil
		tramp-completion-reread-directory-timeout t
		tramp-persistency-file-name "~/.emacs.d/tramp"))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
		 ("/sshd?_config\\'" . ssh-config-mode)
		 ("/known_hosts\\'" . ssh-known-hosts-mode)
		 ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))


(defun my/term-mode-hook () "My term mode hook"
	   (display-line-numbers-mode -1))

(add-hook 'term-mode-hook 'my/term-mode-hook)

;;__________________________________________________________
;; minibuffers

(use-package minibuffer :ensure nil
  :config
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook))


;;__________________________________________________________
;; cua rectangles

;; (use-package cua-base
;;   :config
;;   (cua-selection-mode t))			 ;; Better rectangle selection

(use-package gdb :ensure nil
  :commands gdb
  :init
  (setq-default gdb-many-windows t))

;;__________________________________________________________
;; Two options for diffs
(use-package ediff :ensure nil
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
(use-package vdiff
  :commands (vdiff-files
			 vdiff-files3
			 vdiff-buffers
			 vdiff-buffers3))

;;__________________________________________________________
;; Diminish To Hide Packages from bar
(use-package diminish)

;;__________________________________________________________
;; Mocp and multi-term music player
(use-package multi-term
  :commands (multi-term-dedicated-open
			 multi-term))

(use-package sane-term
  :bind (("C-x t" . sane-term)
		 ("C-x T" . sane-term-create))
  :init
  (setq sane-term-shell-command "/bin/bash")
  )

;;__________________________________________________________
;; which-key
(use-package which-key
  :defer 5
  :diminish
  :config
  (setq which-key-separator ": "
		which-key-idle-delay 0.8)
  (which-key-mode t)
  (which-key-add-key-based-replacements
	"C-c h" "highlight"
	"C-c s" "sidebars"
	"C-x r" "rectangle-register"
	"C-x n" "narrow"
	"C-x a" "abrev"))

;;__________________________________________________________
;; Status bar (mode line in emacs) two options to chose

;; (use-package spaceline
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

;; 	 (set-face-attribute 'mode-line nil :background myblue :foreground mywhite))

;; (use-package smart-mode-line
;;   :config
;;   (use-package smart-mode-line-powerline-theme
;; 	:config
;; 	(setq sml/theme 'powerline)
;; 	)
;;   (setq sml/no-confirm-load-theme t
;; 		sml/name-width 40)
;;   (sml/setup))

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

	   (use-package whole-line-or-region)

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
(use-package mouse :ensure nil
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
(use-package multiple-cursors  ;; Multiple cursors package
  :bind (("C-c m m" . mc/edit-lines)
		 ("C-c m r" . mc/mark-all-in-region)
		 ("C-c m i" . mc/mark-more-like-this-extended)
		 ("C-c m a" . mc/mark-all-like-this)
		 ("C-c m w" . mc/mark-all-words-like-this)
		 ("C-c m C-f" . mc/mark-next-like-this-word)
		 ("C-c m n" . mc/mark-next-like-this)
		 ("C-c m #" . mc/insert-numbers)
		 ("C-c m l" . mc/insert-letters)
		 ("C-c m p" . mc/mark-previous-like-this)
		 ("C-c m C-a" . mc/edit-beginnings-of-lines)
		 ("C-c m C-e" . mc/edit-ends-of-lines)
		 ("C-c m <mouse-1>" . mc/add-cursor-on-click))
  :init
  (which-key-add-key-based-replacements "C-c m" "multiple-cursors")
  :config
  (setq mc/always-run-for-all t)
  )

;;__________________________________________________________
;; My program's mode hooks

(use-package whitespace-mode :ensure nil
  :hook prog-mode
  :init
  (setq whitespace-style '(face trailing)))

(use-package clean-aindent-mode
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
(use-package fill-column-indicator
  :bind ("C-c h f" . fci-mode)
  :commands fci-mode
  :config
  (setq fci-rule-color "#7f7f7f7f7f7f"
		fci-rule-character ?\u2502))

;;__________________________________________________________
;; Undo tree
(use-package undo-tree
  :diminish
  :init (global-undo-tree-mode))

;;__________________________________________________________
;; Mark column 80 when crossed
(use-package hl-line-mode :ensure nil
  :diminish
  :bind ("C-c h l" . column-enforce-mode)
  :commands column-enforce-mode
  :config
  (column-enforce-mode t)
  (setq column-enforce-comments nil)
  (set-face-attribute 'column-enforce-face nil :inherit nil :background (cdr (assoc 'brightblack my/colors))))


;;__________________________________________________________
;; Mark column 80 when crossed
(use-package column-enforce-mode
  :diminish
  :bind ("C-c h c" . column-enforce-mode)
  :commands column-enforce-mode
  :config
  (column-enforce-mode t)
  (setq column-enforce-comments nil)
  (set-face-attribute 'column-enforce-face nil :inherit nil :background (cdr (assoc 'brightblack my/colors))))

;;__________________________________________________________
;; Lineas de Indent
(use-package highlight-indent-guides
  :diminish
  :bind ("C-c h i" . highlight-indent-guides-mode)
  :commands (highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-enabled nil
		highlight-indent-guides-method 'character)
  (set-face-attribute 'highlight-indent-guides-character-face nil :foreground "#242424"))

;;__________________________________________________________
;; Resalta parentesis entorno al cursor
;; (use-package highlight-parentheses
;;	 :diminish
;;	 :hook (prog-mode . highlight-parentheses-mode)
;;	 :config
;;	 (set-face-attribute 'hl-paren-face nil :weight 'bold)
;;	 (setq hl-paren-delay 0.05
;;		hl-paren-colors	'("#00ff00" "#00ffff" "#ff0000" "#cd0000")))

;;__________________________________________________________
;; Resalta scopes entorno al cursor
(use-package highlight-blocks
  :diminish
  :bind (("C-c h b" . highlight-blocks-now)
		 ("C-c h B" . highlight-blocks-mode))
  :commands (highlight-blocks-now highlight-blocks-mode)
  :config
  (set-face-attribute 'highlight-blocks-depth-2-face nil :background "#262626") ;; gray15
  (set-face-attribute 'highlight-blocks-depth-3-face nil :background "#333333") ;; gray20
  (set-face-attribute 'highlight-blocks-depth-4-face nil :background "#404040") ;; gray25
  (set-face-attribute 'highlight-blocks-depth-5-face nil :background "#4d4d4d")
  (set-face-attribute 'highlight-blocks-depth-6-face nil :background "#595959")
  (set-face-attribute 'highlight-blocks-depth-7-face nil :background "#666666")
  (set-face-attribute 'highlight-blocks-depth-8-face nil :background "#737373")
  (set-face-attribute 'highlight-blocks-depth-9-face nil :background "#7f7f7f"))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode)
  :config
  (set-face-attribute 'hes-escape-backslash-face nil :foreground (cdr (assoc 'magenta my/colors)))
  (set-face-attribute 'hes-escape-sequence-face nil :foreground (cdr (assoc 'magenta my/colors))))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode)
  :config
  (set-face-attribute 'highlight-numbers-number nil :foreground (cdr (assoc 'red my/colors))))

;;__________________________________________________________
;; Flyspell (Orthography)
(use-package flyspell :ensure nil
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
;; (use-package smart-tabs-mode
;;	 :hook (prog-mode)
;;	 :config
;;	 (smart-tabs-insinuate 'c 'c++))

;;__________________________________________________________
;; ycmd Mode

;; (use-package ycmd
;;	 :hook ((c-mode . ycmd-mode)
;;		 (c++-mode . ycmd-mode))
;;	 :config
;;	 (setq ycmd-server-command '("python" "/home/ergo/gits/ycmd/ycmd"))
;;	 (setq ycmd-global-config "/home/ergo/gits/ycmd/.ycm_extra_conf.py")

;;	 (use-package company-ycmd
;;	:config
;;	(company-ycmd-setup))

;;	 (use-package flycheck-ycmd
;;	:config
;;	(flycheck-ycmd-setup))

;;	 (use-package ycmd-eldoc
;;	:config
;;	(ycmd-eldoc-setup)))

;;__________________________________________________________
;; LSP try for a while
(defun my/lsp-mode-hook () "My lsp mode hook."
	   (use-package lsp-mode
		 :hook ((c++-mode . lsp)
				(c-mode . lsp))
		 :init
		 (setq lsp-auto-configure nil
			   lsp-prefer-flymake nil)

		 :config
		 (require 'lsp-clients)
		 ;; (use-package lsp-ui
		 ;;   :after flycheck
		 ;;   :config
		 ;;   (lsp-ui-mode)
		 ;;   (use-package lsp-ui-flycheck
		 ;; 	:config
		 ;; 	(lsp-ui-flycheck-enable t)))

		 (use-package company-lsp
		   :after company
		   :config
		   (add-to-list (make-local-variable 'company-backends) 'company-lsp))))

;;__________________________________________________________
;; Irony config (C completions)

(defun my/irony-mode-hook () "My irony-mode Hook.

This is in the hook for c-common mode.	If the file is remote it loads
company-c-headers instead if irony"
	   (use-package irony
		 :diminish
		 :config
		 (irony-mode)
		 (irony-cdb-autosetup-compile-options)

		 (use-package company-irony
		   :config
		   (use-package company-irony-c-headers)

		   (add-to-list (make-local-variable 'company-backends)
						'(company-irony-c-headers company-irony)))

		 (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
		 (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)

		 (use-package flycheck-irony
		   :after flycheck
		   :config
		   (flycheck-irony-setup))

		 (use-package irony-eldoc
		   :if eldoc-mode
		   :config
		   (irony-eldoc))))

;;__________________________________________________________
;; C common mode (for all c-like languajes)

(setq-default c-default-style
			  '((java-mode . "java")
				(awk-mode . "awk")
				(other . "linux"))
			  c-basic-offset 4		  ;; Default is set-from-style
			  indent-tabs-mode t)

(defun my/c-mode-common-hook () "My hook for C and C++."

	   (when (and (member major-mode '(c++-mode c-mode arduino-mode))
				  buffer-file-name)

		 (if (string-match-p tramp-file-name-regexp buffer-file-name)
		 	 (use-package company-c-headers ;; company-c-headers
			   :after company
			   :config
			   (add-to-list (make-local-variable 'company-backends) 'company-c-headers))
		   (my/lsp-mode-hook)))

	   (use-package preproc-font-lock ;; Preprocessor
		 :config
		 (preproc-font-lock-mode 1)
		 (set-face-attribute 'preproc-font-lock-preprocessor-background nil
							 :inherit 'font-lock-preprocessor-face))

	   (c-set-offset 'cpp-macro 0 nil)
	   (message "Loaded my/c-mode common"))

(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

;;__________________________________________________________
;; Agrega doble indentation a clases y simple a structs (para private y public)

;;__________________________________________________________
;; C++ mode

(defun my/c++-mode-hook () "My C++-Mode hook function."
	   (setq flycheck-gcc-language-standard "c++17"
			 flycheck-clang-language-standard "gnu++17")

	   (defun my/c++-lineup-inclass (langelem) "LANGELEM Offset struct vs class."
			  (let ((inclass (assoc 'inclass c-syntactic-context)))
				(save-excursion
				  (goto-char (c-langelem-pos inclass))
				  (if (or (looking-at "struct")
						  (looking-at "typedef struct"))
					  '+
					'++))))

	   (c-set-offset 'access-label '-)
	   (c-set-offset 'inline-open 0)
	   (c-set-offset 'inclass 'my/c++-lineup-inclass)

	   (use-package modern-cpp-font-lock)

	   (message "Loaded my c++-mode"))

(add-hook 'c++-mode-hook 'my/c++-mode-hook)

;; Even if the file extension is just .c or .h, assume it is a C++ file:
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))

;;__________________________________________________________
;; Cuda
(use-package cuda-mode
  :mode "\\.cu\\'")

;;__________________________________________________________
;; OpenCL Mode
(use-package opencl-mode
  :mode "\\.cl\\'")

;;__________________________________________________________
;; Markdown mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :config
  (flyspell-mode t))

;;__________________________________________________________
;; Restructured text
(use-package rst-mode :ensure nil
  :mode "\\.rst\\'"
  :config
  (use-package sphinx-mode
	:hook rst-mode)
  (flyspell-mode t))

;;__________________________________________________________
;; Makefile
(use-package makefile-mode :ensure nil
  :mode (".*Makefile.*" "\\.mak"))

;;__________________________________________________________
;; ruby-mode
(use-package ruby-mode :ensure nil
  :mode ("\\.rb\\'" "\\.rjs\\'" "\\Rakefile\\'" "\\Gemfile\\'")
  :config
  (use-package ruby-tools)
  (use-package ruby-electric
	:hook ruby-electric-mode)
  (setq-default ruby-indent-level 2))

;;__________________________________________________________
;; Julia Mode
(use-package julia-mode
  :mode "\\.jl\\'"
  :config
  (use-package flycheck-julia
	:after flycheck
	:config
	(flycheck-julia-setup)))

;;__________________________________________________________
;; Rust Mode
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (use-package flycheck-rust
	:after flycheck
	:config
	(flycheck-rust-setup)))

;;__________________________________________________________
;; Ocaml Mode
(use-package caml-mode :ensure caml
  :mode "\\.ml\\'")

;;__________________________________________________________
;; D languaje
(use-package d-mode :mode "\\.d\\'")

;;__________________________________________________________
;; Go languaje
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (use-package company-go
	:after company
	:config
	(add-to-list (make-local-variable 'company-backends) 'company-go))

  (use-package go-snippets))

;;__________________________________________________________
;; lua language
(use-package lua-mode
  :mode "\\.lua\\'"
  :config
  (use-package company-lua
	:after company
	:config
	(add-to-list (make-local-variable 'company-backends) 'company-lua)))

;;__________________________________________________________
;; systemd mode
(use-package systemd
  :mode ("\\.service\\'" "\\.timer\\'" "\\.target\\'"
		 "\\.mount\\'" "\\.socket\\'" "\\.slice\\'"
		 "\\.automount\\'"))

;;__________________________________________________________
;; DOS batch files
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;;__________________________________________________________
;; Use for Qt's .pro and .pri files
(use-package qt-pro-mode
  :mode ("\\.pr[io]\\'")
  :init
  (add-to-list 'auto-mode-alist '("\\.moc\\'" . c++-mode)) ;; Treat .moc files (Qt) as C++
  (add-to-list 'auto-mode-alist '("\\.ui\\'" . xml-mode))  ;; Treat .ui files (Qt) as XML
  )

;;__________________________________________________________
;; javascript-mode
(use-package js2-mode
  :mode ("\\.js\\'")
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode))

;;__________________________________________________________
;; xml-mode
(use-package xml-mode :ensure nil
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
(use-package windmove :ensure nil
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
(use-package winner-mode :ensure nil
  :defer 5
  :bind (("C-x w u" . winner-undo)
		 ("C-x w r" . winner-redo))
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode t))

;; winum (windows number) for spaceline
(use-package winum
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
(use-package gnuplot-mode
  :mode ("\\.gp\\'" "\\.gpl\\'" "\\.plt\\'"))

;; __________________________________________________________
;; Emacs lisp
(use-package elisp :ensure nil
  :preface
  (defun my/elisp-mode-hook () "My elisp mode hook"
		 (add-to-list
		  (make-local-variable 'company-backends) 'company-elisp))
  :hook (emacs-lisp-mode . my/elisp-mode-hook))

;;__________________________________________________________
;; Auto completamiento

(use-package company
  :diminish
  :bind (:map company-mode-map ("C-c RET" . company-other-backend)
			  :map company-active-map ("C-c RET" . company-other-backend))
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 1.0	 ;; no delay for autocomplete
		company-minimum-prefix-length 2
		;;company-tooltip-limit 20
		;;company-show-numbers t
		company-backends '(company-semantic
						   company-capf		 ;; completion at point
						   company-files	 ;; company files
						   (company-dabbrev-code company-gtags company-keywords)
						   company-dabbrev))

  (set-face-attribute 'company-tooltip nil		  ;; dialog face
					  :background (cdr (assoc 'brightblack my/colors)) :foreground (cdr (assoc 'white my/colors)))
  (set-face-attribute 'company-tooltip-common nil ;; common part face
					  :inherit 'company-tooltip :foreground (cdr (assoc 'green my/colors)))
  (set-face-attribute 'company-tooltip-selection nil ;; selection face
					  :background (cdr (assoc 'blue my/colors)) :weight 'ultra-bold)
  (set-face-attribute 'company-scrollbar-bg nil	  ;; scroll bar face bg
					  :background (cdr (assoc 'brightblack my/colors)))
  (set-face-attribute 'company-scrollbar-fg nil	  ;; scroll bar face fg
   					  :background (cdr (assoc 'blue my/colors)))
  )

;; (use-package yasnippet
;; 	:diminish
;; 	:hook (prog-mode . yas-minor-mode)
;; 	:bind (:map yas-minor-mode-map ("TAB" . nil)
;; 				("C-c & TAB" . yas-maybe-expand))
;; 	:init
;; 	(which-key-add-key-based-replacements "C-c &" "yasnippet")
;; 	:config
;; 	(use-package yasnippet-snippets)
;; 	(add-to-list 'company-backends 'company-yasnippet))


;;__________________________________________________________
;; Chequeo de syntaxis
(use-package flycheck
  :diminish
  :if (< (buffer-size) 200000)
  :hook (prog-mode . flycheck-mode)
  :config
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (setq-default flycheck-display-errors-delay 1)

  ;; (use-package flycheck-popup-tip
  ;; 	:after flycheck
  ;; 	:config
  ;; 	(flycheck-popup-tip-mode))

  (use-package flycheck-color-mode-line
	:after flycheck
	:config
	(flycheck-color-mode-line-mode)))

;;__________________________________________________________
;; Function arguments show

;; (use-package eldoc
;;	 :diminish
;;	 :config
;;	 (eldoc-mode t))

(use-package helpful
  :bind (("C-h e" . nil)
		 ("C-h e f" . helpful-function)
         ("C-h e c" . helpful-command)
         ("C-h e m" . helpful-macro)
         ("C-h e l" . helpful-callable)
         ("C-h e k" . helpful-key)
         ("C-h e p" . helpful-at-point)
		 ("C-h e v" . helpful-variable)))


;;__________________________________________________________
;; Chequeo de gramatica
(use-package langtool
  :defer t
  :config
  (setq langtool-default-language "en")
  (setq langtool-language-tool-jar "~/gits/languagetool/languagetool-standalone/target/LanguageTool-4.3-SNAPSHOT/LanguageTool-4.3-SNAPSHOT/languagetool-commandline.jar"))

;;__________________________________________________________
;; EMMS mode.
(use-package emms
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
(use-package abbrev :ensure nil
  :defer 5
  :diminish)

;; (use-package mail-mode
;;   :mode ()
;;   :config
;;   (setq-local normal-auto-fill-function 'do-auto-fill)
;;   (auto-fill-mode t)
;;   (mail-abbrevs-setup))

;; Asocia buffers que empiecen con messaje mode
(use-package message-mode :ensure nil
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
	;;(setq notmuch-init-file "~/almacen/mail/notmuch-config")
	:config
	(setq notmuch-address-command "~/gits/notmuch-addrlookup-c/notmuch-addrlookup"))

  (use-package notmuch-company :ensure notmuch
	:config
	(add-to-list (make-local-variable 'company-backends) 'notmuch-company)))


;;__________________________________________________________
;; Latex mode

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq LaTeX-babel-hyphen nil
		TeX-auto-save t
		TeX-parse-self t
		;;LaTeX-biblatex-use-Biber t
		;;TeX-save-query nil		;; don't prompt for saving the .tex file
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

	(use-package company-reftex
	  :after company
	  :config
	  (add-to-list (make-local-variable 'company-backends)
				   '(company-reftex-labels company-reftex-citations))))

  (use-package company-math
	:after company
	:config
	(add-to-list (make-local-variable 'company-backends)
				 '(company-math-symbols-latex company-latex-commands)))

  (use-package company-auctex
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
(use-package bibtex-mode :ensure nil
  :mode "\\.bib\\'"
  :config
  (bibtex-set-dialect 'biblatex)
  (use-package company-bibtex
	:config
	(add-to-list (make-local-variable 'company-backends) 'company-bibtex)))

;;__________________________________________________________
;; Python mode
(use-package python-mode
  :mode ("\\.py" . python-mode)
  :config
  (use-package company-jedi
	:after company
	:config
	(add-to-list (make-local-variable 'company-backends) 'company-jedi))

  (use-package elpy
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

  (use-package flycheck-pycheckers
	:after flycheck
	:config
	(setq flycheck-pycheckers-checkers '(pylint flake8 mypy3))
	(flycheck-pycheckers-setup)))

;;__________________________________________________________
;; Dired-mode settings (file manager)
(use-package dired :ensure nil
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
  (use-package dired-x :ensure nil
	:config
	(setq dired-guess-shell-alist-user
		  (list
		   (list "\\.\\(ps\|ps.gz\|eps\|eps.gz\|pdf\|PDF\\)$" "evince")
		   (list "\\.\\(rgb\|tiff\|tif\|xbm\|gif\|pgm\|ppm\|bmp\|tga\\)$" "eog ")
		   (list "\\.\\(ppm\|gif\|png\|jpg\|JPG\\)$" "eog")
		   (list "\\.\\(avi\|wav\|flv\|mov\|3gp\\)$" "vlc"))))

  (use-package dired-sidebar
	:bind ("C-c s d" . dired-sidebar-toggle-sidebar)
	:commands (dired-sidebar-toggle-sidebar)
	:config
	(setq ;;dired-sidebar-use-term-integration t
	 ;;dired-sidebar-theme 'nerd
	 dired-sidebar-subtree-line-prefix ".")))

;;__________________________________________________________
;; projectile

(use-package projectile
  :bind (:map projectile-mode-map
			  ("C-c p" . projectile-command-map))
  :config
  (projectile-mode t)

  :custom
  (projectile-completion-system 'ido)
  (projectile-file-exists-remote-cache-expire (* 10 60)))

;;__________________________________________________________
;; ibuffer
(use-package ibuffer :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
  (defalias 'list-buffers 'ibuffer) ; make ibuffer default
  :config
  (use-package ibuffer-sidebar
	:bind ("C-c s b")
	:commands (ibuffer-sidebar-toggle-sidebar))

  (use-package ibuffer-tramp
	:hook tramp-mode
	:config
	(defun my/ibuffer-tramp-hook () "ibuffer tram hook"
		   (ibuffer-tramp-set-filter-groups-by-tramp-connection)
		   (ibuffer-do-sort-by-alphabetic))
	(add-hook 'ibuffer-hook 'my/ibuffer-tramp-hook))

  (use-package ibuffer-projectile
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
(use-package neotree
  :bind ("C-c s n" . neotree-toggle))

;;__________________________________________________________
;; Ivy (probare un tiempo con helm/ivy)
(use-package headlong :defer t)

(use-package ivy
  :diminish
  :defer t
  :bind (("C-c i r" . ivy-resume)
		 :map ivy-minibuffer-map
		 ("TAB" . ivy-partial)
		 ("RET" . ivy-alt-done))
  :config

  ;;(set-face-attribute 'minibuffer-prompt nil :foreground mycyan) ;; prompt minibuffer
  (set-face-attribute 'ivy-current-match nil
  					  :inherit nil :background (cdr (assoc 'brightblack my/colors)) :foreground nil :weight 'ultrabold)
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil ;; Espacio entre matches
  					  :inherit nil :background (cdr (assoc 'brightblue my/colors)) :foreground nil :weight 'ultrabold)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil ;; primer match
  					  :background (cdr (assoc 'blue my/colors)) :foreground nil :weight 'ultrabold)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil ;; segundo match
  					  :inherit nil :background (cdr (assoc 'blue my/colors)) :foreground nil :weight 'ultrabold)

  (setq ivy-use-virtual-buffers t
		ivy-count-format "(%d/%d) "
		ivy-display-style 'fancy
		ivy-pulse-delay nil
		ivy-height 10
		;;ivy-wrap t					 ;; cycle in minibuffer
		enable-recursive-minibuffers t)

  (ivy-mode t)

  (use-package ivy-rich
	:config
	(ivy-rich-mode 1)))

(use-package swiper
  :defer t
  :bind (("C-s" . swiper)
		 ("C-r" . swiper)
		 :map minibuffer-local-map ("C-r" . counsel-minibuffer-history)
		 :map read-expression-map ("C-r" . counsel-minibuffer-history))
  :config
  (set-face-attribute 'swiper-line-face nil ;; segundo match
  					  :background (cdr (assoc 'brightblack my/colors)) :weight 'ultrabold)
  (set-face-attribute 'swiper-match-face-1 nil ;; Espacio entre matches
  					  :inherit nil :background (cdr (assoc 'brightblue my/colors)) :foreground nil :weight 'ultrabold)
  (set-face-attribute 'swiper-match-face-2 nil ;; primer match
  					  :inherit nil :background (cdr (assoc 'blue my/colors)) :foreground nil :weight 'ultrabold)
  (set-face-attribute 'swiper-match-face-3 nil ;; segundo match
  					  :inherit nil :background (cdr (assoc 'blue my/colors)) :foreground nil :weight 'ultrabold)
  )

(use-package imenu-anywhere
  :bind ("C-c i i" . ivy-imenu-anywhere)
  :init
  (setq imenu-auto-rescan t))

(use-package wgrep
  :defer 5)

(use-package imenu-list
  :bind ("C-c s i" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-position 'left))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
		 ("C-x C-f" . counsel-find-file)
		 :map counsel-mode-map
		 ("C-c c a" . counsel-ag)
		 ("C-c c i" . counsel-imenu)
		 ("C-c c g" . counsel-grep)
		 ("C-c c t" . counsel-git)
		 ("C-c c r" . counsel-rg)            ; like git grep
		 ("C-c c r" . counsel-git-grep)
		 ("C-c c l" . counsel-locate)
		 :map help-map                            ; help-map
         ("f" . counsel-describe-function)
         ("v" . counsel-describe-variable)
         ("C-l" . counsel-info-lookup-symbol))
  :init
  (which-key-add-key-based-replacements "C-c c" "counsel")

  :config
  (counsel-mode t)
  (use-package amx)  ;; Complete history

  (use-package counsel-projectile
	:after projectile
	:config
	(counsel-projectile-mode t)))

(use-package counsel-gtags
  :diminish
  :hook (c-mode-common . counsel-gtags-mode)
  :after counsel
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
  (add-to-list (make-local-variable 'company-backends) 'company-gtags))

(use-package dumb-jump
  :bind (("C-c j 4 n" . dumb-jump-go-other-window)
		 ("C-c j 4 x" . dumb-jump-go-prefer-external-other-window)
		 ("C-c j n" . dumb-jump-go)
		 ("C-c j i" . dumb-jump-go-prompt)
		 ("C-c j x" . dumb-jump-go-prefer-external)
		 ("C-c j p" . dumb-jump-back)
		 ("C-c j q" . dumb-jump-quick-look))
  :init
  (which-key-add-key-based-replacements "C-c j" "dumb-jump")
  :config
  (setq dumb-jump-selector 'ivy))

(use-package hydra
  :defer
  :init
  (which-key-add-key-based-replacements "C-c v" "hydra-vi")
  :config
  (global-set-key (kbd "C-c v")
				  (defhydra hydra-vi (:pre (set-cursor-color "#e52b50")
										   :post (set-cursor-color "#ffffff")
										   :color red
										   :foreign-keys warn)
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
					("v" set-mark-command "mark")
					("ESC" nil)
					("C-g" nil)))
  (hydra-set-property 'hydra-vi :verbosity 1))

;;__________________________________________________________
;; Historical completion
(use-package historian
  :config
  (use-package ivy-historian
	:after ivy
	:config (ivy-historian-mode t)))

;;__________________________________________________________
;; Magit
(use-package magit
  :commands magit-status)

;;______________________________________
;; Git commit
(use-package git-commit
  :mode ("COMMIT_EDITMSG" . global-git-commit-mode)
  :config
  (setq git-commit-summary-max-length 50
		fill-column 72)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

;;__________________________________________________________
;; Ensamblador nasm
(use-package nasm-mode
  :mode ("\\.asm\\'" "\\.s\\'"))

;;__________________________________________________________
;; CMake
(use-package cmake-mode
  :mode ("/CMakeLists\\.txt\\'" "\\.cmake\\'")
  :after company
  :config
  (use-package cmake-font-lock
	:config
	(add-hook 'cmake-mode-hook 'cmake-font-lock-activate))
  (add-to-list (make-local-variable 'company-backends) 'company-cmake))

;;__________________________________________________________
;; Cobol
(use-package cobol-mode
  :mode ("\\.cobc\\'" "\\.cob\\'" "\\.cbl\\'" "\\.cpy\\'"))

;;__________________________________________________________
;; path
(defun shell-command-on-buffer (command)
  (interactive (list
				(read-shell-command "Shell command on buffer: " nil nil)))
  (shell-command-on-region (point-min) (point-max) command t t))

;;__________________________________________________________
;; Better shell (for ssh)
(use-package better-shell
  :bind ("C-c C-b" . better-shell-shell))
;;__________________________________________________________
;;; Google calendar (view only)

(use-package org
  :mode ("\\.org$" . org-mode)
  :config
  (use-package org-bullets
	:hook (org-mode . org-bullets-mode)))

;;__________________________________________________________
;;; Google calendar (view only)

(use-package calfw
  :commands (my/calendar)
  :config
  (use-package calfw-org)
  (use-package calfw-ical)
  (use-package calfw-gcal)

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
(use-package move-text
  :bind(("C-M-<up>" . move-text-up)
		("C-M-<down>" . move-text-down)
		("C-M-<left>" . (lambda () (interactive) (transpose-words -1)))
		("C-M-<right>" . (lambda () (interactive) (transpose-words 1)))
		("M-<left>" . (lambda () (interactive) (transpose-chars -1)))
		("M-<right>" . (lambda () (interactive) (transpose-chars 1)))))

;;__________________________________________________________
;; for having tabs in top
(use-package elscreen
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

(use-package avy
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

  (use-package zzz-to-char
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
  (set-face-attribute 'avy-lead-face nil :background (cdr (assoc 'black my/colors)) :foreground (cdr (assoc 'red my/colors))))

(use-package arduino-mode
  :mode "\\.ino\\'"
  :config
  (use-package company-arduino
	:config
	(company-arduino-turn-on))

  (use-package flycheck-arduino :ensure arduino-mode
	:after flycheck
	:config
	(flycheck-arduino-setup)))

(use-package expand-region
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


(use-package web-mode
  :mode ("\\.html\\'" "\\.php\\'")
  :init
  (setq web-mode-enable-current-element-highlight t
		web-mode-enable-block-face t
		web-mode-enable-auto-expanding t
		web-mode-enable-current-column-highlight t
		web-mode-comment-style 2)
  :config

  (use-package company-web
	:config
	(add-to-list (make-local-variable 'company-backends) '(company-web-html)))

  (setq web-mode-engines-alist
		'(("php"	. "\\.phtml\\'")))

  (use-package web-mode-edit-element))

(use-package nginx-mode
  :commands (nginx-mode)
  :config
  (use-package company-nginx
	:hook nginx-mode))

(use-package json-mode
  :mode "\\.json\\'")

(use-package sudo-edit
  :commands sudo-edit)

(use-package evil
  :commands evil-mode
  :init
  (setq evil-esc-delay 0.001
		evil-want-keybinding nil)
  :config
  (setq show-paren-when-point-inside-paren t)	 ;; show parent even when over)

  (use-package evil-collection
	:config
	(evil-collection-init))

  ;; (use-package evil-leader
  ;; 	:config
  ;; 	(global-evil-leader-mode)
  ;; 	(evil-leader/set-key "e" 'find-file
  ;; 						 "b" 'switch-to-buffer
  ;; 						 "k" 'kill-buffer))
  )

(provide 'init)
;;; init.el ends here
