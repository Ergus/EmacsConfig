;;; init.el --- Emacs Initialization and Configuration
;; Copyright (C) 2018-2019 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;; Code:

;;__________________________________________________________
;; For using Melpa and Elpa

;; Measure time from one file to the other

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(setq package-quickstart t)

;;__________________________________________________________
;; Internal options

(setq-default auto-revert-verbose nil)	;; not show message when file changes
(global-auto-revert-mode t)		;; Autoload files changed in disk

(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)		;; Use font-lock everywhere.

;; (setq-default display-line-numbers-widen t)	;; keep line numbers inside a narrow
(global-display-line-numbers-mode t)	;; line numbers on the left

(global-display-fill-column-indicator-mode t)

(savehist-mode t)			;; Historial
(auto-compression-mode t)		;; Uncompress on the fly

(size-indication-mode t)		;; Muestra el el tamanno en modeline
(delete-selection-mode t)		;; Sobreescribe seleccion al pegar

(prefer-coding-system 'utf-8)	        ;; Encoding
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(column-number-mode t)			;; Numero de la columna
(line-number-mode t)			;; Numero de linea modeline

(save-place-mode 1)                     ;; Remember point in files

(setq-default vc-follow-symlinks t	    ;; Open links not open
	      ;;tab-always-indent complete  ;; make tab key do indent only
	      initial-scratch-message ";; Welcome Jimmy!!"
	      ring-bell-function #'ignore
	      user-full-name "Jimmy Aguilar Mena"
	      inhibit-startup-message t
	      inhibit-startup-screen t
	      ;;tab-width 4		    ;; Tabulador a 4
	      ;;indent-tabs-mode t	    ;; Indent with tabs
	      ;;fill-column 80		    ;; default is 70
	      make-backup-files nil	    ;; Sin copias de seguridad
	      create-lockfiles nil	    ;; No lock files, good for tramp
	      visible-bell nil		    ;; Flash the screen (def)
	      display-line-numbers-width 4  ;; Minimum line number width
	      confirm-kill-processes nil    ;; no ask kill processes on exit
	      read-key-delay 0.01
	      recenter-redisplay nil
	      ;;recenter-positions '(top middle bottom)
	      line-move-visual nil
	      backward-delete-char-untabify-method nil ;; Don't untabify on backward delete

	      ;; split-width-threshold 160  ;; Limite para split vertical
	      ;; kill-whole-line t
	      ;; load-prefer-newer t
	      ;; mark-even-if-inactive nil	    ;; no mark no region
	      next-screen-context-lines 5           ;; Lines of continuity when scrolling
	      fast-but-imprecise-scrolling t
	      scroll-error-top-bottom t	    ;; Move cursor before error scroll
	      scroll-preserve-screen-position t	  ;; Cursor keeps screen pos
	      scroll-margin 1		    ;; Margen al borde
	      scroll-step 1		    ;; Scroll step (better conservatively)
	      scroll-conservatively 1000
	      window-combination-resize t   ;; Windows resize proportional
	      x-wait-for-event-timeout nil  ;; Espera por eventos en X
	      jit-lock-stealth-load 60
	      jit-lock-stealth-time 4
	      inhibit-default-init t	    ;; Avoid emacs default init
	      term-suppress-hard-newline t  ;; Text can resize
	      echo-keystrokes 0.01	    ;; Muestra binds in echo area
	      confirm-kill-emacs nil        ;; No confirm exit emacs
	      disabled-command-function nil
	      auto-save-default nil         ;; No autosave
	      auto-save-list-file-name nil
	      ;; minibuffer interaction
	      enable-recursive-minibuffers t
	      minibuffer-message-timeout 1
	      read-quoted-char-radix 16     ;; Read number of chars with C-q
	      kill-buffer-query-functions nil
	      kill-do-not-save-duplicates t

	      eval-expression-print-length nil
	      eval-expression-print-level nil
	      enable-remote-dir-locals t    ;; Open remote dir locals.
	      )

(minibuffer-depth-indicate-mode 1)
;;__________________________________________________________
;; I don't want confirm exit, not write yes-not either
(defalias 'yes-or-no-p 'y-or-n-p) ;; Reemplazar "yes" por "y" en el prompt

;;__________________________________________________________
;; Config file not here to not track it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file)

;; Personal Lisp dir
(defvar mylisp-dir (expand-file-name "lisp" user-emacs-directory))

(unless (file-exists-p mylisp-dir)
  (make-directory mylisp-dir)
  (message "Creating %s" mylisp-dir))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; System Lisp dir
(defvar syslisp-dir "/usr/share/emacs/site-lisp")

(when (file-exists-p syslisp-dir)
  (add-to-list 'load-path syslisp-dir))

;; Next file is in my lisp directory. it only defines mu4e config and
;; a variable for the gmail calendar.
(unless (require 'configmail "configmail.el" t)
  (message "No mail config file found: ignored"))

;;__________________________________________________________
;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
	use-package-enable-imenu-support t)

  (require 'use-package)

   (if init-file-debug
      (progn
	(setq use-package-verbose t
	      use-package-expand-minimally nil
	      use-package-compute-statistics t
	      debug-on-error t)
	(use-package benchmark-init
	  :config
	  (add-hook 'window-setup-hook 'benchmark-init/deactivate t)))

    (setq use-package-verbose nil
	  use-package-expand-minimally t)))

(use-package use-package-hydra)

;;__________________________________________________________
;; Benchmark-init

;; (use-package ergoemacs-mode
;;   :init
;;   (setq ergoemacs-theme nil)
;;   (setq ergoemacs-keyboard-layout "us")
;;   (ergoemacs-mode 1))

(use-package diminish)		      ;; if you use :diminish
(use-package bind-key)		      ;; if you use any :bind variant

(use-package paradox
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config
  (setq paradox-spinner-type 'progress-bar
	paradox-display-download-count t
	paradox-display-star-count t))
;;__________________________________________________________
;; Isearch

(use-package isearch :ensure nil
  :defer t
  :custom
  (search-nonincremental-instead nil) ;; No incremental if enter with empty
  (lazy-highlight-initial-delay 0)
  (isearch-allow-scroll t)	      ;; Permit scroll can be 'unlimited
  (isearch-lazy-count t)
  ;;(search-exit-option 'edit)          ;; Control or meta keys edit search
  (isearch-yank-on-move 'shift)       ;; Copy text from buffer with meta
  )

(use-package phi-search
  :defer t)

(use-package phi-search-mc
  :after multiple-cursors
  :config
  (phi-search-mc/setup-keys)
  (add-hook 'isearch-mode-mode #'phi-search-from-isearch-mc/setup-keys))

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

       (set-face-attribute 'default nil :family "Hack" :height 105)

       (set-background-color (alist-get 'black my/colors))
       (set-foreground-color (alist-get 'white my/colors))

       (set-face-foreground 'font-lock-preprocessor-face (alist-get 'magenta my/colors))	;; Preprocessor
       (set-face-foreground 'font-lock-comment-face (alist-get 'cyan my/colors))		;; Comentarios
       (set-face-foreground 'font-lock-doc-face (alist-get 'brightcyan my/colors))		;; Documentation

       (set-face-foreground 'font-lock-string-face (alist-get 'red my/colors))		        ;; Strings
       (set-face-foreground 'font-lock-function-name-face (alist-get 'white my/colors))	        ;; Funciones
       (set-face-foreground 'font-lock-variable-name-face (alist-get 'white my/colors))	;; Variables
       (set-face-foreground 'font-lock-constant-face (alist-get 'magenta my/colors))		;; Constates y Clases

       (set-face-foreground 'font-lock-type-face (alist-get 'green my/colors))		        ;; Tipos (int, float)
       (set-face-foreground 'font-lock-keyword-face (alist-get 'yellow my/colors))		;; Keywords (for, if)
       (set-face-foreground 'font-lock-builtin-face (alist-get 'green my/colors))		;; Keywords (for, if)

       (set-face-attribute 'highlight nil :background (alist-get 'brightblack my/colors) :foreground nil)
       (set-face-attribute 'secondary-selection nil :background (alist-get 'brightblue my/colors))

       ;; search C-s, resalta lo que encuentra
       (set-face-attribute 'isearch nil :background (alist-get 'blue my/colors)
			   :foreground (alist-get 'white my/colors) :weight 'ultrabold)	;; Search

       (set-face-attribute 'lazy-highlight nil :background (alist-get 'brightblue my/colors))

       (set-face-attribute 'region nil :background (alist-get 'brightblue my/colors))          ;; Seleccion

       (set-face-attribute 'mode-line-inactive nil :background (alist-get 'brightblack my/colors)
			   :foreground (alist-get 'white my/colors))

       (set-face-attribute 'mode-line nil :background (alist-get 'blue my/colors)
			   :foreground (alist-get 'white my/colors))

       (set-face-attribute 'line-number nil :foreground (alist-get 'brightblack my/colors))	      ;; numero de linea
       (set-face-attribute 'line-number-current-line nil :foreground (alist-get 'green my/colors))  ;; resalta la linea actual
       (set-face-attribute 'fill-column-indicator nil :foreground (alist-get 'brightblack my/colors))

       (set-face-attribute 'tab-bar nil
			   :background (alist-get 'black my/colors) :foreground (alist-get 'white my/colors)
			   :inverse-video nil)

       (set-face-attribute 'tab-bar-tab nil :weight 'ultra-bold :underline t)

       (set-face-attribute 'tab-bar-tab-inactive nil
			   :background (alist-get 'black my/colors) :foreground (alist-get 'brightwhite my/colors)
			   :weight 'normal :underline nil)
       )

(my/colors)

;;__________________________________________________________
;;Packages options
;;__________________________________________________________

;;__________________________________________________________
;; Show paren mode
(setq-default show-paren-delay 0
	      blink-matching-paren nil)
(show-paren-mode t)	  ;; Highlight couple parentesis
(set-face-attribute 'show-paren-match nil :inherit nil
		    :background (alist-get 'brightblue my/colors))

;;__________________________________________________________
;; ssh
(use-package tramp :ensure nil
  :defer t
  :config
  (setq-default compilation-scroll-output 'first-error
		tramp-auto-save-directory "~/.emacs.d/tramp-autosave-dir"
		tramp-default-method "rsync"
		;;tramp-default-method "ssh"
		;;tramp-change-syntax 'simplified
		tramp-use-ssh-controlmaster-options nil
		tramp-completion-reread-directory-timeout t
		tramp-persistency-file-name "~/.emacs.d/tramp")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package tramp-term
  :after tramp
  :commands tramp-term)

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
	 ("/sshd?_config\\'" . ssh-config-mode)
	 ("/known_hosts\\'" . ssh-known-hosts-mode)
	 ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))


(defun my/term-mode-hook () "My term mode hook."
       (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
       (setq-local mouse-yank-at-point t)
       (setq-local transient-mark-mode nil)
       (display-line-numbers-mode -1)
       (display-fill-column-indicator-mode -1)
       (auto-fill-mode -1))

(add-hook 'term-mode-hook 'my/term-mode-hook)

;;__________________________________________________________
;; minibuffers

;; (setq minibuffer-eldef-shorten-default t)

(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000)
  (garbage-collect))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

;;__________________________________________________________
;; gdb rectangles

(use-package gdb :ensure nil
  :commands gdb
  :init
  (setq gdb-many-windows nil
	gdb-show-main t))

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
	     vdiff-buffers3)
  :bind (:map vdiff-mode-map
	      ("C-c d v" . vdiff-hydra/body))
  :config
  (setq vdiff-auto-refine t)
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))

(use-package man :ensure nil
  :commands man
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

;;__________________________________________________________
;; Diminish To Hide Packages from bar
(use-package diminish)

;;__________________________________________________________
;; Mocp and multi-term music player
(use-package multi-term
  :bind (("C-c 4 t" . multi-term-dedicated-open)
	 ("C-c 5 t" . multi-term)
	 ("C-c t t" . multi-term-dedicated-toggle)
	 ("C-c t k" . multi-term-dedicated-close))
  :config
  (setq multi-term-dedicated-window-height 24
	;;multi-term-program "/bin/bash"
	multi-term-program-switches "--login"
	multi-term-dedicated-select-after-open-p t))

;;__________________________________________________________
;; Better shell (for ssh)
(use-package better-shell
  :bind ("C-c t b" . better-shell-shell))

(use-package bang
  :bind ("M-!" . bang))

;;__________________________________________________________
;; which-key

(use-package which-key
;;  :defer 1
  :bind (("C-h b" . which-key-show-top-level)
         ("C-h m" . which-key-show-major-mode))
  :diminish
  :custom
  ;;(which-key-idle-delay 0.4)
  (which-key-use-C-h-commands nil)
  ;;(which-key-echo-keystrokes echo-keystrokes)
  (which-key-separator ": ") ;which-key-idle-delay 2.0)
  :config
  (which-key-mode t)
  (which-key-add-key-based-replacements
    "C-c h" "highlight"
    "C-c s" "sidebars"
    "C-x r" "rectangle||register"
    "C-x n" "narrow"
    "C-x n" "tabs"
    "C-x a" "abbrev"))

(use-package fancy-narrow
  :bind (("C-x n N" . fancy-narrow-to-region)
	 ("C-x n D" . fancy-narrow-to-defun)
	 ("C-x n P" . fancy-narrow-to-page)
	 ("C-x n W" . fancy-widen)))

;;__________________________________________________________
;; Clipboard copy and paste with: M-w & C-c v

;; (use-package clipetty
;;   :unless (display-graphic-p)
;;   :hook (after-init . global-clipetty-mode))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode 1))

;; (use-package whole-line-or-region
;;   :config
;;   (whole-line-or-region-global-mode 1))

;;__________________________________________________________
;;	Seleccionar con el mouse
(use-package mouse :ensure nil
  :unless (display-graphic-p)
  :config
  (xterm-mouse-mode t)			  ;; mover el cursor al click
  (defun track-mouse (e))
  (setq-default mouse-sel-mode t ;; Mouse selection
		mouse-scroll-delay 0
		mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control)))
		mouse-wheel-progressive-speed nil
		)
  (set-cursor-color "white")
  (set-mouse-color "white")		  ;; Flechita del mouse en blanco
  (mouse-wheel-mode t)			  ;; scrolling con el mouse
  )

(global-set-key [drag-mouse-2] 'mouse-yank-at-click)

(defun my/scroll-up-command (&optional arg)
  (interactive "^P")
  (if arg
      (scroll-up-command arg)
    (scroll-up-command 1)))

(defun my/scroll-down-command (&optional arg)
  (interactive "^P")
  (if arg
      (scroll-down-command arg)
    (scroll-down-command 1)))

(global-set-key [remap scroll-up-command] 'my/scroll-up-command)
(global-set-key [remap scroll-down-command] 'my/scroll-down-command)
;;__________________________________________________________
;; My program's mode hooks

;; (use-package whitespace-mode :ensure nil
;;   :preface
;;   (defun my/whitespace-mode () "My whitespace mode."
;; 	 (setq whitespace-style '(face tabs tab-mark trailing)
;; 	       whitespace-display-mappings	'((tab-mark 9 [?\u2502 9] [?\u2502 9])))
;; 	 (custom-set-faces '(whitespace-tab ((t (:foreground "#444444")))))
;; 	 (whitespace-mode 1))
;;   :hook (prog-mode . my/whitespace-mode)
;;   )

;; (use-package clean-aindent-mode
;;   :hook prog-mode
;;   :bind ("RET" . newline-and-indent)
;;   :config
;;   (clean-aindent-mode t)
;;   (setq clean-aindent-is-simple-indent t))

(defun my/prog-mode-hook () "Some hooks only for prog mode."
       ;;(electric-indent-mode t)	    ;; On by default
       (electric-pair-mode t)			  ;; Autoannadir parentesis
       (which-function-mode t)			  ;; Shows the function in spaceline

       ;;(define-key global-map (kbd "RET") 'newline-and-indent)
       (electric-indent-local-mode t)
       (setq show-trailing-whitespace t)

       (defun smart-beginning-of-line ()
	 "Move point to first non-whitespace character or beginning-of-line."
	 (interactive)
	 (let ((oldpos (point)))
	   (back-to-indentation)
	   (and (= oldpos (point))
		(beginning-of-line))))

       (global-set-key (kbd "C-a") 'smart-beginning-of-line))

(add-hook 'prog-mode-hook #'my/prog-mode-hook)

;;__________________________________________________________
;; 80 Column rules
;; (use-package fill-column-indicator
;;   :hook (prog-mode . fci-mode)
;;   :bind ("C-c h f" . fci-mode)
;;   :config
;;   (setq fci-rule-color "#7f7f7f7f7f7f"
;;		fci-rule-character ?\u2502))

;;__________________________________________________________
;; Undo tree

;; (use-package undo-tree
;;   :diminish
;;   :init (global-undo-tree-mode))

;; (use-package undo-propose
;;   :commands undo-propose)

;; Saner undo/redo
(use-package undo-fu
  :custom
  (undo-fu-allow-undo-in-region t)
  :bind (([remap undo] . undo-fu-only-undo)
         ("C-M-_" . undo-fu-only-redo)))

;;__________________________________________________________
;; Mark column 80 when crossed
(use-package hl-line :ensure nil
  :diminish
  :bind ("C-c h l" . hl-line-mode))

;;__________________________________________________________
;; Mark column 80 when crossed
(use-package column-enforce-mode
  :diminish
  :bind ("C-c h c" . column-enforce-mode)
  :config
  ;;(column-enforce-mode t)
  (setq column-enforce-comments nil)
  (set-face-attribute 'column-enforce-face nil
		      :inherit nil :background (alist-get 'brightblack my/colors)))

(use-package highlight-indent-guides
  :disabled
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :bind ("C-c h i" . highlight-indent-guides-mode)
  :commands (highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-enabled nil
	highlight-indent-guides-method 'character)
  (set-face-attribute 'highlight-indent-guides-character-face nil
		      :foreground (alist-get 'brightblack my/colors)))

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
  :disabled
  :diminish
  :hook (prog-mode . hes-mode)
  :config
  (set-face-attribute 'hes-escape-backslash-face nil :foreground (alist-get 'magenta my/colors))
  (set-face-attribute 'hes-escape-sequence-face nil :foreground (alist-get 'magenta my/colors)))

(use-package highlight-numbers
  :diminish
  :hook (prog-mode . highlight-numbers-mode)
  :bind ("C-c h n" . highlight-numbers-mode)
  :config
  (set-face-attribute 'highlight-numbers-number nil :foreground (alist-get 'red my/colors)))

;;__________________________________________________________
;; Flyspell (Orthography)
(use-package flyspell :ensure nil
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode))
  :bind (:map flyspell-mode-map
	      ("C-M-i" .  nil)
	      ("C-'" .	nil)
	      ("C-;" .	nil)
	      ("C-," .	nil)
	      ("C-." .	nil)
	      ("C-c $" .  nil)
	      ("C-c f r" . flyspell-region)
	      ("C-c f b" . flyspell-buffer)
	      ("C-c f n" . flyspell-goto-next-error)
	      )
  :config
  (which-key-add-key-based-replacements "C-c f" "flyspell"))

(use-package flyspell-correct-ivy
  :diminish
  :after flyspell
  :bind (("C-c f w" . flyspell-correct-wrapper)
	 ("C-c f f" . flyspell-correct-at-point)
	 ("C-c f C-n" . flyspell-correct-next)
	 ("C-c f C-p" . flyspell-correct-previous))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;;__________________________________________________________
;; {c/c++}-mode
;;__________________________________________________________

;;__________________________________________________________
;; ycmd Mode

;; (use-package ycmd
;;   :disabled
;;   :hook ((c-mode . ycmd-mode)
;; 	 (c++-mode . ycmd-mode))
;;   :config
;;   (setq ycmd-server-command '("python" "/home/ergo/gits/ycmd/ycmd"))
;;   (setq ycmd-global-config "/home/ergo/gits/ycmd/.ycm_extra_conf.py")

;;   (use-package company-ycmd
;;     :config
;;     (company-ycmd-setup))

;;   (use-package flycheck-ycmd
;;     :config
;;     (flycheck-ycmd-setup))

;;   (use-package ycmd-eldoc
;;     :config
;;     (ycmd-eldoc-setup)))

;;__________________________________________________________
;; LSP try for a while

(use-package lsp-mode
  :diminish lsp
  ;; :hook ((c-mode . lsp-deferred)
  ;; 	 (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-snippet nil)
  (lsp-eldoc-hook nil)
  )

(use-package lsp-ui
  :diminish
  :after lsp
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

(use-package company-lsp
  :diminish
  :after company lsp
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package lsp-treemacs
  :diminish
  :after lsp company
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

(use-package lsp-ivy
  :diminish
  :after lsp ivy)

;;__________________________________________________________
;; Irony config (C completions)

;; (defun my/irony-mode-hook () "My irony-mode Hook.

;; This is in the hook for c-common mode.	If the file is remote it loads
;; company-c-headers instead if irony"
;;        (use-package irony
;; 	 :diminish
;; 	 :config
;; 	 (irony-mode)
;; 	 (irony-cdb-autosetup-compile-options)

;; 	 (use-package company-irony
;; 	   :config
;; 	   (use-package company-irony-c-headers)

;; 	   (add-to-list (make-local-variable 'company-backends)
;; 			'(company-irony-c-headers company-irony)))

;; 	 (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
;; 	 (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)

;; 	 (use-package flycheck-irony
;; 	   :after flycheck
;; 	   :config
;; 	   (flycheck-irony-setup))

;; 	 (use-package irony-eldoc
;; 	   :if eldoc-mode
;; 	   :config
;; 	   (irony-eldoc))))

;;__________________________________________________________
;; C common mode (for all c-like languajes)

;;====================

(defvar c-ms-space-for-alignment t
  "Control ms-space-for-alignment.")
(make-variable-buffer-local 'c-ms-space-for-alignment)

(defun ms-space-for-alignment ()
  "Make the current line use tabs for indentation and spaces for alignment.

It is intended to be called from the hook
`c-special-indent-hook'.  It assumes that `indent-tabs-mode' is
non-nil and probably assumes that `c-basic-offset' is the same as
`tab-width'."
  (save-excursion
    (let* ((indent-pos (progn (back-to-indentation) (point)))
	   (indent-col (current-column))
	   (syn-elt (car c-syntactic-context))
	   (syn-sym (c-langelem-sym syn-elt)))
      (when (memq syn-sym '(arglist-cont-nonempty
			    stream-op
			    template-args-cont)) ;; <==============
	(let* ((syn-anchor (c-langelem-pos syn-elt))
	       (anchor-col (progn (goto-char syn-anchor)
				  (back-to-indentation)
				  (current-column))))
	  ;;
	  (goto-char indent-pos)
	  (delete-horizontal-space)
	  (insert-char ?\t (/ anchor-col tab-width))
	  (insert-char ?\  (- indent-col (current-column)))))))
  (when (= (current-column) 0)
    (back-to-indentation))
  )

(defun c-toggle-ms-space-for-alignment (&optional arg)
  "Toggle align with spaces."
  (interactive "P")
  (setq c-ms-space-for-alignment
	(c-calculate-state arg c-ms-space-for-alignment))
  (if c-ms-space-for-alignment
      (when (and c-ms-space-for-alignment
		 indent-tabs-mode
		 (= c-basic-offset tab-width))
	(add-hook 'c-special-indent-hook #'ms-space-for-alignment nil t))
    (remove-hook 'c-special-indent-hook #'ms-space-for-alignment t)))

;;====================

(c-add-style "mylinux"
	     '("linux"
	       (tab-width . 4)
	       (c-basic-offset . 4)
	       (indent-tabs-mode . t)
	       (fill-column . 80)
	       (c-offsets-alist (inline-open . 0)
				(comment-intro . 0)
				;;(innamespace . [0])
				;;(access-label '-)
				)))

(setq-default c-default-style
	      '((java-mode . "java")
		(awk-mode . "awk")
		(other . "mylinux")))

(defun my/c-mode-common-hook () "My hook for C and C++."
       (c-toggle-auto-newline 1)
       (c-toggle-cpp-indent-to-body 1)
       (c-toggle-ms-space-for-alignment 1)
       (message "Loaded my/c-mode-common"))

(add-hook 'c-mode-common-hook #'my/c-mode-common-hook)

(use-package preproc-font-lock ;; Preprocessor
  :hook (c-mode . preproc-font-lock-mode)
  :config
  (set-face-attribute 'preproc-font-lock-preprocessor-background nil
		      :inherit 'font-lock-preprocessor-face))

;; company-c-headers
(use-package company-c-headers
  :after company
  :when (and (member major-mode '(c++-mode c-mode arduino-mode))
	     buffer-file-name
	     (string-match-p tramp-file-name-regexp buffer-file-name))
  :config
  (add-to-list (make-local-variable 'company-backends) 'company-c-headers))

(use-package clang-format
  :commands clang-format-region)

;;__________________________________________________________
;; C++ mode
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))


;;__________________________________________________________
;; sh mode

(defvaralias 'sh-basic-offset 'tab-width)
(defun my/sh-mode-hook () "My term mode hook."
       (setq-local indent-tabs-mode t))

(add-hook 'sh-mode-hook 'my/sh-mode-hook)

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
	 ("\\.markdown\\'" . markdown-mode)))

;;__________________________________________________________
;; Restructured text
(use-package rst-mode :ensure nil
  :mode "\\.rst\\'")

(use-package sphinx-mode
    :hook rst-mode)

;;__________________________________________________________
;; Makefile
(use-package makefile-mode :ensure nil
  :mode (".*Makefile.*" "\\.mak"))

;;__________________________________________________________
;; ruby-mode
(use-package ruby-mode :ensure nil
  :mode ("\\.rb\\'" "\\.rjs\\'" "\\Rakefile\\'" "\\Gemfile\\'")
  :config
  (setq-default ruby-indent-level 2))

(use-package ruby-tools
  :hook (ruby-mode . ruby-tools-mode))

(use-package ruby-electric
  :hook (ruby-mode . ruby-electric-mode))

;;__________________________________________________________
;; Julia Mode
(use-package julia-mode
  :mode "\\.jl\\'")

(use-package flycheck-julia
  :after flycheck
  :hook (julia-mode . flycheck-julia-setup))

;;__________________________________________________________
;; Rust Mode
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package flycheck-rust
  :after flycheck
  :hook (rust-mode . flycheck-rust-setup))

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
  :interpreter "lua")

(use-package company-lua
    :after company
    :preface
    (defun my/company-lua ()
      (add-to-list (make-local-variable 'company-backends) 'company-lua))
    :hook (lua-mode . my/company-lua))

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
(use-package js-mode :ensure nil
  :mode ("\\.js\\'"))

;;__________________________________________________________
;; xml-mode
(use-package xml-mode :ensure nil
  :mode ("\\.ipe\\'" "\\.qrc\\'" "\\.svn\\'"))

;;__________________________________________________________
;; Completion
(add-to-list 'completion-styles 'flex)
;; '(completion-ignored-extensions
;;   (quote ("CVS/" ".o" "~" ".bin" ".lbin" ".fasl" ".ufsl" ".a" ".ln" ".blg" ".bbl"
;; 	  ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".class" ".fas"
;; 	  ".lib" ".x86f" ".sparcf" ".lo" ".la" ".toc" ".log" ".aux" ".cp" ".fn"
;; 	  ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".lbl"
;; 	  ".out" ".brf" ".ncb" ".sln" ".suo" ".vcproj.AD.ghali.user" ".idb" ".pdb"
;; 	  ".synctex.gz" ".svn")))

;;__________________________________________________________
;; splitting

;; Move split keybindings
;; (use-package windmove :ensure nil
;;   :bind (("C-x <left>" . windmove-left)
;; 	 ("C-x <right>" . windmove-right)
;; 	 ("C-x <up>" . windmove-up)
;; 	 ("C-x <down>" . windmove-down))
;;   :init
;;   (which-key-add-key-based-replacements "C-x w" "windmove winner")
;;   ;;:config
;;   ;;(windmove-default-keybindings 'meta)  ;; Move between panes S-arrow
;;   ;;(setq windmove-wrap-around t)		  ;; Cyclic bound mode
;;   )

(use-package ace-window
  :bind ([remap other-window] . ace-window)
  :custom
  (aw-background nil)
  (aw-ignore-current t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Undo redo split
(use-package winner-mode :ensure nil
  :defer 5
  :bind (("C-x w u" . winner-undo)
	 ("C-x w r" . winner-redo))
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode t))

;; winum (windows number) for spaceline
;; (use-package winum
;;   :bind (("C-x w 1" . winum-select-window-1)
;;	 ("C-x w 2" . winum-select-window-2)
;;	 ("C-x w 3" . winum-select-window-3)
;;	 ("C-x w 4" . winum-select-window-4)
;;	 ("C-x w 5" . winum-select-window-5)
;;	 ("C-x w 6" . winum-select-window-6)
;;	 ("C-x w 7" . winum-select-window-7)
;;	 ("C-x w 8" . winum-select-window-8))
;;   :init
;;   (winum-mode))

;;__________________________________________________________
;; Lines enabling gnuplot-mode
(use-package gnuplot-mode
  :mode ("\\.gp\\'" "\\.gpl\\'" "\\.plt\\'"))

;;__________________________________________________________
;; Auto completamiento
(use-package company
  :diminish
  :bind (:map company-active-map
	      ("C-n" . company-select-next-or-abort)
	      ("C-p" . company-select-previous-or-abort))
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 1.0)	 ;; no delay for autocomplete
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around nil)
  (company-show-numbers t)
  ;;company-tooltip-limit 20
  ;;company-show-numbers t
  (company-backends '(company-semantic
		     company-capf		 ;; completion at point
		     company-files	 ;; company files
		     (company-dabbrev-code company-gtags company-keywords)
		     company-dabbrev))
  :config
  (company-tng-configure-default)
  (define-key company-active-map (kbd "<C-return>") 'company-other-backend)
  (define-key company-mode-map (kbd "<C-return>") 'company-other-backend)
  (set-face-attribute 'company-tooltip nil		  ;; dialog face
		      :background (alist-get 'brightblack my/colors)
		      :foreground (alist-get 'white my/colors))
  (set-face-attribute 'company-tooltip-common nil ;; common part face
		      :inherit 'company-tooltip :foreground (alist-get 'green my/colors))
  (set-face-attribute 'company-tooltip-selection nil ;; selection face
		      :background (alist-get 'blue my/colors) :weight 'ultra-bold)
  (set-face-attribute 'company-scrollbar-bg nil	  ;; scroll bar face bg
		      :background (alist-get 'brightblack my/colors))
  (set-face-attribute 'company-scrollbar-fg nil	  ;; scroll bar face fg
		      :background (alist-get 'blue my/colors)))

(use-package yasnippet                  ; Snippets
  :diminish
  :bind (("C-c y i" . yas-insert-snippet)
	 ("C-c y n" . yas-new-snippet)
	 ("C-c y f" . yas-visit-snippet-file)
	 ("C-c y TAB" . yas-expand)
	 :map yas-minor-mode-map
	 ("TAB" . nil))
  :init
  (which-key-add-key-based-replacements "C-c y" "yasnippet")
  :config
  (setq yas-verbosity 1                 ; No need to be so verbose
	yas-wrap-around-region t)

  ;; (yas-reload-all)
  ;; (yas-minor-mode 1)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

;; __________________________________________________________
;; Emacs lisp

(defun my/elisp-mode-hook ()
  "My elisp mode hook."
  (with-eval-after-load "company"
    (add-to-list 'company-backends 'company-elisp)))

(add-hook 'emacs-lisp-mode-hook 'my/elisp-mode-hook)

;;__________________________________________________________
;; Chequeo de syntaxis
;; (use-package flycheck
;;   :diminish
;;   :if (< (buffer-size) 200000)
;;   :hook (prog-mode . flycheck-mode)
;;   :config
;;   (setq flycheck-gcc-language-standard "c++17"
;; 	flycheck-clang-language-standard "c++17"
;; 	flycheck-display-errors-delay 1.0)
;;   (which-key-add-key-based-replacements "C-c !" "flycheck"))

(use-package flymake-mode :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 1.0))

;;__________________________________________________________
;; Function arguments show

(use-package eldoc :ensure nil
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . eldoc-mode)
  :config
  (eldoc-mode t))

(use-package helpful
  :bind (("<f1> e" . nil)
	 ("<f1> e f" . helpful-function)
	 ("<f1> e c" . helpful-command)
	 ("<f1> e m" . helpful-macro)
	 ("<f1> e l" . helpful-callable)
	 ("<f1> e k" . helpful-key)
	 ("<f1> e p" . helpful-at-point)
	 ("<f1> e v" . helpful-variable)))

;;__________________________________________________________
;; Chequeo de gramatica
(use-package langtool
  :defer t
  :config
  (setq langtool-default-language "en")
  (setq langtool-language-tool-jar "~/gits/languagetool/languagetool-standalone/target/LanguageTool-4.6-SNAPSHOT/LanguageTool-4.6-SNAPSHOT/languagetool-commandline.jar"))

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
  :diminish)

;; Asocia buffers que empiecen con messaje mode
(use-package message-mode :ensure nil
  :mode ("/neomut" "neomutt-Ergus-" "draft")
  :init
  (setq mail-header-separator "")
  :config
  (auto-fill-mode t)
  (mail-abbrevs-setup)
  (flyspell-mode t))

(use-package notmuch
  :preface
  (defun my/notmuch ()
    (require 'notmuch-address)
    (setq notmuch-address-command "~/gits/notmuch-addrlookup-c/notmuch-addrlookup")
    (require 'notmuch-company)
    (add-to-list (make-local-variable 'company-backends) 'notmuch-company))
  :init
  (setenv "NOTMUCH_CONFIG" "/home/ergo/almacen/mail/notmuch-config")
  :hook (message-mode . my/notmuch))

;;__________________________________________________________
;; Latex mode

(use-package tex :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq TeX-source-correlate-start-server t
	TeX-auto-save t
	TeX-parse-self t)
  :config
  (setq LaTeX-babel-hyphen nil)

  (TeX-source-correlate-mode t)
  (setq-default TeX-master nil) ;; Multidocument

  (flyspell-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 1)

  (add-to-list 'TeX-command-list
  	       '("Makeglossaries" "makeglossaries %s" TeX-run-command nil
  		 (latex-mode)
  		 :help "Run makeglossaries, will choose xindy or makeindex") t)
  (flyspell-buffer))

(use-package company-math
  :after (company tex)
  :config
  (add-to-list 'company-backends
	       '(company-math-symbols-latex company-latex-commands)))

(use-package company-auctex
  :after (company-math tex)
  :config
  (add-to-list 'company-backends 'company-auctex-labels)
  (add-to-list 'company-backends 'company-auctex-bibs)
  (add-to-list 'company-backends
	       '(company-auctex-macros company-auctex-symbols company-auctex-environments)))

(use-package reftex :ensure nil ;; Reftex for cross references
  :after tex
  :config
  (reftex-isearch-minor-mode)
  (setq reftex-plug-into-AUCTeX t
	reftex-cite-prompt-optional-args t   ; Prompt for empty optional arguments in cite
	reftex-cite-format 'biblatex
	reftex-plug-into-AUCTeX t
	reftex-insert-label-flags '(t t)
	reftex-save-parse-info t
	reftex-enable-partial-scans t
	reftex-use-multiple-selection-buffers t))


(use-package company-reftex
  :after (company reftex)
  :config
  (add-to-list 'company-backends
	       '(company-reftex-labels company-reftex-citations)))

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

(use-package python-mode :ensure nil
  :mode "\\.py\\'"
  :interpreter "python"
  :bind (:map python-mode-map
              ("C-c C-z" . python-shell))
  :init
  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
  )

;; (use-package company-jedi		 ;;; company-mode completion back-end for Python JEDI
;;   :hook (python-mode . jedi:setup)
;;   :custom
;;   (jedi:server-args '("--sys-path" "/usr/lib/python3.7/site-packages"))
;;   :config
;;   (add-to-list (make-local-variable 'company-backends) 'company-jedi))

;; (use-package flycheck-pycheckers
;;   :after (flycheck company-jedi)
;;   :hook (python-mode . flycheck-pycheckers-setup)
;;   :init
;;   (setq flycheck-pycheckers-checkers '(pylint flake8 mypy3)))

;; (use-package elpy
;;   :defer
;;   :init
;;   (defun enable-elpy-once ()
;;     (elpy-enable)
;;     (advice-remove 'python-mode 'enable-elpy-once))
;;   (advice-add 'python-mode :before 'enable-elpy-once)
;;   :config
;;   (setq python-shell-interpreter "jupyter"
;; 	python-shell-interpreter-args "console --simple-prompt"
;; 	python-shell-prompt-detect-failure-warning nil
;; 	elpy-rpc-python-command "python3"
;; 	python-check-command "pyflakes"
;; 	flycheck-python-flake8-executable "flake8"
;; 	)
;;   (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))

(use-package ein)

;;__________________________________________________________
;; Dired-mode settings (file manager)
(use-package dired :ensure nil
  :commands dired
  :bind (("C-x d" . dired)
	 :map dired-mode-map
	 ("RET" . dired-find-alternate-file)
	 ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :custom
  (dired-recursive-copies 'top)	     ;; Always ask recursive copy
  (dired-recursive-deletes 'top)     ;; Always ask recursive delete
  (dired-dwim-target t)		     ;; Copy in split mode with p
  (dired-auto-revert-buffer t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (set-face-attribute 'dired-directory nil	;; Dired directory colors
		      :foreground (alist-get 'cyan my/colors))

  (require 'dired-x))

(use-package dired-sidebar
  :bind ("C-c s d" . dired-sidebar-toggle-sidebar)
  :config
  (setq ;;dired-sidebar-use-term-integration t
   dired-sidebar-theme 'nerd
   dired-sidebar-subtree-line-prefix "."))
;;__________________________________________________________
;; projectile

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :defer t
  :init
  (which-key-add-key-based-replacements "C-c p" "projectile")
  :custom
  (projectile-completion-system 'ivy)
  (projectile-file-exists-remote-cache-expire (* 10 60))
  (projectile-enable-caching nil)
  (projectile-verbose nil)
  (projectile-do-log nil)
  :config
  (projectile-mode t))

;; Uncomment the projectile section is comment this.
(use-package counsel-projectile
  :after counsel
  :config
  (counsel-projectile-mode t))

;;__________________________________________________________
;; ibuffer
(use-package ibuffer :ensure nil
  :bind ([list-buffers] . ibuffer)
  :init
  (defalias 'list-buffers 'ibuffer)) ; make ibuffer default

(use-package ibuffer-sidebar
  :bind (("C-c s b" . ibuffer-sidebar-toggle-sidebar)))

(use-package ibuffer-tramp
  :hook tramp-mode
  :config
  (defun my/ibuffer-tramp-hook () "ibuffer tram hook"
	 (ibuffer-tramp-set-filter-groups-by-tramp-connection)
	 (ibuffer-do-sort-by-alphabetic))
  (add-hook 'ibuffer-hook 'my/ibuffer-tramp-hook))

(use-package ibuffer-projectile
  :after ibuffer
  :config
  (defun my/ibuffer-projectile-hook () "My ibuffer-projectile-hook."
	 (ibuffer-projectile-set-filter-groups)
	 (unless (eq ibuffer-sorting-mode 'alphabetic)
           (ibuffer-do-sort-by-alphabetic)))
  (add-hook 'ibuffer-hook 'my/ibuffer-projectile-hook))

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

(use-package hydra :defer t)

(use-package headlong :defer t)

;;(use-package flx :defer t)

(use-package ivy
  :diminish
  :defer 0.5
  :bind (("C-c C-r" . ivy-resume)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-partial)
	 ("RET" . ivy-alt-done)
	 )
  :init
  (which-key-add-key-based-replacements "C-c i" "ivy")
  :config
  (copy-face 'highlight 'ivy-current-match)  ;; Linea seleccionada

  (set-face-attribute 'ivy-minibuffer-match-face-1 nil     ;; espacio entre matches
   		      :inherit nil :background nil :foreground nil :underline t)
  (copy-face 'lazy-highlight 'ivy-minibuffer-match-face-2)
  (copy-face 'lazy-highlight 'ivy-minibuffer-match-face-3)
  (copy-face 'lazy-highlight 'ivy-minibuffer-match-face-4)

  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	ivy-display-style 'fancy
	ivy-pulse-delay nil
	ivy-use-selectable-prompt t
	;;ivy-height 10
	;;ivy-wrap t					 ;; cycle in minibuffer
	)

  ;; Highlight with arrows by default.
  (ivy-mode t)

  (add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-arrow))
  ;; (add-to-list 'ivy-re-builders-alist '(t . ivy--regex-fuzzy))
  )

(use-package ivy-hydra
  :after (ivy hydra))

(use-package swiper
  :preface
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward)
	 :map swiper-map
	 ("C-y" . yank)
	 ("M-%" . swiper-query-replace)
	 ("C-," . swiper-avy)
	 ("C-c m" . swiper-mc)
	 ("C-r" . ivy-previous-line-or-history)
	 ("C-o" . swiper-isearch-toggle)
	 :map isearch-mode-map
	 ("C-o" . swiper-isearch-toggle))
  :config
  ;; (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper-isearch . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper-isearch-backward . ivy--regex-plus))

  (copy-face 'isearch 'swiper-isearch-current-match)
  (copy-face 'highlight 'swiper-line-face)         ;; linea minibuffer

  (set-face-attribute 'swiper-match-face-1 nil     ;; linea en minibuffer
   		      :inherit nil :background nil :underline t)

  (copy-face 'isearch 'swiper-match-face-2) ;; primer match
  (copy-face 'isearch 'swiper-match-face-3) ;; segundo match
  (copy-face 'isearch 'swiper-match-face-4) ;; tercer match

  (copy-face 'lazy-highlight 'swiper-background-match-face-2)
  (copy-face 'lazy-highlight 'swiper-background-match-face-3)
  (copy-face 'lazy-highlight 'swiper-background-match-face-4)
  )

(use-package imenu-anywhere
  :bind ("C-c i i" . ivy-imenu-anywhere)
  :init
  (setq imenu-auto-rescan t))

(use-package imenu-list
  :bind ("C-c s i" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-position 'left))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x 4 b" . counsel-switch-buffer-other-window)
	 ("C-c c a" . counsel-ag)
	 ("C-c c b" . counsel-ibuffer)
	 ("C-c c d" . counsel-dired)
	 ("C-c c i" . counsel-imenu)
	 ("C-c c j" . counsel-file-jump)
	 ("C-c c C-s" . counsel-grep)
	 ("C-c c r" . counsel-rg)	      ;; like git grep
	 ("C-c c g" . counsel-git)
	 ("C-c c <f2>" . counsel-linux-app)
	 ("C-c c G" . counsel-git-grep)
	 ("C-c c l" . counsel-locate)
	 ("C-c c L" . counsel-find-library)   ;; Search lisp libraries
	 ("C-c c C" . counsel-compile)        ;; Compile
	 ("C-c c R" . counsel-recentf)
	 ("C-c c C-r" . counsel-register)
	 :map help-map			      ;; help-map
	 ("f" . counsel-describe-function)
	 ("v" . counsel-describe-variable)
	 ("C-l" . counsel-info-lookup-symbol))
  :defer 2
  :init
  (which-key-add-key-based-replacements "C-c c" "counsel")
  :custom
  (counsel-find-file-at-point t)       ;; Select file at point
  (counsel-preselect-current-file t)   ;; Select current file in list
  :config
  (counsel-mode t)
  ;; (add-to-list 'ivy-re-builders-alist '(counsel-rg . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(counsel-ag . ivy--regex-plus))

  )

(use-package amx ;; Complete history
  :after counsel)

;; (use-package recentf :ensure nil
;;   :commands recentf-mode
;;   :after counsel
;;   :config
;;   (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
;;                           "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
;;                           ".*png$" ".*cache$"))
;;   (setq recentf-max-saved-items 600))

(use-package counsel-gtags
  :diminish
  :load-path "~/gits/emacs-counsel-gtags/"
  :bind (("C-c g g" . counsel-gtags-dwim)
	 ("C-c g d" . counsel-gtags-find-definition)
	 ("C-c g r" . counsel-gtags-find-reference)
	 ("C-c g s" . counsel-gtags-find-symbol)
	 ("C-c g p" . counsel-gtags-go-backward)
	 ("C-c g n" . counsel-gtags-go-forward)
	 ("C-c g c" . counsel-gtags-create-tags)
	 ("C-c g f" . counsel-gtags-find-file)
	 ("C-c g u" . counsel-gtags-update-tags))
  :init
  (which-key-add-key-based-replacements "C-c g" "counsel-gtags")
  :config
  (add-to-list (make-local-variable 'company-backends) 'company-gtags))


(use-package ggtags
  :diminish
  :bind (("C-c o g" . ggtags-find-tag-dwim)
	 ("C-c o d" . ggtags-find-definition)
	 ("C-c o r" . ggtags-find-reference)
	 ("C-c o l" . ggtags-view-tag-history)
	 ("C-c o p" . ggtags-prev-mark)
	 ("C-c o n" . ggtags-next-mark)
	 ("C-c o c" . ggtags-create-tags)
	 ("C-c o f" . ggtags-find-file)
	 ("C-c o u" . ggtags-update-tags))
  :init
  (which-key-add-key-based-replacements "C-c o" "counsel-etags"))

;;Counsel etags
(use-package counsel-etags
  :hook (prog-mode . (lambda ()
		       (add-hook 'after-save-hook
				 'counsel-etags-virtual-update-tags
				 'append
				 'local)))
  :bind (("C-c t d" . counsel-etags-find-tag-at-point)
	 ("C-c t p" . xref-pop-marker-stack)
	 ("C-c t g" . counsel-etags-grep-symbol-at-point)
	 ("C-c t f" . counsel-etags-find-tag))
  :init
  (which-key-add-key-based-replacements "C-c t" "counsel-etags")
  :config
  (setq tags-revert-without-query t) ;; Don't ask before rereading TAGS
  (setq large-file-warning-threshold nil)   ;; Don't warn when TAGS files are large
  )

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

;;__________________________________________________________
;; Magit

(use-package magit
  :commands (magit-status
	     magit-log-all
	     magit-log)
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package gitattributes-mode
  :mode "\\.gitattributes\\'")

(use-package gitconfig-mode
  :mode "\\.gitconfig\\'")

(use-package gitignore-mode
  :mode "\\.gitignore\\'")

;;______________________________________
;; Git commit
(use-package git-commit
  :mode ("COMMIT_EDITMSG" . git-commit-mode)
  :config
  (defun my/git-commit-setup-hook ()
    (setq-local git-commit-summary-max-length 68
		fill-column 72)
    (git-commit-turn-on-flyspell))
  (add-hook 'git-commit-setup-hook #'my/git-commit-setup-hook))

;;__________________________________________________________
;; Ensamblador nasm
(use-package nasm-mode
  :mode ("\\.asm\\'" "\\.s\\'"))

;;__________________________________________________________
;; CMake
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\(.in\)?\\'")
  :config
  (add-to-list 'company-backends 'company-cmake))

(use-package cmake-font-lock
  :preface
  (defun my/cmake-font-lock ()
    (cmake-font-lock-setup)
    (font-lock-refresh-defaults))
  :init
  (add-hook 'cmake-mode-hook #'my/cmake-font-lock t)
  ;;(add-hook 'cmake-mode-hook #'cmake-font-lock-activate)
  )

(use-package eldoc-cmake
  :after company
  :hook (cmake-mode . eldoc-cmake-enable))

;;__________________________________________________________
;; Cobol
(use-package cobol-mode
  :mode ("\\.cobc\\'" "\\.cob\\'" "\\.cbl\\'" "\\.cpy\\'"))

;;__________________________________________________________
;; path
(defun shell-command-on-buffer (start end command)
  "Execute shell COMMAND on buffer overwriting it."
  (interactive (let (string)
		 (setq string (read-shell-command "Shell command on buffer: "))
		 (if (use-region-p)
		   (list (region-beginning) (region-end) string)
		 (list (point-min) (point-max) string))))
  (save-excursion
    (shell-command-on-region start end command t t)))

;;__________________________________________________________
;;; Org Mode (I don't use it)

(use-package org :ensure nil
 :mode ("\\.org\\'" . org-mode))

;; (use-package org-bullets
;;    :hook (org-mode . org-bullets-mode))

;;__________________________________________________________
;; Move current line up and down Shift+arrow
;; (use-package move-text
;;   :bind(("C-M-<up>" . move-text-up)
;; 	("C-M-<down>" . move-text-down)
;; 	("C-M-<left>" . (lambda () (interactive) (transpose-words -1)))
;; 	("C-M-<right>" . (lambda () (interactive) (transpose-words 1)))
;; 	("M-<left>" . (lambda () (interactive) (transpose-chars -1)))
;; 	("M-<right>" . (lambda () (interactive) (transpose-chars 1)))))

(use-package move-dup
  :bind (("M-<up>" .  md-move-lines-up)
	 ("M-<down>" . md-move-lines-down)
	 ("C-M-<up>" . md-duplicate-up)
	 ("C-M-<down>" . md-duplicate-down)
	 ("C-M-<left>" . (lambda () (interactive) (transpose-words -1)))
	 ("C-M-<right>" . (lambda () (interactive) (transpose-words 1)))
	 ("M-<left>" . (lambda () (interactive) (transpose-chars -1)))
	 ("M-<right>" . (lambda () (interactive) (transpose-chars 1)))))

;;__________________________________________________________
;; for having tabs in top
;; (use-package elscreen
;;   :commands elscreen-start
;;   :config
;;   (which-key-add-key-based-replacements "C-c t" "elscreen")
;;   (setq elscreen-prefix-key (kbd "C-c t")
;; 	elscreen-tab-display-kill-screen nil
;; 	elscreen-tab-display-control nil
;; 	elscreen-display-screen-number nil))

;;__________________________________________________________
;; evil mode

(use-package avy
  :bind (("C-' r" . avy-resume)
	 ("C-' C-'" . avy-goto-char-timer)
	 ("C-' 1" . avy-goto-char)
	 ("C-' 2" . avy-goto-char-2)
	 ("C-' '" . avy-goto-char-in-line)
	 ("C-' w" . avy-goto-word-or-subword-1)
	 ("C-' W" . avy-goto-word-0)
	 ("C-' M-b" . avy-goto-word-0-above)
	 ("C-' M-f" . avy-goto-word-0-below)
	 ("C-' p" . avy-prev)
	 ("C-' n" . avy-next)
	 ("C-' s" . avy-goto-symbol-1)
	 ("C-' C-a" . avy-goto-line)
	 ("C-' C-e" . avy-goto-end-of-line)
	 ("C-' C-n" . avy-goto-line-below)
	 ("C-' C-p" . avy-goto-line-above)
	 ("C-' C-w" . avy-move-region)
	 ("C-' C-k" . avy-kill-region)
	 ("C-' M-w" . avy-kill-ring-save-region)
	 ("C-' i" . avy-copy-region)
	 :map isearch-mode-map
	 ("C-'" . avy-isearch))
  :config
  (setq avy-keys (nconc (number-sequence ?a ?z)	 ;; Order of proposals
	;;		(number-sequence ?A ?Z)
			)
	avy-style 'at-full		         ;; Propose only 1 letter
	;;avy-background t
	avy-all-windows nil			 ;; Only current window
	avy-case-fold-search nil		 ;; ignore case
	avy-highlight-first t
	;;avy-timeout-seconds 0.5
	)
  (set-face-attribute 'avy-lead-face nil
		      :background (alist-get 'blue my/colors)
		      :foreground (alist-get 'red my/colors)))

(use-package avy-zap
  :bind (("M-Z". avy-zap-up-to-char-dwim)
	 ("M-z". avy-zap-to-char-dwim)))

;; (use-package goto-line-preview
;;   :bind ([remap goto-line] . goto-line-preview))

;;__________________________________________________________
;; Arduino Mode

(use-package arduino-mode
  :mode "\\.ino\\'")

(use-package company-arduino
  :after company
  :hook (arduino-mode . company-arduino-turn-on))

(use-package flycheck-arduino :ensure arduino-mode
  :after flycheck
  :hook (arduino-mode . flycheck-arduino-setup))

;;__________________________________________________________
;; Multiple Cursors

(use-package iedit
  :bind ("C-c m i" . iedit-mode)
  :config
  (setq iedit-auto-recenter nil)
  (define-key iedit-lib-keymap (kbd "C-c m '") 'iedit-toggle-unmatched-lines-visible))

(global-unset-key (kbd "C-c <down-mouse-1>"))
(use-package multiple-cursors  ;; Multiple cursors package
  :bind (("C-c m l" . mc/edit-lines)
	 ("C-c m a" . mc/mark-all-like-this)
	 ("C-c m w" . mc/mark-all-words-like-this)

	 ("C-c m r" . mc/mark-all-in-region)
	 ("C-c m s" . mc/mark-all-in-region-regexp)
	 ("C-c m e" . mc/mark-more-like-this-extended)

	 ("C-c m n" . mc/mark-next-like-this)
	 ("C-c m p" . mc/mark-previous-like-this)

	 ("C-c m M-f" . mc/mark-next-like-this-word)
	 ("C-c m M-b" . mc/mark-previous-word-like-this)

	 ("C-c m M-p" . mc/mark-pop)

	 ("C-c m #" . mc/insert-numbers)
	 ("C-c m L" . mc/insert-letters)

	 ("C-c m C-a" . mc/edit-beginnings-of-lines)
	 ("C-c m C-e" . mc/edit-ends-of-lines))
  :init
  (which-key-add-key-based-replacements "C-c m" "multiple-cursors")

  :config
  (setq mc/always-run-for-all t
	mc/always-repeat-command t
	mc/edit-lines-empty-lines 'ignore))

;;__________________________________________________________
;; Expand region
(use-package expand-region
  :bind ("C-c e" . hydra-er/body)
  :hydra (hydra-er (:color red :columns 4)
		   "Expand region"
		   ("e" er/expand-region "expand")
		   ("w" er/mark-word "word")
		   ("c" er/mark-comment "comment")
		   ("S" er/c-mark-statement "c-stat")
		   ("i" er/mark-inside-pairs "ipar")
		   ("o" er/mark-outside-pairs "opar")
		   ("p" er/mark-text-paragraph "parag")
		   ("s" er/mark-text-sentence "sentence")
		   ("f" er/mark-defun "funct")
		   ("m" er/mark-email "mail")
		   ("u" er/mark-url "url")
		   ("\"" er/mark-outside-quotes "out \"")
		   ("'" er/mark-inside-quotes "in \"")
		   ("-" er/contract-region "contract")
		   ("ESC" nil "exit"))
  :init
  (which-key-add-key-based-replacements "C-c e" "expand-region"))

;;__________________________________________________________
;; Web mode

(use-package php-mode
  :mode ("\\.php\\'"))

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.html\\'"
	 "\\.tpl\\.php\\'" "\\.[agj]sp\\'"
	 "\\.as[cp]x\\'" "\\.erb\\'" "\\.djhtml\\'"))

(use-package company-web
  :preface
  (defun my/company-web ()
    (add-to-list (make-local-variable 'company-backends) '(company-web-html)))
  :hook (web-mode . my/company-web))

;; (use-package web-mode-edit-element
;;   :hook (web-mode . web-mode-edit-element-minor-mode))

;;__________________________________________________________
;; nginx mode
(use-package nginx-mode
  :mode ("sites-\\(?:available\\|enabled\\)\\'" "nginx\\.config\\'"))

(use-package company-nginx
  :hook (nginx-mode . company-nginx-keywords))

(use-package lice
  :commands lice)

(use-package lorem-ipsum :defer t)
;;__________________________________________________________

(use-package json-mode
  :mode "\\.json\\'")

(use-package sudo-edit
  :commands sudo-edit)

(use-package evil
  :disabled
  :init
  (setq-default evil-esc-delay 0.001
		evil-want-keybinding nil)
  :config
  (setq-default show-paren-when-point-inside-paren t)
  (evil-mode 1)
  ;; Modeline color
  (setq original-background (face-attribute 'mode-line :background)
	normal-state-background (alist-get 'brightblack my/colors)
	visual-state-background (alist-get 'green my/colors))

  (add-hook 'evil-normal-state-entry-hook
	    (lambda ()
	      (set-face-attribute 'mode-line nil :background normal-state-background)))
  (add-hook 'evil-insert-state-entry-hook
	    (lambda ()
	      (set-face-attribute 'mode-line nil :background original-background)))
  (add-hook 'evil-visual-state-entry-hook
	    (lambda ()
	      (set-face-attribute 'mode-line nil :background visual-state-background))))

(use-package evil-collection
  :disabled
  :custom (evil-collection-setup-minibuffer t)
  :hook (evil-mode .  evil-collection-init))

(use-package composable
  :diminish
  :after which-key
  :load-path "~/gits/composable.el/"
  :custom
  (composable-copy-active-region-highlight nil)
  :config
  (composable-mode)       ; Activates the default keybindings
  (composable-mark-mode)) ; Use composable with C-SPC

(use-package slime
  :commands slime
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy)))

(use-package objed
  :commands objed-mode)

(use-package e2ansi
  :defer t)

(use-package mutt-mode
  :mode "muttrc")

(provide 'init)
;;; init.el ends here
