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
;; Internal options

(setq-default auto-revert-verbose nil)	;; not show message when file changes
(global-auto-revert-mode t)		;; Autoload files changed in disk

;; (setq-default font-lock-maximum-decoration t)
;; (global-font-lock-mode t)		;; Use font-lock everywhere.

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

(setq-default ;;save-place-forget-unreadable-files t
	      save-place-ignore-files-regexp  ;; Modified to add /tmp/* files
	      "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\|^/tmp/.+\\)$")
(save-place-mode 1)                     ;; Remember point in files

;; Break long lines.
;;(global-visual-line-mode t)

;; Modeline
;; (display-battery-mode t)

;; (setq-default display-time-24hr-format t
;; 	      display-time-day-and-date t
;; 	      display-time-default-load-average nil)
;; (display-time-mode t)


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
	      ;; line-move-visual nil       ;; move cursor visual lines
	      backward-delete-char-untabify-method nil ;; Don't untabify on backward delete

	      ;; split-width-threshold 160  ;; Limite para split vertical
	      ;; kill-whole-line t
	      load-prefer-newer t
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
	      minibuffer-message-timeout 1
	      read-quoted-char-radix 16     ;; Read number of chars with C-q
	      kill-buffer-query-functions nil
	      kill-do-not-save-duplicates t ;; duplicate kill ring entries

	      eval-expression-print-length nil
	      eval-expression-print-level nil
	      enable-remote-dir-locals t    ;; Open remote dir locals.

	      ;; suggest-key-bindings t     ;; Ivy ya hace lo que esta opcion
	      uniquify-buffer-name-style 'post-forward
	      ;;uniquify-min-dir-content 0
	      truncate-lines t
	      auto-hscroll-mode 'current-line       ;; scroll horizontally 1 line not all
	      save-interprogram-paste-before-kill t ;; Save clipboard before replace
	      minibuffer-eldef-shorten-default t
	      )

;; Vertical window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?\u2502))

;; These two must be enabled/disabled together
;; (setq enable-recursive-minibuffers t) ;; Enable nesting in minibuffer
;; (minibuffer-depth-indicate-mode 1)    ;; Mostrar nivel de nesting en minibuffer

;;__________________________________________________________
;; I don't want confirm exit, not write yes-not either
(defalias 'yes-or-no-p 'y-or-n-p) ;; Reemplazar "yes" por "y" en el prompt

;;__________________________________________________________
;; use-package

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ;; Using Melpa and Elpa
			 ("melpa" . "https://melpa.org/packages/"))
      package-quickstart t)

(unless (and (fboundp 'package-installed-p)
	     (package-installed-p 'use-package))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package)

   (if init-file-debug
       (setq use-package-always-ensure t
	     use-package-enable-imenu-support t
	     use-package-verbose t
	     use-package-expand-minimally nil
	     use-package-compute-statistics t
	     debug-on-error t)

     (setq use-package-always-ensure nil
	   use-package-enable-imenu-support nil
	   use-package-verbose nil
	   use-package-expand-minimally t)))


(use-package benchmark-init
  :if init-file-debug
  :config
  (add-hook 'window-setup-hook 'benchmark-init/deactivate t))

(use-package use-package-hydra)

;;__________________________________________________________
;; Config file not here to not track it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  (message "Creating %s" custom-file))
(load custom-file)

;; Personal Lisp dir
(defconst mylisp-dir (expand-file-name "lisp" user-emacs-directory))

(unless (file-exists-p mylisp-dir)
  (make-directory mylisp-dir)
  (message "Creating %s" mylisp-dir))
(add-to-list 'load-path mylisp-dir)

;; System Lisp dir
(defconst syslisp-dir "/usr/share/emacs/site-lisp")
(when (file-exists-p syslisp-dir)
  (add-to-list 'load-path syslisp-dir))

;; Next file is in my lisp directory. it only defines mu4e config and
;; a variable for the gmail calendar.
(unless (require 'configmail "configmail.el" t)
  (message "No mail config file found: ignored"))

;;__________________________________________________________
;; Benchmark-init

(use-package diminish)		      ;; if you use :diminish
(use-package bind-key)		      ;; if you use any :bind variant

(use-package paradox
  :defer t
  :custom
  (paradox-spinner-type 'progress-bar)
  (paradox-display-download-count t)
  (paradox-display-star-count t))
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
;; The Colors I am using my own theme

(load-theme 'simple-16)

(if (display-graphic-p)
    (set-face-attribute 'default nil :family "Hack" :height 110))

(defmacro named-color (colorname)
  "Get color by name COLORNAME from `my/colors' alist."
  `(simple-16-theme-color ,colorname))

;;__________________________________________________________
;;Packages options
;;__________________________________________________________

;;__________________________________________________________
;; Show paren mode
(setq-default show-paren-delay 0
	      blink-matching-paren nil)
(show-paren-mode t)	  ;; Highlight couple parentesis


(global-set-key [remap just-one-space] #'cycle-spacing)

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;__________________________________________________________
;; compile

(use-package compile :ensure nil
  :defer t
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t))

;;__________________________________________________________
;; ssh
(use-package tramp :ensure nil
  :defer t
  :custom
  (tramp-auto-save-directory
   (expand-file-name "tramp-autosave-dir" user-emacs-directory))
  (tramp-default-method "rsync")
  ;;(tramp-change-syntax 'simplified)
  (tramp-use-ssh-controlmaster-options nil)
  ;;(tramp-completion-reread-directory-timeout t) ;; Obsolete
  (tramp-persistency-file-name
   (expand-file-name "tramp" user-emacs-directory))
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; (use-package tramp-term
;;   :after tramp
;;   :commands tramp-term)

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
	 ("/sshd?_config\\'" . ssh-config-mode)
	 ("/known_hosts\\'" . ssh-known-hosts-mode)
	 ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

;;__________________________________________________________
;; tab-bar

(use-package tab-bar :ensure nil
  :defer t
  :custom
  (tab-bar-tab-hints t)  ;; show tab numbers
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable) ;; When close last
  ;;(tab-bar-show 1)
  )

;;__________________________________________________________
;; minibuffers

(defun my/minibuffer-setup-hook ()
  "Hook to call when open the minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  "Hook to call when leave the setq."
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

;;__________________________________________________________
;; gdb rectangles

(use-package gdb :ensure nil
  :defer t
  :custom
  (gdb-many-windows nil)
  (gdb-show-main t))

;;__________________________________________________________
;; Two options for diffs
(use-package ediff :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)
  :config
  (with-eval-after-load 'winner
    (add-hook 'ediff-after-quit-hook-internal #'winner-undo)))

;; more like vimdiff
(use-package vdiff
  :defer t
  :bind-keymap ("C-c v" . vdiff-mode-prefix-map)
  :custom
  (vdiff-auto-refine t))

(use-package smerge-mode :ensure nil
  :defer t
  :preface
  (defun my/enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (when (not (bound-and-true-p smerge-mode))
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^<<<<<<< " nil :noerror)
	  (smerge-mode 1)
	  (hydra-smerge/body)))))

  :hook ((find-file magit-diff-visit-file) . my/enable-smerge-maybe)
  :custom
  (smerge-diff-buffer-name "*smerge-diff*")
  :hydra (hydra-smerge
	  (:color pink :hint nil
		  :post (smerge-auto-leave))
	  "smerge"
	  ("n" smerge-next "next")
	  ("p" smerge-prev "prev")
	  ("b" smerge-keep-base "base")
	  ("u" smerge-keep-upper "upper")
	  ("l" smerge-keep-lower "lower")
	  ("a" smerge-keep-all "all")
	  ("q" nil "cancel" :color blue))
  )

;;__________________________________________________________
;; Diminish To Hide Packages from bar
(use-package diminish)

;;__________________________________________________________
;; which-key

(use-package which-key
  :defer t
  :diminish
  :custom
  ;;(which-key-idle-delay 0.4)
  ;;(which-key-idle-delay 10000)
  ;;(which-key-show-early-on-C-h t)
  (which-key-idle-secondary-delay 0.01)  ;; nil sets the same delay
  (which-key-dont-use-unicode t)
  ;;(which-key-separator ": ") ;which-key-idle-delay 2.0)
  :config
  (which-key-mode t)
  (which-key-add-key-based-replacements
    "C-c h" "highlight"
    "C-c s" "sidebars"
    "C-x r" "rectangle||register"
    "C-x n" "narrow"
    "C-x t" "tabs"
    "C-x a" "abbrev"))
;;__________________________________________________________
;; terms

(use-package vterm
  :defer t
  :preface
  (defun my/vterm-mode-hook ()
    (display-fill-column-indicator-mode -1)
    (auto-fill-mode -1))
  :hook (vterm-mode . my/vterm-mode-hook)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  :config
  ;; Add find-file-other-window to accepted commands
  (add-to-list 'vterm-eval-cmds
	       '("find-file-other-window" find-file-other-window))
  )

;; (use-package multi-vterm
;;   :custom
;;   (multi-vterm-dedicated-window-height 15) ;; height in lines
;;   :bind (("C-c 5 v" . multi-vterm)
;; 	 ("C-c t t" . multi-vterm-dedicated-toggle)))

(use-package vterm-toggle
  :bind (("C-c t t" . vterm-toggle-cd)
	 :map vterm-mode-map
	 (("<C-return>" . vterm-toggle-insert-cd)
	  ("C-M-n" . vterm-toggle-forward)
	  ("C-M-p" . vterm-toggle-backward)))
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-project-root t)
  (vterm-toggle-reset-window-configration-after-exit 'kill-window-only)
  (vterm-toggle-fullscreen-p nil)
  :config
  ;; Show at bottom
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _)
		   (with-current-buffer bufname
		     (equal major-mode 'vterm-mode)))
                 ;; (display-buffer-reuse-window display-buffer-at-bottom)
                 (display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 (direction . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package emamux :defer t)

(use-package pkgbuild-mode
  :mode "/PKGBUILD$")

;;__________________________________________________________
;; Better shell (for ssh)
(use-package better-shell
  :bind ("C-c t b" . better-shell-shell))

(use-package shell-command+
  :bind ([remap shell-command] . shell-command+))

;;__________________________________________________________
;; Clipboard copy and paste with: M-w & C-c v

(setq-default xclip-method
	      (and (not (or (display-graphic-p)        ;; checks
			    (string-equal (getenv "TERM") "linux")))
		   (or (and (getenv "DISPLAY")         ;; x11
			    (executable-find "xclip")
			    'xclip)
		       (and (getenv "WAYLAND_DISPLAY") ;; wayland
			    (executable-find "wl-copy")
			    'wl-copy))))
(use-package xclip
  :if xclip-method
  :config
  (xclip-mode 1))

;;__________________________________________________________
;;	Seleccionar con el mouse
(use-package mouse :ensure nil
  :unless (or (display-graphic-p)
	      (string-equal (getenv "TERM") "linux"))
  :custom
  (mouse-sel-mode t)          ;; Mouse selection
  (mouse-scroll-delay 0)
  :config
  (xterm-mouse-mode t)			  ;; mover el cursor al click
  (defun track-mouse (e))
  (set-cursor-color "white")
  (set-mouse-color "white")		  ;; Flechita del mouse en blanco
  (when (fboundp 'mouse-wheel-mode)
    (setq-default mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control)))
		  mouse-wheel-progressive-speed nil)
    (mouse-wheel-mode t))			  ;; scrolling con el mouse
  )

(global-set-key [drag-mouse-2] 'mouse-yank-at-click)

(defun my/scroll-up-command (&optional arg)
  "Scroll up single line or ARG."
  (interactive "^p")
  (scroll-up-command 1))

(defun my/scroll-down-command (&optional arg)
  "Scroll down single line or ARG."
  (interactive "^p")
  (scroll-down-command arg))

(global-set-key [remap scroll-up-command] 'my/scroll-up-command)
(global-set-key [remap scroll-down-command] 'my/scroll-down-command)
;;__________________________________________________________
;; My program's mode hooks

(use-package which-func :ensure nil
  :diminish
  :hook (prog-mode . which-function-mode) ;; Shows the function in spaceline
  )

(defun my/prog-mode-hook () "Some hooks only for prog mode."
       ;;(electric-indent-mode t)	    		;; On by default
       (electric-pair-local-mode t)			;; Autoannadir parentesis

       ;;(define-key global-map (kbd "RET") 'newline-and-indent)
       ;;(electric-indent-local-mode t)         ;; Shouldn't be need
       (setq-local show-trailing-whitespace t)

       (defun smart-beginning-of-line ()
	 "Move point to first non-whitespace character or beginning-of-line."
	 (interactive)
	 (let ((oldpos (point)))
	   (back-to-indentation)
	   (and (= oldpos (point))
		(beginning-of-line))))

       (global-set-key [remap move-beginning-of-line]
		       #'smart-beginning-of-line))

(add-hook 'prog-mode-hook #'my/prog-mode-hook)

;;__________________________________________________________
;; Undo tree

(global-set-key [remap undo] 'undo-only)
(global-set-key (kbd "C-M-_") 'undo-redo)
(global-set-key (kbd "C-M-/") 'undo-redo)

(use-package string-inflection
  :bind ("C-c <right>" . string-inflection-all-cycle))

;;__________________________________________________________
;; Mark column 80 when crossed
(use-package hl-line :ensure nil
  :diminish
  :bind ("C-c h l" . hl-line-mode))

;;__________________________________________________________
;; Mark column 80 when crossed

(use-package highlight-indent-guides
  :diminish
  :bind ("C-c h i" . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-method 'character)
  :config
  (set-face-attribute 'highlight-indent-guides-character-face nil
		      :foreground (named-color brightblack)))

;;__________________________________________________________
;; Resalta scopes entorno al cursor
(use-package highlight-blocks
  :diminish
  :bind (("C-c h b" . highlight-blocks-now)
	 ("C-c h B" . highlight-blocks-mode))
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
  :diminish
  :bind ("C-c h s" . hes-mode)
  :config
  (set-face-attribute 'hes-escape-backslash-face nil
		      :foreground (named-color magenta))
  (set-face-attribute 'hes-escape-sequence-face nil
		      :foreground (named-color magenta)))

;;__________________________________________________________
;; Flyspell (Orthography)
(use-package flyspell :ensure nil
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode))
  :bind (:map flyspell-mode-map
	      (("C-M-i" .  nil)
	       ("C-'" .	nil)
	       ("C-;" .	nil)
	       ("C-," .	nil)
	       ("C-." .	nil)
	       ("C-c $" .  nil)
	       ("C-c f r" . flyspell-region)
	       ("C-c f b" . flyspell-buffer)
	       ("C-c f n" . flyspell-goto-next-error)))
  :config
  (which-key-add-key-based-replacements "C-c f" "flyspell"))

(use-package flyspell-correct-ivy
  :diminish
  :after flyspell
  :bind (("C-c f w" . flyspell-correct-wrapper)
	 ("C-c f f" . flyspell-correct-at-point)
	 ("C-c f C-n" . flyspell-correct-next)
	 ("C-c f C-p" . flyspell-correct-previous))
  :custom
  (flyspell-correct-interface #'flyspell-correct-ivy))

;;__________________________________________________________
;; {c/c++}-mode
;;__________________________________________________________

;;__________________________________________________________
;; LSP try for a whil

(use-package eglot
  :defer t)


(use-package lsp-mode
  :diminish lsp
  :bind-keymap ("C-c l" . lsp-command-map)
  :init
  (which-key-add-key-based-replacements "C-c l" "lsp")
  :custom
  (lsp-keymap-prefix (kbd "C-c l"))
  (lsp-enable-snippet nil)
  (lsp-eldoc-hook nil)
  (lsp-enable-indentation nil)
  (lsp-prefer-capf t)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  ;; lsp-diagnostic-package t ;; prefer flymake
  :config
  ;; This before calling lsp
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  (lsp)
  ;; TODO: extend this for more languages
  (when (memq major-mode '(c-mode c++-mode))
    (add-hook 'c-mode-common-hook #'lsp-deferred))

  (when (eq major-mode 'python-mode)
    (add-hook 'python-mode-hook #'lsp-deferred))
  )

(use-package lsp-ui
  :diminish
  :bind (:map lsp-command-map
	      ;; peek commands
	      (("u d" . lsp-ui-peek-find-definitions)
	       ("u r" . lsp-ui-peek-find-references)
	       ("u i" . lsp-ui-peek-find-implementation)
	       ;;("s" . lsp-ui-peek-find-workspace-symbol)
	       ("u c" . lsp-ui-peek-find-custom)
	       ;; imenu
	       ("u m" . lsp-ui-imenu)
	       ;; flycheck
	       ("u f" . lsp-ui-flycheck-list)
	       ;; lsp-ui
	       ("u n" . lsp-ui-find-next-reference)
	       ("u p" . lsp-ui-find-prev-reference)))
  :custom
  ;;(lsp-ui-sideline-delay 1.0)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  :config
  (which-key-add-key-based-replacements "C-c l u" "lsp-ui"))

(use-package lsp-treemacs
  :diminish
  :after lsp-mode
  :custom
  (lsp-metals-treeview-enable t)
  (lsp-metals-treeview-show-when-views-received t))

(use-package lsp-ivy
  :diminish
  :after lsp-mode
  :bind (:map lsp-mode-map
	      ("C-c l i" . lsp-ivy-workspace-symbol)))

;; (use-package ccls
;;   :defer t
;;   :config
;;   (setq ccls-enable-skipped-ranges nil))

;;__________________________________________________________
;; C common mode (for all c-like languajes)

(defun ms-space-for-alignment-hook ()
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
    (back-to-indentation)))

(define-minor-mode c-ms-space-for-alignment-mode
  "Enable indent with tabs align with spaces."
  :global nil
  :init-value nil
  (if c-ms-space-for-alignment-mode
      (when (and indent-tabs-mode
		 (= c-basic-offset tab-width))
	(add-hook 'c-special-indent-hook #'ms-space-for-alignment-hook nil t))
    (remove-hook 'c-special-indent-hook #'ms-space-for-alignment-hook t)))

;;====================

;; (defun my/c-semi&comma ()
;;   "Function to handle addition of ; in 'c-mode'."
;;   (assq 'class-close c-syntactic-context))

(use-package cc-mode :ensure nil
  :preface
  (defun my/c-mode-common-hook ()
    "My hook for C and C++."
    (c-toggle-auto-newline 1)
    (c-toggle-cpp-indent-to-body 1)
    (c-ms-space-for-alignment-mode 1)
    (message "Loaded my/c-mode-common"))
  :hook (c-mode-common . my/c-mode-common-hook)
  :custom
  (c-default-style '((java-mode . "java")
		     (awk-mode . "awk")
		     (other . "mylinux")))
  :config
  (c-add-style "mylinux"
	       '("linux"
		 (tab-width . 4)
		 (c-basic-offset . 4)
		 (indent-tabs-mode . t)
		 (fill-column . 80)
		 ;; (c-hanging-semi&comma-criteria my/c-semi&comma)
		 (c-hanging-semi&comma-criteria nil)
		 (c-cleanup-list empty-defun-braces ;; {}
				 brace-else-brace   ;; } else {
				 brace-elseif-brace ;; } else if {
				 defun-close-semi   ;; }; after class
				 )
		 (c-hanging-braces-alist (defun-open before after)
					 (brace-list-open)
					 (brace-entry-open)
					 (substatement-open after)
					 (namespace-open after)
					 (namespace-close before)
					 (block-close . c-snug-do-while)
					 (arglist-cont-nonempty)
					 (class-open after)
					 (class-close before))
		 (c-offsets-alist (inline-open . 0)
				  (comment-intro . 0)
				  (arglist-close . 0)
				  ;;(innamespace . [0])
				  ;;(access-label '-)
				  ))))

(use-package preproc-font-lock ;; Preprocessor
  :hook (c-mode-common . preproc-font-lock-mode)
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
	 ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown"))

;;__________________________________________________________
;; Restructured text
(use-package sphinx-mode
    :hook rst-mode)

;;__________________________________________________________
;; ruby-mode
(use-package ruby-mode :ensure nil
  :mode ("\\.rb\\'" "\\.rjs\\'" "\\Rakefile\\'" "\\Gemfile\\'")
  :custom
  (ruby-indent-level 2))

(use-package ruby-tools
  :hook (ruby-mode . ruby-tools-mode))

(use-package ruby-electric
  :hook (ruby-mode . ruby-electric-mode))

;;__________________________________________________________
;; Julia Mode
(use-package julia-mode
  :mode "\\.jl\\'")

(use-package flycheck-julia
  :hook (julia-mode . flycheck-julia-setup))

;;__________________________________________________________
;; Rust Mode
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package flycheck-rust
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
  :mode "\\.go\\'")

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
  :defer t)

;;__________________________________________________________
;; Use for Qt's .pro and .pri files
(use-package qt-pro-mode
  :mode (("\\.pr[io]\\'" . qt-pro-mode)
	 ("\\.moc\\'" . c++-mode)
	 ("\\.ui\\'" . xml-mode)))

;;__________________________________________________________
;; javascript-mode
(use-package js-mode :ensure nil
  :mode ("\\.js\\'"))

;;__________________________________________________________
;; xml-mode
(use-package xml-mode :ensure nil
  :mode ("\\.ipe\\'" "\\.qrc\\'" "\\.svn\\'"))

;;__________________________________________________________
;; splitting

(use-package windmove :ensure nil
  :bind (("C-x <left>" . windmove-left)
	 ("C-x <right>" . windmove-right)
	 ("C-x <up>" . windmove-up)
	 ("C-x <down>" . windmove-down)
	 ("C-x <M-left>" . windmove-swap-states-left)
	 ("C-x <M-right>" . windmove-swap-states-right)
	 ("C-x <M-up>" . windmove-swap-states-up)))

;; (use-package ace-window
;;   :bind ([remap other-window] . ace-window)
;;   :custom
;;   ;;(aw-background nil)
;;   (aw-ignore-current t)
;;   (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Lets try this instead of ace window
(use-package switch-window
  :bind (
	 ("C-x o" . switch-window)
	 ("C-x 4 1" . switch-window-then-maximize)
	 ("C-x 4 2" . switch-window-then-split-below)
	 ("C-x 4 3" . switch-window-then-split-right)
	 ("C-x 4 0" . switch-window-then-delete)

	 ("C-x 4 d" . switch-window-then-dired)
	 ;;("C-x 4 f" . switch-window-then-find-file)
	 ;;("C-x 4 r" . switch-window-then-find-file-read-only)
	 ("C-x 4 C-b" . switch-window-then-display-buffer)
	 ("C-x 4 k" . switch-window-then-kill-buffer))
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-threshold 2)
  (switch-window-minibuffer-shortcut ?z)
  (switch-window-background t)
  (switch-window-shortcut-appearance 'asciiart))

;; Undo redo split
(use-package winner :ensure nil
  :bind (("C-x w u" . winner-undo)
	 ("C-x w r" . winner-redo))
  :defer 1  ;; this always after the bind
  :custom
  (winner-dont-bind-my-keys t)
  :init
  (which-key-add-key-based-replacements "C-x w" "winner")
  :config
  (winner-mode 1))

;; Change color selected buffers
(use-package auto-dim-other-buffers
  :ensure
  :commands auto-dim-other-buffers-mode
  :custom
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (auto-dim-other-buffers-dim-on-focus-out t))

;;__________________________________________________________
;; Lines enabling gnuplot-mode
(use-package gnuplot-mode
  :mode ("\\.gp\\'" "\\.gpl\\'" "\\.plt\\'"))

;;__________________________________________________________
;; Auto completamiento
(use-package company
  :diminish
  :bind (;;([remap dabbrev-expand] . company-complete)
	 ("<C-return>" . company-complete)
	 :map company-active-map
	 ("<M-return>" . company-other-backend)
	 ;;([remap dabbrev-expand] . company-abort)
	 ("<C-return>" . company-abort)
	 )
  :hook ((prog-mode message-mode conf-mode) . company-mode)
  :custom
  (company-idle-delay nil)	 ;; no delay for autocomplete
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around nil)
  (company-show-numbers t)
  ;;company-tooltip-limit 20
  (company-backends '(company-semantic
		     company-capf		 ;; completion at point
		     company-files	 ;; company files
		     (company-dabbrev-code company-gtags company-keywords)
		     company-dabbrev))
  )

(use-package yasnippet        ;; Snippets
  :diminish
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y x" . yas-expand)
	 :map yas-keymap
         ([remap indent-for-tab-command] . yas-next-field-or-maybe-expand)
	 :map yas-minor-mode-map
	 ("TAB" . nil)
	 ("<tab>" . nil))
  :init
  (which-key-add-key-based-replacements "C-c y" "yasnippet")
  :custom
  (yas-verbosity 1)                 ; No need to be so verbose
  (yas-wrap-around-region t)
  :config
  (yas-reload-all)
  (yas-minor-mode 1)
  ;; (yas-global-mode 1)
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
(use-package flycheck
  :diminish
  :if (< (buffer-size) 200000)
  ;;:defer t
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-command-map
	      ("a" . hydra-flycheck/body))
  :init
  (which-key-add-key-based-replacements "C-c a" "flycheck")
  :hydra (hydra-flycheck (:color red :columns 4
				 :pre (hl-line-mode t)
				 :post (hl-line-mode -1))
			 "flycheck"
			 ("b" flycheck-buffer "check-buffer")
			 ("c" flycheck-compile "compile")
			 ("d" flycheck-display-error-at-point "display-error")
			 ("e" flycheck-explain-error-at-point "explain-error")
			 ("l" flycheck-list-errors "list-errors")
			 ("n" flycheck-next-error "next-error")
			 ("p" flycheck-previous-error "previous-error")
			 ("s" flycheck-select-checker "select-checker")
			 ("v" flycheck-verify-setup "verify-setup")
			 ("ESC" nil "exit"))
  :custom
  (flycheck-display-errors-delay 1.0)
  (flycheck-keymap-prefix (kbd "C-c a"))
  :config
  (pcase major-mode
    ('c-mode
     (setq flycheck-gcc-language-standard "c17"
	   flycheck-clang-language-standard "c17"))
    ('c++-mode
     (setq flycheck-gcc-language-standard "c++17"
	   flycheck-clang-language-standard "c++17"))))

(use-package flymake :ensure nil
  :diminish
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 1.0) ;; default 0.5
  )

;;__________________________________________________________
;; Function arguments show

(use-package eldoc :ensure nil
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . eldoc-mode)
  :config
  (eldoc-mode t))

(use-package helpful
  :bind (("C-h F" . helpful-function)
	 ("C-h C" . helpful-command)
	 ("C-h M" . helpful-macro)
	 ("C-h L" . helpful-callable)
	 ("C-h K" . helpful-key)
	 ("C-h P" . helpful-at-point)
	 ("C-h V" . helpful-variable)))

;;__________________________________________________________
;; Chequeo de gramatica
(use-package langtool
  :defer t
  :custom
  (langtool-default-language "en")
  (langtool-language-tool-jar "~/gits/languagetool/languagetool-standalone/target/LanguageTool-4.6-SNAPSHOT/LanguageTool-4.6-SNAPSHOT/languagetool-commandline.jar"))

;;__________________________________________________________
;; Email mode for mutt
;;__________________________________________________________
(use-package abbrev :ensure nil
  :diminish)

;; Asocia buffers que empiecen con messaje mode
(use-package message-mode :ensure nil
  :mode ("/neomut" "neomutt-Ergus-" "draft")
  :custom
  (mail-header-separator "")
  :config
  (auto-fill-mode t)
  (mail-abbrevs-setup)
  (flyspell-mode t))

(use-package notmuch
  :preface
  (defun my/notmuch ()
    (require 'notmuch-address)
    ;; (setq notmuch-address-command "~/gits/notmuch-addrlookup-c/notmuch-addrlookup")
    (require 'notmuch-company)
    (add-to-list (make-local-variable 'company-backends) 'notmuch-company))
  :init
  (setenv "NOTMUCH_CONFIG" "/home/ergo/almacen/mail/notmuch-config")
  :hook (message-mode . my/notmuch))

;;__________________________________________________________
;; Latex mode

(use-package tex :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :custom
  (TeX-source-correlate-start-server t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (LaTeX-babel-hyphen nil)
  (TeX-master nil) ;; Multidocument

  :config
  (TeX-source-correlate-mode t)

  (defun my/LaTeX-mode-hook ()
    (flyspell-mode 1)
    (visual-line-mode 1)
    (auto-fill-mode 1))

  (add-hook 'LaTeX-mode-hook #'my/LaTeX-mode-hook)

  (add-to-list 'TeX-command-list
  	       '("Makeglossaries" "makeglossaries %s" TeX-run-command nil
  		 (latex-mode)
  		 :help "Run makeglossaries, will choose xindy or makeindex") t)

  ;; ====== Fix for itemize indentation.
  (defcustom LaTeX-indent-level-item-continuation 4
    "Indentation of continuation items."
    :group 'LaTeX-indentation
    :type 'integer)

  (defun my/LaTeX-indent-item ()
    "Syntactic indentation for itemize like environments to add extra offsets."
    (save-match-data
      (let* ((itemcont (or (and (boundp 'LaTeX-indent-level-item-continuation)
				(numberp LaTeX-indent-level-item-continuation)
				LaTeX-indent-level-item-continuation)
			   0))
	     (offset (+ LaTeX-indent-level LaTeX-item-indent))
             (re-beg "\\\\begin{")
             (re-end "\\\\end{")
             (re-env "\\(itemize\\|\\enumerate\\|description\\)")
             (indent (save-excursion                                 ;; parent indent column
                       (when (looking-at (concat re-beg re-env "}"))
			 (end-of-line))
                       (LaTeX-find-matching-begin)
                       (current-column))))
	(cond ((looking-at (concat re-beg re-env "}"))               ;; row with \begin{itemize}
               (or (save-excursion
                     (beginning-of-line)
                     (ignore-errors
                       (LaTeX-find-matching-begin)
		       (+ (current-column)
			  (if (looking-at (concat re-beg re-env "}")) ;; check if parent scope is also itemize
			      offset
			    LaTeX-indent-level)))
		     indent)))
              ((looking-at (concat re-end re-env "}"))               ;; row with \end{itemize}
	       indent)
              ((looking-at "\\\\item")                               ;; row with \item
	       (+ indent offset))
              (t                                                     ;; any other row
	       (+ indent offset itemcont))))))

    (with-eval-after-load "latex"
      (add-to-list 'LaTeX-indent-environment-list '("itemize" my/LaTeX-indent-item))
      (add-to-list 'LaTeX-indent-environment-list '("enumerate" my/LaTeX-indent-item))
      (add-to-list 'LaTeX-indent-environment-list '("description" my/LaTeX-indent-item)))
    ;; =========================

    (flyspell-buffer))

(use-package auctex-latexmk
  :defer t
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package company-math
  :after (company tex)
  :config
  (add-to-list 'company-backends
	       '(company-math-symbols-latex company-latex-commands)))

;; (use-package company-auctex
;;   :after (company-math tex)
;;   :config
;;   (add-to-list 'company-backends 'company-auctex-labels)
;;   (add-to-list 'company-backends 'company-auctex-bibs)
;;   (add-to-list 'company-backends
;; 	       '(company-auctex-macros company-auctex-symbols company-auctex-environments)))

(use-package reftex :ensure nil ;; Reftex for cross references
  :defer t
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-cite-prompt-optional-args t)   ; Prompt for empty optional arguments in cite
  (reftex-cite-format 'biblatex)
  (reftex-plug-into-AUCTeX t)
  (reftex-insert-label-flags '(t t))
  (reftex-save-parse-info t)
  (reftex-enable-partial-scans t)
  (reftex-use-multiple-selection-buffers t)
  :config
  (reftex-isearch-minor-mode))


(use-package company-reftex
  :after (company reftex)
  :config
  (add-to-list 'company-backends
	       '(company-reftex-labels company-reftex-citations)))

;;__________________________________________________________
;;bibtex mode set use biblatex
(use-package bibtex :ensure nil
  :mode (("\\.bib\\'" . bibtex-mode))
  :custom
  (bibtex-dialect 'biblatex))

(use-package company-bibtex
  :after bibtex
  :config
  (add-to-list (make-local-variable 'company-backends) 'company-bibtex))

(use-package ivy-bibtex
  :defer t
  :custom
  (ivy-bibtex-default-action #'bibtex-completion-insert-citation))

;;__________________________________________________________
;; Python mode

(use-package python-mode :ensure nil
  :mode "\\.py\\'"
  :interpreter "python3"
  :bind (:map python-mode-map
              ("C-c C-z" . python-shell))
  :custom
  (python-shell-interpreter "ipython3")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  (python-check-command "pyflakes")
  (flycheck-python-flake8-executable "flake8"))

(use-package ein :defer t)

;;__________________________________________________________
;; Dired-mode settings (file manager)
(use-package dired :ensure nil
  :defer t
  :preface
  (defun my/dired-up-directory ()
    (interactive)
    (find-alternate-file ".."))
  :bind (:map dired-mode-map
	 ([remap dired-find-file] . dired-find-alternate-file)
	 ([remap dired-up-directory] . my/dired-up-directory))
  :custom
  (dired-recursive-copies 'top)	     ;; Always ask recursive copy
  (dired-recursive-deletes 'top)     ;; Always ask recursive delete
  (dired-dwim-target t)		     ;; Copy in split mode with p
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alh")
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x :ensure nil
  :hook (dired))

(use-package dired-sidebar
  :bind ("C-c s d" . dired-sidebar-toggle-sidebar)
  :custom
  ;;(dired-sidebar-use-term-integration t)
  (dired-sidebar-theme 'nerd)
  (dired-sidebar-subtree-line-prefix "."))

(use-package dired-du
  :commands dired-du-mode)


;; __________________________________________________________
;; Templates Projects

(use-package ptemplate
  :defer t)

(use-package ptemplate-templates
  :after ptemplate
  :config
  (ptemplate-templates-mode 1))
;;__________________________________________________________
;; ibuffer
(use-package ibuffer :ensure nil
  :preface
  (defun my/ibuffer-mode-hook ()
    (hl-line-mode 1)
    (ibuffer-auto-mode 1))
  :bind ([remap list-buffers] . ibuffer)
  :hook (ibuffer-mode . my/ibuffer-mode-hook)
  :custom
  (ibuffer-default-sorting-mode 'alphabetic)  ;; can use recency
  )

(use-package ibuffer-sidebar
  :bind (("C-c s b" . ibuffer-sidebar-toggle-sidebar)))

(use-package ibuffer-tramp
  :after tramp
  :commands ibuffer-tramp-set-filter-groups-by-tramp-connection)

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

(use-package flx :defer t)

(use-package ivy
  :diminish
  :bind (("C-c C-r" . ivy-resume)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-partial)
	 ("RET" . ivy-alt-done))
  :init
  (which-key-add-key-based-replacements "C-c i" "ivy")
  :custom
  (ivy-use-virtual-buffers nil)   ;; Recent files or buffers in ivy
  (ivy-count-format "(%d/%d) ")
  (ivy-pulse-delay nil)
  (ivy-use-selectable-prompt t)
  (ivy-fixed-height-minibuffer t)
  (ivy-read-action-function #'ivy-hydra-read-action)	;; Depends of ivy-hydra
  ;;(ivy-height 10)
  ;;(ivy-wrap t)					;; cycle in minibuffer
  :config
  (defun ivy-highlight-thing-at-point ()
    "Highlight thing at point or region."
    (interactive)
    (require 'hi-lock)
    (let ((thing (ivy-thing-at-point))
	  (face (hi-lock-read-face-name)))
      (or (facep face) (setq face 'hi-yellow))
      (unless hi-lock-mode (hi-lock-mode 1))
      (when (use-region-p)
	(deactivate-mark))
      (hi-lock-set-pattern (regexp-quote thing) face)))
  (global-set-key [remap highlight-symbol-at-point] #'ivy-highlight-thing-at-point)

  (copy-face 'highlight 'ivy-current-match)  ;; Linea seleccionada

  (set-face-attribute 'ivy-minibuffer-match-face-1 nil     ;; espacio entre matches
		      :inherit nil :background nil
		      :foreground nil :underline t)
  (copy-face 'lazy-highlight 'ivy-minibuffer-match-face-2)
  (copy-face 'lazy-highlight 'ivy-minibuffer-match-face-3)
  (copy-face 'lazy-highlight 'ivy-minibuffer-match-face-4)

  ;; Highlight with arrows by default.
  ;;(add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-arrow))
  (ivy-mode 1))

(use-package ivy-avy :after ivy)

(use-package ivy-hydra :defer t) ;; Dependency from ivy to use ivy-hydra-read-action

(use-package ivy-xref
  :init
  (which-key-add-key-based-replacements "C-c x" "xref")
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :bind (("C-c x d" . xref-find-definitions)
	 ("C-c x 4" . xref-find-definitions-other-window)
	 ("C-c x a" . xref-find-apropos)
	 ("C-c x b" . xref-pop-marker-stack) ;; go back
	 ("C-c x r" . xref-find-references)
	 ("C-c x TAB" . completion-at-point)))

(use-package swiper
  :preface
  :bind (([remap isearch-forward] . swiper-isearch)
	 ([remap isearch-backward] . swiper-isearch-backward)
	 ([remap isearch-forward-symbol-at-point] . swiper-isearch-thing-at-point)
	 :map swiper-map
	 ("C-," . swiper-avy)
	 ("C-c m" . swiper-mc)
	 ("C-o" . swiper-isearch-toggle)
	 :map isearch-mode-map
	 ("C-o" . swiper-isearch-toggle))
  :config
  (copy-face 'isearch 'swiper-isearch-current-match)
  (copy-face 'highlight 'swiper-line-face)         ;; linea minibuffer

  (set-face-attribute 'swiper-match-face-1 nil     ;; linea en minibuffer
   		      :inherit nil :background nil :weight 'ultrabold)

  (copy-face 'isearch 'swiper-match-face-2) ;; primer match
  (copy-face 'isearch 'swiper-match-face-3) ;; segundo match
  (copy-face 'isearch 'swiper-match-face-4) ;; tercer match

  (copy-face 'lazy-highlight 'swiper-background-match-face-2)
  (copy-face 'lazy-highlight 'swiper-background-match-face-3)
  (copy-face 'lazy-highlight 'swiper-background-match-face-4)

  ;; (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper-isearch . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper-isearch-backward . ivy--regex-plus))
  )

(use-package imenu-anywhere
  :bind ("C-c i i" . ivy-imenu-anywhere)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 200))

(use-package imenu-list
  :bind ("C-c s i" . imenu-list-smart-toggle)
  :custom
  (imenu-list-position 'left))

(use-package counsel
  :diminish
  :bind (:map counsel-mode-map
	      (("C-x b" . counsel-switch-buffer)
	       ("C-x 4 b" . counsel-switch-buffer-other-window)
	       ("C-c c a" . counsel-ag)
	       ("C-c c b" . counsel-ibuffer)
	       ("C-c c d" . counsel-dired)
	       ("C-c c f" . counsel-flycheck)
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
	       ("C-c c C-r" . counsel-register))
	      :map help-map			      ;; help-map
	      (("f" . counsel-describe-function)
	       ("v" . counsel-describe-variable)
	       ("C-l" . counsel-info-lookup-symbol)))
  :defer 0.25
  :init
  (which-key-add-key-based-replacements "C-c c" "counsel")
  :custom
  (counsel-find-file-at-point t)       ;; Select file at point
  (counsel-preselect-current-file t)   ;; Select current file in list
  :config
  (counsel-mode 1)
  ;; match by words
  ;; (add-to-list 'ivy-re-builders-alist '(counsel-M-x . ivy--regex-fuzzy))
  )

(use-package amx ;; Complete history
  :after counsel)

(use-package counsel-gtags
  :diminish
  :bind-keymap ("C-c g" . counsel-gtags-command-map)
  :load-path "~/gits/emacs-counsel-gtags/"
  :custom
  (counsel-gtags-debug-mode t)
  (counsel-gtags-use-dynamic-list nil)
  :init
  (which-key-add-key-based-replacements "C-c g" "counsel-gtags")
  :config
  (counsel-gtags-mode 1))

(use-package global-tags ;; gtags with xref integration
  :after counsel-gtags
  :demand t
  :bind (:map counsel-gtags-mode-map
	      ("C-c g x c" . global-tags-create-database)
	      ("C-c g x u" . global-tags-update-database))
  :init
  (which-key-add-key-based-replacements "C-c g x" "global-tags")
  :config
  (add-to-list 'xref-backend-functions 'global-tags-xref-backend)
  (add-to-list 'project-find-functions 'global-tags-try-project-root))

(use-package dumb-jump
  :bind ("C-c j" . hydra-dumb-jump/body)
  :init
  (which-key-add-key-based-replacements "C-c j" "hydra/dumb-jump")
  :hydra (hydra-dumb-jump (:color blue :columns 3)
			  "Dumb Jump"
			  ("j" dumb-jump-go "Go")
			  ("o" dumb-jump-go-other-window "Other window")
			  ("e" dumb-jump-go-prefer-external "Go external")
			  ("x" dumb-jump-go-prefer-external-other-window
			   "Go external other window")
			  ("i" dumb-jump-go-prompt "Prompt")
			  ("l" dumb-jump-quick-look "Quick look")
			  ("b" dumb-jump-back "Back"))
  :custom
  (dumb-jump-selector 'ivy)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;__________________________________________________________
;; Magit

(use-package magit
  :defer t
  :custom
  (magit-completing-read-function #'ivy-completing-read) ;; this is autoset
  :config
  ;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (defun my/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (bind-key "q" #'my/magit-kill-buffers magit-status-mode-map)
  )

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
  :custom
  (git-commit-summary-max-length 68)
  :config
  (defun my/git-commit-setup-hook ()
    (setq-local fill-column 72)
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
  :defer t
  :preface
  (defun my/cmake-font-lock ()
    (let ((auto-refresh-defaults (boundp 'font-lock-keywords)))
      (cmake-font-lock-activate)
      (when auto-refresh-defaults
	(font-lock-refresh-defaults))))
  :hook (cmake-mode . my/cmake-font-lock))

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
  (interactive (let ((string (read-shell-command "Shell command on buffer: ")))
		 (if (use-region-p)
		     (list (region-beginning) (region-end) string)
		   (list (point-min) (point-max) string))))
  (save-excursion
    (shell-command-on-region start end command t t)))

(defun filename-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;;__________________________________________________________
;;; Org Mode (I don't use it)

(use-package org :ensure nil
 :mode ("\\.org\\'" . org-mode))

;;__________________________________________________________
;; Move current line up and down M+arrow

(use-package move-dup
  :bind (
	 ("M-<up>" . md-duplicate-up)
	 ("M-<down>" . md-duplicate-down)
	 ("C-M-<up>" .  md-move-lines-up)
	 ("C-M-<down>" . md-move-lines-down)
	 ("C-M-<left>" . (lambda () (interactive) (transpose-words -1)))
	 ("C-M-<right>" . (lambda () (interactive) (transpose-words 1)))
	 ("M-<left>" . (lambda () (interactive) (transpose-chars -1)))
	 ("M-<right>" . (lambda () (interactive) (transpose-chars 1)))))

;;__________________________________________________________
;; evil mode

(use-package avy
  :bind (("C-' C-r" . avy-resume)
	 ("C-' C-'" . avy-goto-char-timer)
	 ("C-' 1" . avy-goto-char)
	 ("C-' 2" . avy-goto-char-2)
	 ("C-' c" . avy-goto-char-in-line)
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
	 ("C-' C-SPC" . avy-pop-mark)
	 ("C-' i" . avy-copy-region)
	 :map isearch-mode-map
	 ("C-'" . avy-isearch))
  :init
  (which-key-add-key-based-replacements "C-'" "avy")
  :custom
  (avy-timeout-seconds 0.75)
  ;; (avy-style 'at-full)  ;; this is already the default
  (avy-all-windows nil)    ;; commands only in this window
  (avy-all-windows-alt t)  ;; with prefix commands in all windows
  (avy-case-fold-search nil)
  (avy-highlight-first t)
  (avy-keys (nconc (number-sequence ?a ?z)	 ;; Order of proposals
		   (number-sequence ?1 ?9)
		   (number-sequence ?A ?Z))))

(use-package avy-zap
  :bind (("M-Z". avy-zap-up-to-char-dwim)
	 ([remap zap-to-char]. avy-zap-to-char-dwim)))

;;__________________________________________________________
;; Arduino Mode

(use-package arduino-mode
  :mode ("\\.ino\\'" "\\.pde\\'"))

(use-package company-arduino
  :after company
  :hook (arduino-mode . company-arduino-turn-on))

(use-package arduino-cli-mode
  :hook (arduino-mode . arduino-cli-mode)
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t)
  (arduino-cli-mode-keymap-prefix "C-c C-a")
  :config
  (which-key-add-key-based-replacements "C-c C-a" "arduino-cli-mode")
)

;;__________________________________________________________
;; Multiple Cursors

(use-package iedit
  :bind (("C-c m i" . iedit-mode)
	 :map iedit-lib-keymap
	 (("C-c m '" .  iedit-toggle-unmatched-lines-visible)))
  :custom
  (iedit-auto-recenter nil))

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

  :custom
  (mc/always-run-for-all t)
  (mc/always-repeat-command t)
  (mc/edit-lines-empty-lines 'ignore))

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

(use-package lice :defer t)

(use-package lorem-ipsum :defer t)
;;__________________________________________________________
;; json mode

(use-package json-mode
  :mode "\\.json\\'")

(use-package flymake-json
  :hook (json-mode . flymake-json-load))

;;__________________________________________________________
;; Modeline
(use-package powerline
  :config
  (powerline-default-theme))

;;__________________________________________________________
;; yaml mode
(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package flymake-yaml
  :hook (yaml-mode . flymake-yaml-load))

(use-package sudo-edit :defer t)

(use-package evil
  :defer t
  :custom
  (evil-esc-delay 0.001)
  (evil-want-keybinding nil)
  (show-paren-when-point-inside-paren t)
  :config
  ;; Modeline color
  (defconst original-background (face-attribute 'mode-line :background))

  (add-hook 'evil-normal-state-entry-hook
	    (lambda ()
	      (set-face-attribute 'mode-line nil :background (named-color brightblack))))
  (add-hook 'evil-insert-state-entry-hook
	    (lambda ()
	      (set-face-attribute 'mode-line nil :background original-background)))
  (add-hook 'evil-visual-state-entry-hook
	    (lambda ()
	      (set-face-attribute 'mode-line nil :background (named-color green)))))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :hook (evil-mode .  evil-collection-init))

(use-package composable
  :diminish
  :after which-key
  :load-path "~/gits/composable.el/"
  :custom
  (composable-copy-active-region-highlight nil)
  (composable-mode-debug-level 0)
  :config
  (composable-mode)       ; Activates the default keybindings
  (composable-mark-mode)) ; Use composable with C-SPC

(use-package slime :defer t
  :custom
  (inferior-lisp-program "sbcl")
  (slime-contribs '(slime-fancy)))

;; Navegacion por objetos... no lo he probado
(use-package objed
  :commands objed-mode)

;; Emacs en less y otros comandos
(use-package e2ansi
  :defer t)

(use-package i3wm-config-mode
  :mode "/i3/config\\'")

;; (use-package ivy-posframe
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
;;   (ivy-posframe-mode 1))

(use-package mutt-mode
  :mode "muttrc")
(provide 'init)

;;; init.el ends here
