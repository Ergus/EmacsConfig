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

;; (setq-default display-line-numbers-widen t)	;; keep line numbers inside a narrow
(global-display-line-numbers-mode t)	;; line numbers on the left
(global-display-fill-column-indicator-mode t)

(savehist-mode t)			;; Historial
(auto-compression-mode t)		;; Uncompress on the fly

(size-indication-mode t)		;; Muestra el tamanno en modeline
(delete-selection-mode t)		;; Sobreescribe seleccion al pegar

(prefer-coding-system 'utf-8)	        ;; Encoding
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(column-number-mode t)			;; Numero de la columna
(line-number-mode t)			;; Numero de linea modeline

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
	      next-screen-context-lines 5   ;; Lines of continuity when scrolling
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
	      ;;minibuffer-message-timeout 1
	      read-quoted-char-radix 16       ;; Read number of chars with C-q
	      kill-buffer-query-functions nil ;; Functions to call before quering a buffer
	                                      ;; Default asks if process running.
	      kill-do-not-save-duplicates t   ;; duplicate kill ring entries

	      eval-expression-print-length nil
	      eval-expression-print-level nil
	      enable-remote-dir-locals t      ;; Open remote dir locals.

	      ;; suggest-key-bindings t       ;; Ivy ya hace lo que esta opcion
	      ;;uniquify-min-dir-content 0
	      truncate-lines t
	      ;; auto-hscroll-mode 'current-line       ;; scroll horizontally 1 line not all
	      save-interprogram-paste-before-kill t ;; Save clipboard before replace
	      minibuffer-eldef-shorten-default t

	      ;; These two must be enabled/disabled together
	      ;; (setq enable-recursive-minibuffers t) ;; Enable nesting in minibuffer
	      ;; (minibuffer-depth-indicate-mode 1)    ;; Mostrar nivel de nesting en minibuffer

	      ;; M-x show context-local commands
	      read-extended-command-predicate  #'command-completion-default-include-p
	      use-short-answers t                 ;; Use y or n to exit and other shorter answers.
	      goto-line-history-local t           ;; Buffer local goto-line history
	      switch-to-buffer-obey-display-actions t ;; switching the buffer respects display actions
	      )

;; Vertical window divider
(dont-compile
  (set-display-table-slot standard-display-table
			  'vertical-border
			  (make-glyph-code ?\u2502)))
;;__________________________________________________________
;; use-package

;; Function to see the dependencies list.
;; (defvar my/require-tree nil)
;; (defun require--advice (orig-fun feature &rest args)
;;   (setq my/require-tree
;;     (append my/require-tree
;;         (list (let ((my/require-tree (list feature)))
;;             (apply orig-fun feature args)
;;             my/require-tree)))))
;; (advice-add 'require :around 'require--advice)

(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ;; Using Melpa and Elpa
				 ("melpa" . "https://melpa.org/packages/"))
	      package-quickstart t)

(when init-file-debug
  (unless (fboundp 'package-installed-p)
    (require 'package))
  (unless (package-installed-p 'use-package)
    (package-initialize)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-and-compile

  ;; Set this BEFORE require use-ackage
  (if init-file-debug
      (setq-default use-package-always-ensure t
		    use-package-enable-imenu-support t
		    use-package-verbose t
		    use-package-expand-minimally nil
		    use-package-compute-statistics t
		    debug-on-error t)

    (setq-default use-package-always-ensure nil
		  use-package-enable-imenu-support nil
		  use-package-verbose nil
		  use-package-expand-minimally t))
  (require 'use-package)

  (defun my/load-path (path)
    "Return the PATH if exist or nil."
    (and (file-exists-p path) path))

  (defmacro my/gen-delay-hook (mode-name)
    "Generate delayed hook for MODE-NAME."

    (let ((funame (intern (format "my/%s-hook" mode-name)))
	  (delayhook (intern (format "%s-delay-hook" mode-name)))
	  (modehook (intern (format "%s-hook" mode-name))))
      `(progn
	 (defvar ,delayhook nil)

	 (defun ,funame ()
	   ,(format "Delayed hook for %s." mode-name)
	   (run-with-idle-timer 1 nil
				(lambda (buf)
				  (when (buffer-live-p buf)
				    (with-current-buffer buf
				      (run-hooks ',delayhook))))
				(current-buffer)))
	 (add-hook ',modehook (function ,funame))))))

;;Commented because define-obsolete-alias api changed.
;;(use-package benchmark-init
;;  :if init-file-debug
;;  :config
;;  (add-hook 'benchmark-init/tree-mode-hook #'hl-line-mode)
;;  (add-hook 'benchmark-init/tabulated-mode-hook #'hl-line-mode)
;;  (add-hook 'window-setup-hook #'benchmark-init/deactivate 0))

(use-package esup :defer t)

;;__________________________________________________________
;; Config file not here to not track it
(setq-default custom-file
	      (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  (message "Creating %s" custom-file))
(load custom-file)

;; Personal Lisp dir
(defconst mylisp-dir (expand-file-name "lisp" user-emacs-directory))

(if (file-exists-p mylisp-dir)
    (progn
      (add-to-list 'load-path mylisp-dir)

      ;; Next file is in my lisp directory. it only defines mu4e
      ;; config and a variable for the gmail calendar. It goes in the
      ;; lisp directory.
      (unless (require 'configmail "configmail.el" t)
	(message "No mail config file found: ignoring")))

  ;; No lisp subdir so ignore.
  (message "Subdir %s does not exist: ignoring" mylisp-dir))

;; System Lisp dir
(defconst syslisp-dir "/usr/share/emacs/site-lisp")
(when (file-exists-p syslisp-dir)
  (add-to-list 'load-path syslisp-dir))


;;__________________________________________________________
;; which-key
(use-package diminish :defer t)   ;; if you use :diminish
(use-package bind-key :defer t)	  ;; if you use any :bind variant

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  ;;(which-key-idle-delay 10000) ;; To not show
  ;;(which-key-show-early-on-C-h t)
  (which-key-idle-secondary-delay 0.01)  ;; nil sets the same delay
  (which-key-dont-use-unicode t)
  ;;(which-key-popup-type 'minibuffer)
  ;;(which-key-separator ": ") ;which-key-idle-delay 2.0)
  :config
  (which-key-mode t)
  (which-key-add-key-based-replacements
    "C-c h" "highlight"
    "C-c b" "bars"
    "C-x r" "rectangle||register"
    "C-x n" "narrow"
    "C-x t" "tabs"
    "C-x a" "abbrev"))

;;__________________________________________________________
;; Some internal packages to defer them

(use-package uniquify :ensure nil
  :defer 2
  :custom
  (uniquify-buffer-name-style 'post-forward))

(use-package saveplace :ensure nil
  :custom
  ;; (save-place-forget-unreadable-files t)
  (save-place-ignore-files-regexp  ;; Modified to add /tmp/* files
   "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\|^/tmp/.+\\)$")
  :config
  (save-place-mode 1))              ;; Remember point in files

(use-package autorevert :ensure nil
  :defer 2
  :custom
  (auto-revert-verbose nil)	 ;; not show message when file changes
  (auto-revert-avoid-polling t)  ;; don't do pooling for autorevert (use notifications).
  ;;(auto-revert-remote-files t) ;; No autorevert tramp files
  :config
  (global-auto-revert-mode t))		;; Autoload files changed in disk

(use-package paren :ensure nil
  :defer 1
  :custom
  (show-paren-delay 0)
  (blink-matching-paren nil)	;; not show message when file changes
  :config
  (show-paren-mode t))

(use-package smtpmail :ensure nil
  :defer t
  :custom
  (smtpmail-debug-info t))

(use-package sendmail :ensure nil
  :defer t
  :custom
  (send-mail-function #'smtpmail-send-it))

(use-package profiler :ensure nil
  :defer t
  :hook (profiler-report-mode . hl-line-mode))

;; Shows the function in spaceline
(use-package which-func :ensure nil
  :diminish
  :defer t)

(use-package text-mode :ensure nil
  :preface
  (my/gen-delay-hook text-mode)
  :defer t)

(use-package prog-mode :ensure nil
  :preface
  (my/gen-delay-hook prog-mode)
  :defer t
  :hook (prog-mode-delay . (lambda ()
			     (setq show-trailing-whitespace t))))

(use-package elec-pair :ensure nil
  :hook ((prog-mode text-mode) . (lambda ()
				  (electric-pair-local-mode 1)))
  :defer t)

(use-package hl-line :ensure nil
  :diminish
  :bind ("C-c h l" . hl-line-mode)
  :hook (package-menu-mode . hl-line-mode)
  :defer t)

(use-package winner :ensure nil
  :bind (:map winner-mode-map
	      ("C-x w u" . winner-undo)
	      ("C-x w r" . winner-redo))
  :defer 1  ;; this always after the bind
  :custom
  (winner-dont-bind-my-keys t)
  :init
  (which-key-add-key-based-replacements "C-c w" "winner")
  :config
  (winner-mode 1))

(use-package org :ensure nil
  :mode ("\\.org\\'" . org-mode))

(use-package abbrev :ensure nil ;; Abbrev M-/
  :diminish
  :defer t)

(use-package eldoc :ensure nil ;; function arguments
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode) .
	 turn-on-eldoc-mode)
  :custom
  (eldoc-idle-delay 2)                             ;; default 0.5
  (eldoc-print-after-edit t)                       ;; only show after edit
  (eldoc-echo-area-display-truncation-message nil) ;; Not verbose when truncated
  :defer t
  :config
  (global-eldoc-mode -1))  ;; This is enabled by default, disable it

(use-package gdb :ensure nil
  :defer t
  :custom
  (gdb-debug-log-max nil)   ;; no limit log
  (gdb-many-windows nil)
  (gdb-show-main t))

(use-package conf-mode :ensure nil
  :defer t
  :preface
  (my/gen-delay-hook conf-mode))

;;__________________________________________________________
;; Benchmark-init

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
  :bind (:map isearch-mode-map
	      ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (search-nonincremental-instead nil) ;; No incremental if enter with empty
  (lazy-highlight-initial-delay 0)
  (isearch-allow-scroll t)	      ;; Permit scroll can be 'unlimited
  (isearch-lazy-count t)
  (search-ring-max 64)                ;; Cuantas busquedas recordar
  (regexp-search-ring-max 64)
  ;;(search-exit-option 'edit)        ;; Control or meta keys edit search
  (isearch-yank-on-move 'shift)       ;; Copy text from buffer with meta
  :config
  (defun my/goto-match-beginning ()
    (when (and isearch-forward
	       isearch-other-end
	       (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end)))

  (add-hook 'isearch-mode-end-hook #'my/goto-match-beginning))

(use-package phi-search :defer t)

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

(defalias 'my/named-color 'simple-16-theme-color)

;;__________________________________________________________
;;Packages options
;;__________________________________________________________

;;__________________________________________________________
;; Show paren mode

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
  (tramp-completion-use-auth-sources nil)
  (remote-file-name-inhibit-cache 120)           ;; Default 10
  (tramp-completion-reread-directory-timeout 120);; Default 10
  (password-cache-expiry 3600)                   ;; Cache for 1 hour
  (tramp-default-method "scp")
  ;; (tramp-debug-buffer t)
  ;; (tramp-verbose 10)
  ;; (tramp-persistency-file-name         ;; this is already the default
  ;;  (expand-file-name "tramp" user-emacs-directory))
  :config
  ;; Disable vc checking tramp
  (setq vc-ignore-dir-regexp (format "%s\\|%s"
				     vc-ignore-dir-regexp
				     tramp-file-name-regexp))

  (connection-local-set-profile-variables
   'my/tramp-profile '((auth-sources . nil)
		       (tramp-use-ssh-controlmaster-options . nil)))

  (connection-local-set-profiles
   '(:application tramp) 'my/tramp-profile)

  ;;(tramp-change-syntax 'simplified)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-process-environment
               (format "DISPLAY=%s" (getenv "DISPLAY"))))

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
  (tab-bar-show 1)
  )

;;__________________________________________________________
;; minibuffers

(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (setq gc-cons-threshold my/gc-cons-threshold)))

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
  :bind-keymap ("C-c d" . vdiff-mode-prefix-map)
  :custom
  (vdiff-auto-refine t)
  :init
  (which-key-add-key-based-replacements "C-c d" "vdiff"))

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
  :init
  (which-key-add-key-based-replacements "C-c t" "term")
  :config
  ;; Add find-file-other-window to accepted commands
  (add-to-list 'vterm-eval-cmds
	       '("find-file-other-window" find-file-other-window)))

(use-package vterm-toggle
  :bind (("C-c t t" . vterm-toggle-cd)
	 :map vterm-mode-map
	 (("<C-return>" . vterm-toggle-insert-cd)
	  ("C-M-n" . vterm-toggle-forward)
	  ("C-M-p" . vterm-toggle-backward)))
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-project-root t)
  (vterm-toggle-fullscreen-p nil)
  :config
  ;; Show at bottom
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _)
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

(use-package xclip
  :preface
  (setq-default xclip-method
		(and (not (or (display-graphic-p)        ;; checks
			      (string-equal (getenv "TERM") "linux")))
		     (or (and (getenv "DISPLAY")         ;; x11
			      (executable-find "xclip")
			      'xclip)
			 (and (getenv "WAYLAND_DISPLAY") ;; wayland
			      (executable-find "wl-copy")
			      'wl-copy))))
  :if xclip-method
  :defer 2
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
  ;; (defun track-mouse (e))
  (set-cursor-color "white")
  (set-mouse-color "white")		  ;; Flechita del mouse en blanco
  (if (fboundp 'mouse-wheel-mode)
      (progn
	(setq-default mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control)))
		      mouse-wheel-progressive-speed nil)
	(mouse-wheel-mode t))

    ;; Else set them manually
    (global-set-key (kbd "<mouse-4>") #'scroll-down-command)
    (global-set-key (kbd "<mouse-5>") #'scroll-up-command))	;; scrolling con el mouse
  )


(defun my/scroll-up-command (&optional arg)
  "Scroll up single line or ARG."
  (interactive "^p")
  (scroll-up-command arg))

(defun my/scroll-down-command (&optional arg)
  "Scroll down single line or ARG."
  (interactive "^p")
  (scroll-down-command arg))

(global-set-key [remap scroll-up-command] #'my/scroll-up-command)
(global-set-key [remap scroll-down-command] #'my/scroll-down-command)
;;__________________________________________________________
;; My program's mode hooks

(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (<= oldpos (point))
	 (/= (line-beginning-position) oldpos)
	 (beginning-of-line))))

(global-set-key [remap move-beginning-of-line] #'my/smart-beginning-of-line)

;;__________________________________________________________
;; Undo tree

(global-set-key [remap undo] #'undo-only)
(global-set-key (kbd "C-M-_") #'undo-redo)
(global-set-key (kbd "C-M-/") #'undo-redo)

(use-package string-inflection
  :bind ("C-c <right>" . string-inflection-all-cycle))

(use-package undo-propose
  :commands undo-propose)

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
		      :foreground (my/named-color brightblack)))

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
  :bind ("C-c h s" . hes-mode))

;;__________________________________________________________
;; Flyspell (Orthography)

(use-package ispell :ensure nil
  :custom
  (ispell-following-word t)  ;;Check word around point not only before
  (ispell-quietly t)         ;; Supress messages in ispell-word
  :defer t)

(use-package flyspell :ensure nil
  :diminish
  :hook ((prog-mode-delay . flyspell-prog-mode)
	 (text-mode-delay . turn-on-flyspell))
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
  :defer t
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

(use-package eglot :defer t)

(use-package company
  ;; :load-path (lambda () (my/load-path "~/gits/company-mode/"))
  :preface
  (defmacro my/company-backend-after-load (backend)
    `(with-eval-after-load 'company
       (unless (eq ,backend (car company-backends))
	 (setq-local company-backends
		     (cons ,backend (remove ,backend company-backends))))))


  :bind (:map company-mode-map
	      ("M-RET" . company-complete)
	      ("M-/" . company-other-backend)                   ;; M-/
	      ;; ([remap indent-for-tab-command] . company-indent-or-complete-common) ;; TAB
	      :map company-active-map
	      ("M-RET" . company-abort)
	      ([remap dabbrev-expand] . company-other-backend)  ;; M-/
	      ;;("C-h" . company-show-doc-buffer)               ;; this is already default
	      ([remap xref-find-definitions] . company-show-location) ;; M-.
	      )
  :hook ((prog-mode-delay message-mode-delay conf-mode-delay)
	 . (lambda ()
	     (unless (bound-and-true-p company-mode)
	       (company-mode 1))))
  :defer t
  :custom
  (company-idle-delay nil)	 ;; no delay for autocomplete
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around nil)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-format-margin-function #'company-detect-icons-margin)
  ;;company-tooltip-limit 20
  (company-backends '(company-capf       ;; completion at point
		      company-semantic
		      company-files	 ;; company files
		      (company-dabbrev-code company-gtags company-keywords)
		      company-dabbrev)))

(use-package lsp-mode
  :diminish lsp
  :hook (lsp-mode . (lambda ()
		      (my/company-backend-after-load #'company-capf)
		      (lsp-enable-which-key-integration)))
  :defer t
  :custom
  (lsp-keymap-prefix (kbd "C-c l"))
  (lsp-enable-snippet nil)
  (lsp-eldoc-hook nil)
  (lsp-enable-indentation nil)
  (lsp-prefer-capf t)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  ;; lsp-diagnostic-package t ;; prefer flymake
  ;;:config
  ;; TODO: extend this for more languages, and find a way to set this for a project
  ;; (when (memq major-mode '(c-mode c++-mode))
  ;;   (add-hook 'c-mode-common-hook #'lsp-deferred))

  ;; (when (eq major-mode 'python-mode)
  ;;   (add-hook 'python-mode-hook #'lsp-deferred))
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
    (subword-mode 1)
    (message "Loaded my/c-mode-common"))
  :hook (c-mode-common . my/c-mode-common-hook)
  :defer t
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
		 (c-hanging-semi&comma-criteria . nil)
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
  :custom
  (preproc-font-lock-preprocessor-background-face 'font-lock-preprocessor-face)
  :defer t)

;; company-c-headers
(use-package company-c-headers
  :hook ((c-mode c++-mode objc-mode) . (lambda ()
					 (my/company-backend-after-load #'company-c-headers)))
  :defer t)


(use-package clang-format
  :commands clang-format-region)

;;__________________________________________________________
;; C++ mode
(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode)
  :defer t)

;;__________________________________________________________
;; elisp mode (all after the company declaration)

(use-package emacs-lisp-mode :ensure nil
  :hook (emacs-lisp-mode . (lambda ()
			     ;; emacs-lisp-mode is evaluated in too many places...
			     (when (and buffer-file-name
					(string-match "\\.el\\'" buffer-file-name))
			       (my/company-backend-after-load #'company-elisp))))
  :defer t)
;;__________________________________________________________
;; sh mode

(defvaralias 'sh-basic-offset 'tab-width)

(add-hook 'sh-mode-hook (lambda ()
			  (setq-local indent-tabs-mode t
				      tab-width 4)))

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
  :hook rst-mode
  :defer t)

;;__________________________________________________________
;; ruby-mode
(use-package ruby-mode :ensure nil
  :mode ("\\.rjs\\'")
  :custom
  (ruby-indent-level 2))

(use-package ruby-tools
  :hook (ruby-mode . ruby-tools-mode)
  :defer t)

(use-package ruby-electric
  :hook (ruby-mode . ruby-electric-mode)
  :defer t)

;;__________________________________________________________
;; Julia Mode
(use-package julia-mode
  :mode "\\.jl\\'")

(use-package flycheck-julia
  :hook (julia-mode . flycheck-julia-setup)
  :defer t)

;;__________________________________________________________
;; Rust Mode
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup)
  :defer t)

;;__________________________________________________________
;; Ocaml Mode
(use-package caml-mode :ensure caml
  :mode "\\.ml[iylp]?\\'")

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
  :hook (lua-mode . (lambda ()
		      (my/company-backend-after-load #'company-lua)))
  :defer t)

;;__________________________________________________________
;; groovy language

(use-package groovy-mode
  :defer t)

;;__________________________________________________________
;; systemd mode
(use-package systemd :defer t)

;;__________________________________________________________
;; Use for Qt's .pro and .pri files
(use-package qt-pro-mode
  :mode (("\\.pr[io]\\'" . qt-pro-mode)
	 ("\\.moc\\'" . c++-mode)
	 ("\\.ui\\'" . xml-mode)))

;;__________________________________________________________
;; javascript-mode
;; (use-package js-mode :ensure nil
;;   :mode ("\\.js\\'"))

;;__________________________________________________________
;; xml-mode
(use-package xml-mode :ensure nil
  :mode ("\\.\\(ipe\\|qrc\\|svn\\)\\'"))

;;__________________________________________________________
;; splitting

(use-package windmove :ensure nil
  :bind (:map ctl-x-map
	      ("4 <left>" . windmove-display-left)
	      ("4 <right>" . windmove-display-right)
	      ("4 <up>" . windmove-display-up)
	      ("4 <down>" . windmove-display-down)

	      ("<left>" . windmove-left)
	      ("<right>" . windmove-right)
	      ("<down>" . windmove-down)
	      ("<up>" . windmove-up)
	      ("C-M-<left>" . windmove-swap-states-left)
	      ("C-M-<right>" . windmove-swap-states-right)
	      ("C-M-<down>" . windmove-swap-states-down)
	      ("C-M-<up>" . windmove-swap-states-up)))

(use-package repeat :ensure nil
  :defer 1
  :config
  (repeat-mode 1))

;; Change color selected buffers
;; (use-package auto-dim-other-buffers
;;   :defer t
;;   :custom
;;   (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
;;   (auto-dim-other-buffers-dim-on-focus-out t))

;;__________________________________________________________
;; Lines enabling gnuplot-mode
;; (use-package gnuplot-mode
;;   :mode ("\\.gp\\'" "\\.gpl\\'" "\\.plt\\'"))

(use-package gnuplot
  :mode ("\\.\\(gpl?\\|plt\\)\\'" . gnuplot-mode))

;;__________________________________________________________
;; Auto completamiento

(use-package yasnippet        ;; Snippets
  :diminish yas-minor-mode
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
  :defer 2
  :init
  (which-key-add-key-based-replacements "C-c y" "yasnippet")
  :custom
  (yas-verbosity 1)                 ; No need to be so verbose
  (yas-wrap-around-region t)
  :config
  ;; (yas-reload-all)
  ;; (yas-minor-mode 1)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

;;__________________________________________________________
;; Chequeo de syntaxis
(use-package flycheck
  :diminish
  :if (< (buffer-size) 200000)
  :preface
  (defun flycheck-mode-on ()
    (flycheck-mode 1))
  :hook (prog-mode-delay . flycheck-mode-on)
  :bind-keymap ("C-c a" . flycheck-command-map)
  :bind (:map flycheck-command-map
	      (("a" . counsel-flycheck)))
  :defer t
  :init
  (which-key-add-key-based-replacements "C-c a" "flycheck")
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
  ;; :hook (prog-mode-delay . flymake-mode-on)
  :defer t
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (which-key-add-key-based-replacements "C-c k" "flymake")

  (easy-mmode-defmap flymake-basic-map
    `(("n" . flymake-goto-next-error)
      ("p" . flymake-goto-prev-error)
      ("d" . flymake-show-diagnostic)
      ("b" . flymake-show-diagnostic-buffer)
      ("l" . flymake-switch-to-log-buffer))
    "The base keymap for `flymake-mode'.")

  (define-key flymake-mode-map (kbd "C-c k") flymake-basic-map))

;;__________________________________________________________
;; Improved help buffer

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

;; Asocia buffers que empiecen con messaje mode
(use-package message-mode :ensure nil
  :mode ("/neomut" "neomutt-Ergus-" "draft")
  :preface
  (my/gen-delay-hook message-mode)
  :custom
  (message-default-mail-headers "Cc: \nBcc: \n")
  (message-kill-buffer-on-exit t)
  (message-send-mail-function #'message-use-send-mail-function)
  (mail-header-separator "")
  :config
  (auto-fill-mode t)
  (mail-abbrevs-setup)
  (flyspell-mode t))

(use-package notmuch
  :init
  (setenv "NOTMUCH_CONFIG" "/home/ergo/almacen/mail/notmuch-config")
  :hook (message-mode . (lambda ()
			  (my/company-backend-after-load #'notmuch-company)))
  :defer t)

;;__________________________________________________________
;; Latex mode

(use-package tex :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook (LaTeX-mode . (lambda ()
			(flyspell-mode 1)
			(visual-line-mode 1)
			(auto-fill-mode 1)))
  :custom
  (TeX-source-correlate-start-server t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (LaTeX-babel-hyphen nil)
  (TeX-master nil) ;; Multidocument
  (LaTeX-indent-level 4)
  (LaTeX-item-indent 0)

  :config
  (TeX-source-correlate-mode 1)

  (add-to-list 'TeX-command-list
  	       '("Makeglossaries" "makeglossaries %s" TeX-run-command nil
  		 (latex-mode)
  		 :help "Run makeglossaries, will choose xindy or makeindex") t)

  ;; ====== Fix for itemize indentation.

  (with-eval-after-load 'latex
    (defun my/LaTeX-indent-item ()
      "Syntactic indentation for itemize like environments to add extra offsets."
      (save-match-data
	(let* ((offset (+ LaTeX-indent-level LaTeX-item-indent))
               (re-beg "\\\\begin{")
               (re-end "\\\\end{")
               (re-env "\\(itemize\\|\\enumerate\\|description\\)")
               (indent (save-excursion                                 ;; parent indent column
			 (when (looking-at (concat re-beg re-env "}"))
			   (end-of-line))
			 (LaTeX-find-matching-begin)
			 (current-column))))
	  (cond
	   ((looking-at (concat re-beg re-env "}"))               ;; row with \begin{itemize}
	    (or (save-excursion
                  (beginning-of-line)
                  (ignore-errors
		    (LaTeX-find-matching-begin)
		    (+ (current-column)                            ;; parent indentation pos
		       (if (looking-at (concat re-beg re-env "}")) ;; check if parent scope is also itemize
			   offset
			 LaTeX-indent-level)))
		  indent)))
	   ((looking-at (concat re-end re-env "}"))     ;; row with \end{itemize}
	    indent)
	   ((looking-at "\\\\item")                     ;; row with \item
	    (+ indent offset))
	   (t                                           ;; any other row (continuation)
	    (+ indent offset LaTeX-indent-level))))))

    (add-to-list 'LaTeX-indent-environment-list '("itemize" my/LaTeX-indent-item))
    (add-to-list 'LaTeX-indent-environment-list '("enumerate" my/LaTeX-indent-item))
    (add-to-list 'LaTeX-indent-environment-list '("description" my/LaTeX-indent-item)))
  ;; =========================

  )

(use-package auctex-latexmk
  :defer t
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package company-math
  :hook (TeX-mode . (lambda ()
		      (my/company-backend-after-load
		       '(company-math-symbols-latex company-latex-commands))))
  :defer t)

(use-package company-auctex
  :hook (TeX-mode . (lambda ()
		      (my/company-backend-after-load #'company-auctex-init)))
  :defer t)

(use-package reftex :ensure nil ;; Reftex for cross references
  :hook (LaTeX-mode . turn-on-reftex)  ;; with AUCTeX LaTeX mode
  :defer t
  :custom
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
  :hook (reftex-mode . (lambda ()
			 (my/company-backend-after-load
			  '(company-reftex-labels company-reftex-citations))))
  :defer t)

;;__________________________________________________________
;;bibtex mode set use biblatex
(use-package bibtex :ensure nil
  :mode (("\\.bib\\'" . bibtex-mode))
  :custom
  (bibtex-dialect 'biblatex))

(use-package company-bibtex
  :hook (bibtex-mode . (lambda ()
			 (my/company-backend-after-load #'company-bibtex)))
  :defer t)

(use-package ivy-bibtex
  :defer t
  :custom
  (ivy-bibtex-default-action #'bibtex-completion-insert-citation))

;;__________________________________________________________
;; Python mode


(use-package python :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
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
  :bind (:map dired-mode-map
	 ([remap dired-find-file] . dired-find-alternate-file)
	 ([remap dired-up-directory] . (lambda ()
					 (interactive)
					 (find-alternate-file ".."))))
  :custom
  (dired-recursive-copies 'top)	     ;; Always ask recursive copy
  (dired-recursive-deletes 'top)     ;; Always ask recursive delete
  (dired-dwim-target t)		     ;; Copy in split mode with p
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alh")
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x :ensure nil
  :hook dired
  :defer t)

(use-package dired-sidebar
  :bind ("C-c b d" . dired-sidebar-toggle-sidebar)
  :custom
  ;;(dired-sidebar-use-term-integration t)
  (dired-sidebar-theme 'nerd)
  (dired-sidebar-subtree-line-prefix "."))

;; du for direct. Get file sizes..
;; it is too slow, so don't use it please.
;; (use-package dired-du
;;   :custom
;;   (dired-du-size-format t)
;;   :commands dired-du-mode)


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
  :bind ([remap list-buffers] . ibuffer)
  :hook (ibuffer-mode . (lambda ()
			  ;; autorefresh. Not enable when tramp is loaded.
			  ;; (if (featurep 'tramp)
			  ;;     (message "ibuffer-auto-mode not enabled.")
			  ;;   (ibuffer-auto-mode 1))
			  (hl-line-mode 1)))
  :custom
  (ibuffer-default-sorting-mode 'alphabetic)  ;; can use recency
  )

(use-package ibuffer-sidebar
  :bind (("C-c b b" . ibuffer-sidebar-toggle-sidebar)))

(use-package ibuffer-tramp
  :after ibuffer
  :bind (:map ibuffer--filter-map
	      ("G t" . ibuffer-tramp-set-filter-groups-by-tramp-connection)))

(use-package ibuffer-project
  :after ibuffer
  :preface
  (defun ibuffer-set-filter-groups-by-project ()
    (interactive)
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (ibuffer-update nil t))
  :bind (:map ibuffer--filter-map
	      ("G p" . ibuffer-set-filter-groups-by-project)))

(use-package ibuffer-vc
  :after ibuffer
  :bind (:map ibuffer--filter-map
	      ("G v" . ibuffer-vc-set-filter-groups-by-vc-root)))

;; Sidebar Dired+ibuffer (de emacs defaults)
(defun my/sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (dired-sidebar-toggle-sidebar))

(global-set-key (kbd "C-c b s") #'my/sidebar-toggle)

;;__________________________________________________________
;; neotree
;; Like dired sidebar but a bit fancier.
(use-package neotree
  :bind ("C-c b n" . neotree-toggle))

;;__________________________________________________________
;; Ivy (probare un tiempo con helm/ivy)

(use-package headlong :defer t)

(use-package flx :defer t)

(use-package ivy
  :diminish
  :bind (:map ivy-mode-map
	      ("C-c c c" . ivy-resume)
	      :map ivy-minibuffer-map
	      ("TAB" . ivy-partial)
	      ("RET" . ivy-alt-done))
  :init
  (which-key-add-key-based-replacements "C-c i" "ivy")
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-pulse-delay nil)
  (ivy-use-selectable-prompt t)
  (ivy-fixed-height-minibuffer t)
  (ivy-on-del-error-function #'ignore)
  (ivy-read-action-format-function #'ivy-read-action-format-columns)
  ;; (ivy-use-virtual-buffers t)   ;; Recent files or buffers in ivy
  ;; (ivy-height 10)
  ;; (ivy-wrap t)		      ;; cycle in minibuffer
  :config
  ;; Highlight with arrows by default.
  ;;(add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-arrow))
  (ivy-mode 1))

(use-package ivy-hydra :defer t
  :custom
  (ivy-read-action-function #'ivy-hydra-read-action))

(use-package ivy-avy :after ivy)

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
  :bind (;;([remap isearch-forward] . swiper-isearch)
	 ;;([remap isearch-backward] . swiper-isearch-backward)
	 ;;([remap isearch-forward-symbol-at-point] . swiper-isearch-thing-at-point)
	 :map swiper-map
	 ;; TODO: Find bindings for swiper-all and swiper-all-thing-at-point
	 ;;("M-q" . swiper-query-replace)   ;; Already default
	 ("C-o" . swiper-isearch-toggle)
	 :map isearch-mode-map
	 ("C-o" . swiper-isearch-toggle))
  :custom
  (swiper-goto-start-of-match t)
  ;; :config
  ;; (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper-isearch . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper-isearch-backward . ivy--regex-plus))
  )

(use-package imenu-anywhere
  :bind (("C-c i i" . ivy-imenu-anywhere)
	 ("C-c i c" . counsel-imenu))
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 200))

(use-package imenu-list
  :bind ("C-c b i" . imenu-list-smart-toggle)
  :custom
  (imenu-list-position 'left))

(use-package counsel
  :diminish
  :bind (:map counsel-mode-map
	      (;;([remap switch-to-buffer] . counsel-switch-buffer)
	       ;;([remap switch-to-buffer-other-window] . counsel-switch-buffer-other-window)
	       ("C-c c c" . ivy-resume)             ;; resume ivy
	       ("C-c c a" . counsel-ag)
	       ("C-c c b" . counsel-ibuffer)        ;; like ibuffer + switch-to-buffer
	       ("C-c c i" . counsel-imenu)
	       ("C-c c r" . counsel-rg)	            ;; like git grep
	       ("C-c c g" . counsel-grep)           ;; grep in local file
	       ("C-c c G" . counsel-git-grep)       ;; grep in current git repo
	       ("C-c c e" . counsel-linux-app)      ;; call application
	       ("C-c c l" . counsel-find-library)   ;; Search lisp libraries
	       ("C-c c SPC" . counsel-register)     ;; list registers
	       ("C-c c M-RET" . counsel-company)    ;; company completions
	       ("C-c c C-r" . counsel-command-history) ;; command history
	       ("C-c c p" . counsel-package)        ;; command history
	       ("C-c c P" . counsel-list-processes)        ;; command history
	       ;; Find file commands
	       ("C-c c f g" . counsel-git)          ;; find file in git rempo
	       ("C-c c f j" . counsel-file-jump)    ;; file in subdir
	       ("C-c c f l" . counsel-locate)       ;; locate command (como search)
	       ("C-c c f r" . counsel-recentf)
	       ("C-c c f z" . counsel-fzf)
	       ("C-c c f b" . counsel-buffer-or-recentf)))
  :defer 0.25
  :init
  (which-key-add-key-based-replacements "C-c c" "counsel")
  (which-key-add-key-based-replacements "C-c c f" "find-file")
  :custom
  (counsel-find-file-at-point t)       ;; Select file at point
  (counsel-preselect-current-file t)   ;; Select current file in list
  :config
  (counsel-mode 1)
  ;; match by words
  ;; (add-to-list 'ivy-re-builders-alist '(counsel-M-x . ivy--regex-fuzzy))
  )

(use-package amx :defer t) ;; Complete history
(use-package recentf :ensure nil
  :defer t
  :config
  (recentf-mode 1)) ;; Complete history


(use-package counsel-gtags
  :diminish
  :load-path (lambda () (my/load-path "~/gits/emacs_clones/emacs-counsel-gtags/"))
  :bind-keymap ("C-c g" . counsel-gtags-command-map)
  :hook (counsel-gtags . my/counsel-gtags-hook)
  :custom
  (counsel-gtags-debug-mode t)
  (counsel-gtags-use-dynamic-list nil)
  :init
  (which-key-add-key-based-replacements "C-c g" "counsel-gtags")
  :config
  (defun my/counsel-gtags-hook ()
    (my/company-backend-after-load #'company-gtags))

  ;; Promote company-gtags to the beginning.
  (add-hook 'counsel-gtags-mode-hook #'my/counsel-gtags-hook)
  (add-hook 'c-mode-hook #'my/counsel-gtags-hook)
  (add-hook 'c++-mode-hook #'my/counsel-gtags-hook)
  (add-hook 'objc-mode-hook #'my/counsel-gtags-hook)

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
  (add-to-list 'xref-backend-functions #'global-tags-xref-backend)
  (add-to-list 'project-find-functions #'global-tags-try-project-root))

(use-package dumb-jump
  :bind-keymap ("C-c j" . dumb-jump-mode-map)
  :init
  (which-key-add-key-based-replacements "C-c j" "dumb-jump")
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-disable-obsolete-warnings t)
  :bind (:map dumb-jump-mode-map
	      (("j" . dumb-jump-go)
	       ("o" . dumb-jump-go-other-window)
	       ("e" . dumb-jump-go-prefer-external)
	       ("4" . dumb-jump-go-prefer-external-other-window)
	       ("i" . dumb-jump-go-prompt)
	       ("q" . dumb-jump-quick-look)
	       ("b" . dumb-jump-back)))
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;__________________________________________________________
;; Magit and git packages

(use-package magit
  :defer t
  :custom
  (magit-completing-read-function #'ivy-completing-read) ;; this is autoset
  (magit-define-global-key-bindings nil)
  :config
  ;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (add-hook 'magit-log-mode-hook (lambda ()
				   (setq-local show-trailing-whitespace nil
					       tab-width 4)))

  (defun my/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (define-key magit-status-mode-map "j" #'my/magit-kill-buffers))

(use-package gitattributes-mode
  :mode "\\.gitattributes\\'")

(use-package gitconfig-mode
  :mode "\\.gitconfig\\'")

(use-package gitignore-mode
  :mode "\\.gitignore\\'")

(use-package git-timemachine
  :defer t)

(use-package git-commit
  :defer t
  :mode ("COMMIT_EDITMSG" . git-commit-setup)
  :custom
  (git-commit-summary-max-length 68)
  :config
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)

  (add-hook 'git-commit-setup-hook (lambda ()
				     (setq-local fill-column 72)
				     (git-commit-turn-on-flyspell))))

(use-package smerge-mode :ensure nil
  :defer t
  :preface
  (defun my/enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
	(smerge-mode 1))))

  :hook ((find-file magit-diff-visit-file) . my/enable-smerge-maybe)
  :custom
  (smerge-diff-buffer-name "*smerge-diff*")
  :bind-keymap ("C-c s" . smerge-basic-map)
  :config
  (which-key-add-key-based-replacements "C-c s" "smerge"))

(use-package diff-hl
  :preface
  (defun my/diff-hl-mode ()
    (when (or (and buffer-file-name
		   (not (file-remote-p buffer-file-name)))
	      (eq major-mode 'vc-dir-mode))
      (turn-on-diff-hl-mode)
      (unless (display-graphic-p)
	(diff-hl-margin-mode 1))))
  :hook ((prog-mode-delay . my/diff-hl-mode)
	 (vc-dir-mode . my/diff-hl-mode)
	 (dired-mode . diff-hl-dired-mode-unless-remote))
  :config
  ;; Add the hook only after the package is loaded because they are not autoloads.
  (add-hook 'magit-pre-refresh-hook (lambda ()
				      (unless (file-remote-p default-directory)
					(diff-hl-magit-pre-refresh))))
  (add-hook 'magit-post-refresh-hook (lambda ()
				       (unless (file-remote-p default-directory)
					 (diff-hl-magit-post-refresh)))))

;;__________________________________________________________
;; Ensamblador nasm
(use-package nasm-mode
  :mode ("\\.asm\\'" "\\.s\\'"))

(use-package flymake-nasm
  :hook (asm-mode-hook . flymake-nasm-setup)
  :defer t)

;;__________________________________________________________
;; CMake
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\(.in\)?\\'")
  :commands company-cmake
  :hook (cmake-mode . (lambda ()
			(my/company-backend-after-load #'company-cmake)))
  :defer t)

(use-package cmake-font-lock
  :defer t
  :preface
  (defun my/cmake-font-lock ()
    (let ((auto-refresh-defaults (boundp 'font-lock-keywords)))
      (cmake-font-lock-activate)
      (when auto-refresh-defaults
	(font-lock-refresh-defaults))))
  :hook (cmake-mode . my/cmake-font-lock)
  :defer t)

;; (use-package eldoc-cmake
;;   :hook (cmake-mode . (lambda ()
;; 			(run-with-idle-timer 1 nil #'eldoc-cmake-enable)))
;;   :defer t)

;;__________________________________________________________
;; Cobol
(use-package cobol-mode
  :mode ("\\.\\(cobc?\\|cbl\\|cpy\\)\\'"))

;;__________________________________________________________
;; path

(defun my/point-to-abs (point)
  "Count number of not whitespaces characters preceding POINT."
  (save-excursion
    (goto-char point)
    (let ((counter 0))
      (while (not (bobp))
	(skip-chars-backward " \t\n")
	(setq counter (- counter (skip-chars-backward "^ \t\n"))))
      counter)))

(defun my/abs-to-point (value)
  "First buffer position after VALUE non whitespaces chars."
  (save-excursion
    (let ((counter value)
	  (point-min (point-min)))
      (goto-char point-min)
      (while (and (> counter 0)
		  (not (eobp)))
	(setq counter (- counter (skip-chars-forward "^ \t\n")))
	(cond ((> counter 0)
	       (skip-chars-forward " \t\n"))
	      ((< counter 0)
	       (backward-char (- counter))))))
    (point)))

(defun my/shell-command-on-buffer (command)
  "Execute shell COMMAND on buffer overwriting it but preserve
position."
  (interactive (list (read-shell-command "Shell command on buffer: ")))

  (let ((abspoint (my/point-to-abs (point)))
	(absmark (and (region-active-p)
		      (my/point-to-abs (mark))))
	(deactivate-mark t)
	(output (save-excursion
		  (shell-command-on-region (point-min) (point-max) command t t))))

    (when absmark
      (set-mark (my/abs-to-point absmark))
      (activate-mark))

    (goto-char (my/abs-to-point abspoint))))

(defun my/var-to-clipboard ()
  "Put the current file name on the clipboard."
  (interactive)
  (ivy-read "Describe variable: " obarray
	    :predicate #'counsel--variable-p
	    :require-match t
	    :preselect (ivy-thing-at-point)
	    :action (lambda (x)
		      (let ((value (format "%s" (symbol-value (intern x)))))
			(kill-new value)
			(message "Copied %s value %s to clipboard"
				 x value)))
	    :caller 'my/var-to-clipboard))

;;__________________________________________________________
;; Move current line up and down M+arrow

(use-package move-dup
  :bind (("M-<up>" . move-dup-duplicate-up)
	 ("M-<down>" . move-dup-duplicate-down)
	 ("C-M-<up>" .  move-dup-move-lines-up)
	 ("C-M-<down>" . move-dup-move-lines-down)
	 ("C-M-<left>" . (lambda () (interactive) (transpose-words -1)))
	 ("C-M-<right>" . transpose-words)
	 ("M-<left>" . (lambda () (interactive) (transpose-chars -1)))
	 ("M-<right>" . transpose-chars)))

;;__________________________________________________________
;; evil mode

(use-package avy
  :preface
  (easy-mmode-defmap avy-basic-map
    `(("r" . avy-resume)
      (,(kbd "C-'") . avy-goto-char-timer)
      ("'" . avy-goto-char-timer)
      ("c" . avy-goto-char)
      ("2" . avy-goto-char-2)
      ("\C-f" . avy-goto-char-in-line)
      ("w" . avy-goto-word-or-subword-1) ;; Alternative avy-goto-word
      ("W" . avy-goto-word-0)            ;; All words, no initial char
      ("\C-s" . avy-goto-word-1-below)
      ("\C-r" . avy-goto-word-1-above)
      ("\M-b" . avy-goto-word-0-above)
      ("\M-f" . avy-goto-word-0-below)
      ("p" . avy-prev)
      ("n" . avy-next)
      ("s" . avy-goto-symbol-1)
      ("\C-a" . avy-goto-line)
      ("\C-e" . avy-goto-end-of-line)
      ("\C-n" . avy-goto-line-below)
      ("\C-p" . avy-goto-line-above)
      ("\C-w" . avy-move-region)
      ("\C-k" . avy-kill-region)
      ("\M-w" . avy-kill-ring-save-region)
      ("\C-b" . avy-pop-mark)
      ("i" . avy-copy-region))
    "The base keymap for `avy-mode'.")
  :bind (:map isearch-mode-map
	      ("C-'" . avy-isearch))
  :init
  (global-set-key (kbd "C-'") avy-basic-map)
  (which-key-add-key-based-replacements "C-'" "avy")
  :custom
  ;; (avy-timeout-seconds 0.75)
  ;; (avy-style 'at-full)  ;; this is already the default
  (avy-all-windows nil)    ;; commands only in this window
  (avy-all-windows-alt t)  ;; with prefix commands in all windows
  (avy-case-fold-search nil)
  ;; (avy-highlight-first t)
  (avy-indent-line-overlay t) ;; show highlight after non-whitespace
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
  :hook (arduino-mode . (lambda ()
			  (eval-after-load 'company
			    #'company-arduino-turn-on)))
  :defer t
  :config
  ;; This package already loads 'company-c-headers.
  (defconst initial-company-c-headers-path-system company-c-headers-path-system)

  (defun my/company-c-headers-get-system-path ()
    "Return the system include path for the current buffer plus arduino headers"
    (company-arduino-append-include-dirs initial-company-c-headers-path-system t))

  (setq company-c-headers-path-system #'my/company-c-headers-get-system-path))

(use-package arduino-cli-mode
  :after company-arduino      ;; This is latter enough
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t)
  (arduino-cli-mode-keymap-prefix (kbd "C-c C-t"))
  :config
  (arduino-cli-mode)
  (which-key-add-key-based-replacements "C-c C-t" "arduino-cli-mode"))

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
;; (use-package phps-mode
;;   :mode ("\\.php\\'" "\\.phtml\\'")
;;   :custom
;;   (phps-mode-async-process t)
;;   (phps-mode-async-process-using-async-el t)
;;   :config
;;   (eval-after-load 'flycheck
;;     (phps-mode-flycheck-setup)))


;; (use-package php-mode
;;   :mode ("\\.php\\'"))

(use-package web-mode
  :mode ("\\.\\(p\\|dj\\)?html\\'"
	 "\\(\\.tpl\\)?\\.php\\'" "\\.[agj]sp\\'"
	 "\\.as[cp]x\\'" "\\.erb\\'")
  :custom
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-css-colorization t)
  )

(use-package company-web
  :hook (web-mode . (lambda ()
		      (my/company-backend-after-load #'company-web-html)))
  :defer t)

;; (use-package web-mode-edit-element
;;   :hook (web-mode . web-mode-edit-element-minor-mode))

;;__________________________________________________________
;; nginx mode
(use-package nginx-mode
  :mode ("sites-\\(?:available\\|enabled\\)\\'" "nginx\\.config\\'"))

(use-package company-nginx
  :hook (nginx-mode . (lambda ()
			(my/company-backend-after-load #'company-nginx)))
  :defer t)

(use-package lice :defer t)
(use-package lorem-ipsum :defer t)
;;__________________________________________________________
;; json mode

(use-package json-mode
  :mode "\\.json\\'"
  :preface
  (my/gen-delay-hook json-mode))

(use-package flymake-json
  :hook (json-mode-delay . flymake-json-load)
  :defer t)

;;__________________________________________________________
;; Modeline
(use-package powerline
  :config
  (powerline-default-theme))

;;__________________________________________________________
;; yaml mode
(use-package yaml-mode
  :mode "\\.yaml\\'"
  :preface
  (my/gen-delay-hook yaml-mode))

(use-package flymake-yaml
  :hook (yaml-mode-delay . flymake-yaml-load)
  :defer t)

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
	      (set-face-attribute 'mode-line nil :background (my/named-color brightblack))))
  (add-hook 'evil-insert-state-entry-hook
	    (lambda ()
	      (set-face-attribute 'mode-line nil :background original-background)))
  (add-hook 'evil-visual-state-entry-hook
	    (lambda ()
	      (set-face-attribute 'mode-line nil :background (my/named-color green)))))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :hook (evil-mode .  evil-collection-init)
  :defer t)

(use-package composable
  :diminish
  ;; :after which-key
  :preface
  :load-path (lambda () (my/load-path "~/gits/composable.el/"))
  :custom
  (composable-mode-debug-level 3)
  :config
  (composable-mode)       ; Activates the default keybindings
  (composable-mark-mode)) ; Use composable with C-SPC

(use-package slime
  :defer t
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
