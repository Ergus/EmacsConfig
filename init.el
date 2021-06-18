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
	      ring-bell-function #'ignore
	      user-full-name "Ergus"
	      initial-scratch-message (format ";; Welcome %s!!" user-full-name)

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
	      bookmark-menu-confirm-deletion t    ;; ask confirmation to delete bookmark
	      bookmark-fontify t                  ;; Colorize bookmarked lines with bookmark-face
	      )

;; Vertical window divider
(set-display-table-slot standard-display-table
			'vertical-border
			(make-glyph-code ?\u2502))
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
				 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
				 ("melpa" . "https://melpa.org/packages/"))
	      package-native-compile t
	      package-quickstart t)

;; Set this BEFORE require use-package
(defvar my/package-initialized-p nil
  "Set to true when package where initialized.")

(defmacro my/package-install (package)
  "Conditionally install PACKAGE in debug mode."
  `(when init-file-debug
     (unless (fboundp 'package-installed-p)
       (require 'package))
     (unless (package-installed-p ,package)
       (unless my/package-initialized-p
	 (package-initialize)
	 (package-refresh-contents)
	 (setq my/package-initialized-p t))
       (package-install ,package))))

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
	 (run-with-idle-timer 0.5 nil
			      (lambda (buf)
				(when (buffer-live-p buf)
				  (with-current-buffer buf
				    (run-hooks ',delayhook))))
			      (current-buffer)))
       (add-hook ',modehook (function ,funame)))))

(if init-file-debug
    (progn
      ;; Install use-package if not installed
      (my/package-install 'use-package)

      (setq-default use-package-always-ensure t
		    use-package-enable-imenu-support t
		    use-package-verbose t
		    use-package-expand-minimally nil
		    use-package-compute-statistics t
		    debug-on-error t
		    native-comp-async-report-warnings-errors t))

  (setq-default use-package-always-ensure nil
		use-package-enable-imenu-support nil
		use-package-verbose nil
		use-package-expand-minimally t
		native-comp-async-report-warnings-errors 'silent))
(require 'use-package)

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
  :init
  (setq-default which-key-idle-delay 0.5
		which-key-idle-secondary-delay 0.01  ;; nil sets the same delay
		which-key-dont-use-unicode t)
  ;;(which-key-idle-delay 10000) ;; To not show
  ;;(which-key-show-early-on-C-h t)
  ;;(which-key-popup-type 'minibuffer)
  ;;(which-key-separator ": ") ;which-key-idle-delay 2.0)
  :config
  (which-key-mode t)
  (which-key-add-key-based-replacements
    "C-c b" "sidebars"
    "C-x r" "rectangle||register"
    "C-x n" "narrow"
    "C-x p" "project"
    "C-x RET" "coding-system"
    "C-x @" "event-apply-modifier"
    "C-x ESC" "repeat-command"
    "C-x 8" "unicode"
    "C-x x" "buffer"))

;;__________________________________________________________
;; Some internal packages to defer them

;; imenu
(setq-default imenu-use-markers nil
	      imenu-auto-rescan t
	      imenu-max-item-length 256)

;; uniquify
(setq-default uniquify-buffer-name-style 'post-forward)

;; saveplace
(setq-default save-place-ignore-files-regexp  ;; Modified to add /tmp/* files
	      "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\|^/tmp/.+\\)$")
(save-place-mode 1)              ;; Remember point in files

;; autorevert
(setq-default auto-revert-verbose nil	                ;; not show message when file changes
	      auto-revert-avoid-polling t)              ;; don't do pooling for autorevert (use notifications).)
(run-with-idle-timer 1 nil #'global-auto-revert-mode t) ;; Autoload files changed in disk

;; show-paren-mode
(setq-default show-paren-delay 0
	      blink-matching-paren nil)
(run-with-idle-timer 0.5 nil #'show-paren-mode t)

;; profiler
(add-hook 'profiler-report-mode-hook #'hl-line-mode)

;; Shows the function in spaceline
(eval-after-load 'which-func '(diminish 'which-func-mode))

;; text-mode
(my/gen-delay-hook text-mode)

;; prog-mode
(my/gen-delay-hook prog-mode)
(add-hook 'prog-mode-delay-hook
	  (lambda nil
	    (setq show-trailing-whitespace t)))

;; elec-pair
(defun my/electric-pair-local-mode ()
  "Enable electric-pair-local-mode"
  (electric-pair-local-mode 1))
(add-hook 'prog-mode-hook #'my/electric-pair-local-mode)
(add-hook 'text-mode-hook #'my/electric-pair-local-mode)

;; hl-line
(global-set-key (kbd "M-s h L") #'hl-line-mode)
(eval-after-load 'hl-line '(diminish 'hl-line-mode))
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; hilit-chg changes
(easy-mmode-defmap highlight-changes-map
  `(("c" . highlight-changes-mode)
    ("v" . highlight-changes-visible-mode)
    ("r" . highlight-changes-remove-highlight)
    ("n" . highlight-changes-next-change)
    ("p" . highlight-changes-previous-change)
    ("f" . highlight-changes-rotate-faces)
    ("b" . highlight-compare-buffers)
    ("d" . highlight-compare-with-file))
  "The base keymap for `highlight changes'.")
(global-set-key (kbd "M-s h c") highlight-changes-map)
(which-key-add-key-based-replacements "M-s h c" "highlight-changes")

;; winner
(setq-default winner-dont-bind-my-keys t)
(winner-mode t)
(define-key ctl-x-map "wu"  #'winner-undo)
(define-key ctl-x-map "wr"  #'winner-redo)
(which-key-add-key-based-replacements "C-x w" "winner")

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; abbrev
(which-key-add-key-based-replacements "C-x a" "abbrev")
(eval-after-load 'abbrev '(diminish 'abbrev-mode))


;; eldoc
(setq-default eldoc-idle-delay 2                              ;; default 0.5
	      eldoc-print-after-edit t                        ;; only show after edit
	      eldoc-echo-area-display-truncation-message nil) ;; Not verbose when truncated)
(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode)
  (global-eldoc-mode -1))   ;; This is enabled by default, disable it

(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)

;; gdb
(setq-default gdb-debug-log-max nil   ;; no limit log
	      gdb-many-windows nil
	      gdb-show-main t)

;; conf-mode
(my/gen-delay-hook conf-mode)

;; hideif mode
(setq-default hide-ifdef-shadow t
	      hide-ifdef-initially t)
(with-eval-after-load 'hideif
  (diminish 'hide-ifdef-mode)
  (diminish 'hide-ifdef-hiding))

(eval-after-load 'subword (diminish 'subword-mode))

;;__________________________________________________________
;; Benchmark-init

(use-package paradox
  :defer t
  :init
  (setq-default paradox-spinner-type 'progress-bar
		paradox-display-download-count t
		paradox-display-star-count t))
;;__________________________________________________________
;; Isearch

(setq-default search-nonincremental-instead nil  ;; No incremental if enter & empty
	      lazy-highlight-initial-delay 0
	      isearch-allow-scroll t 	         ;; Permit scroll can be 'unlimited
	      isearch-lazy-count t
	      search-ring-max 256
	      regexp-search-ring-max 256
	      isearch-yank-on-move 'shift       ;; Copy text from buffer with meta
	      ;; isearch-wrap-function #'ignore ;; Look at the emacs-major-version check
	      ;;isearch-wrap-pause t            ;; Disable wrapping nil.
	      isearch-repeat-on-direction-change t ;; Don't go to the other end on direction change
	      isearch-regexp-lax-whitespace t   ;; swiper like fuzzy search
	      search-whitespace-regexp ".*?"
	      )

(with-eval-after-load 'isearch
  (define-key isearch-mode-map
    [remap isearch-delete-char] #'isearch-del-char)

  (define-key isearch-mode-map (kbd "M-<") #'isearch-beginning-of-buffer)
  (define-key isearch-mode-map (kbd "M->") #'isearch-end-of-buffer)

  (defun my/isearch-exit-other-end ()
    (interactive)
    (when isearch-other-end
      (goto-char isearch-other-end))
    (call-interactively #'isearch-exit))

  (define-key isearch-mode-map (kbd "M-RET") #'my/isearch-exit-other-end)

  (which-key-add-key-based-replacements "M-s h" "highlight"))

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

;; (use-package unfill
;;   :defer t
;;   :init
;;   (global-set-key [remap fill-paragraph] #'unfill-toggle))

;;__________________________________________________________
;; compile

(setq-default compilation-scroll-output 'first-error
	      compilation-always-kill t)

;;__________________________________________________________
;; ssh
(setq-default tramp-auto-save-directory
	      (expand-file-name "tramp-autosave-dir" user-emacs-directory)
	      tramp-completion-use-auth-sources nil
	      remote-file-name-inhibit-cache 120           ;; Default 10
	      tramp-completion-reread-directory-timeout 120;; Default 10
	      password-cache-expiry 3600                   ;; Cache for 1 hour
	      tramp-default-method "scp")

(with-eval-after-load 'tramp
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

(setq-default tab-bar-tab-hints t  ;; show tab numbers
	      tab-bar-close-last-tab-choice 'tab-bar-mode-disable ;; When close last
	      tab-bar-show 1)
(which-key-add-key-based-replacements "C-x t" "tab-bar")

;;__________________________________________________________
;; minibuffers

(add-hook 'minibuffer-setup-hook
	  (lambda nil
	    (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
	  (lambda nil
	    (setq gc-cons-threshold my/gc-cons-threshold)))

;;__________________________________________________________
;; Two options for diffs
(setq-default ediff-window-setup-function #'ediff-setup-windows-plain
	      ediff-split-window-function #'split-window-horizontally)
(eval-after-load 'ediff
  '(eval-after-load 'winner
     '(add-hook 'ediff-after-quit-hook-internal #'winner-undo)))

;; more like vimdiff
(use-package vdiff
  :defer t
  :bind-keymap ("C-c d" . vdiff-mode-prefix-map)
  :init
  (setq-default vdiff-auto-refine t)
  (which-key-add-key-based-replacements "C-c d" "vdiff")
  :config
  (which-key-add-key-based-replacements "C-c d i" "vdiff-toggle")
  )

;;__________________________________________________________
;; terms

(use-package vterm
  :defer t
  :hook (vterm-mode . (lambda nil
			(display-fill-column-indicator-mode -1)
			(auto-fill-mode -1)))
  :init
  (setq-default vterm-kill-buffer-on-exit t
		vterm-max-scrollback 10000)
  (which-key-add-key-based-replacements "C-c t" "term")
  :config
  ;; Add find-file-other-window to accepted commands
  (add-to-list 'vterm-eval-cmds
	       '("find-file-other-window" find-file-other-window)))

(use-package vterm-toggle
  :defer t
  :init
  (setq-default vterm-toggle-scope 'project
		vterm-toggle-project-root t
		vterm-toggle-fullscreen-p nil)
  (global-set-key (kbd "C-c t t") #'vterm-toggle-cd)
  :config
  (define-key vterm-mode-map (kbd "<C-return>") #'vterm-toggle-insert-cd)
  (define-key vterm-mode-map (kbd "C-M-n") #'vterm-toggle-forward)
  (define-key vterm-mode-map (kbd "C-M-p") #'vterm-toggle-backward)

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
  :defer t
  :init
  (global-set-key (kbd "C-c t b") #'better-shell-shell))

(use-package shell-command+
  :defer t
  :init
  (global-set-key [remap shell-command] #'shell-command+))

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
  :defer 1
  :config
  (xclip-mode 1))

;;__________________________________________________________
;; xterm mouse
(unless (or (display-graphic-p)
	    (string-equal (getenv "TERM") "linux"))
  (setq-default mouse-sel-mode t          ;; Mouse selection
		mouse-scroll-delay 0)

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
    (global-set-key (kbd "<mouse-5>") #'scroll-up-command)))


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

;; (global-set-key [remap undo] #'undo-only)

(global-set-key (kbd "C-_") #'undo-only)
(global-set-key (kbd "C-/") #'undo-only)
(global-set-key (kbd "C-M-_") #'undo-redo)
(global-set-key (kbd "C-M-/") #'undo-redo)

(use-package string-inflection
  :defer t
  :init
  (global-set-key (kbd "C-c <right>") #'string-inflection-all-cycle))

(use-package undo-propose
  :defer t)

;;__________________________________________________________
;; Mark column 80 when crossed

(use-package highlight-indent-guides
  :defer t
  :diminish
  :init
  (setq-default highlight-indent-guides-auto-enabled nil
		highlight-indent-guides-method 'character)
  (global-set-key (kbd "M-s h i") #'highlight-indent-guides-mode)
  :config
  (set-face-attribute 'highlight-indent-guides-character-face nil
		      :foreground (my/named-color brightblack)))

;;__________________________________________________________
;; Resalta scopes entorno al cursor
(use-package highlight-blocks
  :defer t
  :diminish
  :init
  (global-set-key (kbd "M-s h b") #'highlight-blocks-now)
  (global-set-key (kbd "M-s h B") #'highlight-blocks-mode)
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
  :defer t
  :init
  (global-set-key (kbd "M-s h s") #'hes-mode)
  (which-key-add-key-based-replacements "M-s h s" "highlight-escape-mode"))

;;__________________________________________________________
;; Flyspell (Orthography)
(setq-default ispell-following-word t ;;Check word around point not only before
	      ispell-quietly t)       ;; Suppress messages in ispell-word


;; Flyspell
(setq-default flyspell-use-meta-tab nil      ;; Not correct with M-TAB
	      flyspell-mode-line-string nil  ;; Not show Fly in modeline
	      flyspell-delay 2)              ;; default 3

(add-hook 'prog-mode-delay-hook #'flyspell-prog-mode)
(add-hook 'text-mode-delay-hook #'turn-on-flyspell)

(with-eval-after-load 'flyspell
  (easy-mmode-defmap flyspell-basic-map
    `(("r" . flyspell-region)
      ("b" . flyspell-buffer)
      ("n" . flyspell-goto-next-error))
    "The base keymap for `flyspell-mode'")

  (setf (cdr flyspell-mode-map) nil)  ;; clear yas minor map
  (define-key flyspell-mode-map (kbd "C-c f") flyspell-basic-map)
  (which-key-add-keymap-based-replacements flyspell-mode-map "C-c f" "flyspell")
  (diminish 'flyspell-mode))

(use-package flyspell-correct-ivy
  :diminish
  :after flyspell
  :init
  (setq-default flyspell-correct-interface #'flyspell-correct-ivy)
  :config
  (define-key flyspell-basic-map "w" #'flyspell-correct-wrapper)
  (define-key flyspell-basic-map "f" #'flyspell-correct-at-point)
  (define-key flyspell-basic-map "\C-n" #'flyspell-correct-next)
  (define-key flyspell-basic-map "\C-p" #'flyspell-correct-previous)
  )

;;__________________________________________________________
;; {c/c++}-mode
;;__________________________________________________________

;;__________________________________________________________
;; LSP try for a whil

(use-package eglot :defer t)

(use-package company
  ;; :load-path (lambda nil (my/load-path "~/gits/company-mode/"))
  :preface
  (defmacro my/company-backend-after-load (backend)
    `(with-eval-after-load 'company
       (unless (eq ,backend (car company-backends))
	 (setq-local company-backends
		     (cons ,backend (remove ,backend company-backends))))))

  (defun my/company-mode-delay-hook nil
    "Load company mode if not active."
    (unless (bound-and-true-p company-mode)
      (company-mode 1)))
  :defer t
  :init
  (add-hook 'prog-mode-delay-hook #'my/company-mode-delay-hook)
  (add-hook 'message-mode-delay-hook #'my/company-mode-delay-hook)
  (add-hook 'conf-mode-delay-hook #'my/company-mode-delay-hook)

  (setq-default company-idle-delay nil	 ;; no delay for autocomplete
		company-minimum-prefix-length 2
		company-selection-wrap-around nil
		company-show-numbers t
		company-tooltip-align-annotations t
		company-format-margin-function #'company-detect-icons-margin
		;;company-tooltip-limit 20
		company-backends '(company-capf       ;; completion at point
				   company-semantic
				   company-files	 ;; company files
				   (company-dabbrev-code company-gtags company-keywords)
				   company-dabbrev))
  :config
  (define-key company-mode-map (kbd "M-RET") #'company-complete)
  (define-key company-mode-map (kbd "M-/") #'company-other-backend)

  (define-key company-active-map (kbd "M-RET") #'company-abort)
  (define-key company-active-map [remap dabbrev-expand] #'company-other-backend)
  (define-key company-active-map [remap xref-find-definitions] #'company-show-location)
  )

(use-package lsp-mode
  :diminish lsp
  :hook (lsp-mode . (lambda nil
		      (my/company-backend-after-load #'company-capf)
		      (lsp-enable-which-key-integration)))
  :defer t
  :init
  (setq-default lsp-keymap-prefix (kbd "C-c l")
		lsp-enable-snippet nil
		lsp-eldoc-hook nil
		lsp-enable-indentation nil
		lsp-prefer-capf t
		read-process-output-max (* 1024 1024)) ;; 1mb
  ;; lsp-diagnostic-package t ;; prefer flymake
  ;; :init
  ;; (add-hook 'c-mode-common-hook #'lsp-deferred)
  ;; (add-hook 'python-mode-hook #'lsp-deferred)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  )

(use-package lsp-ui
  :diminish
  :after lsp-mode
  :init
  ;;(lsp-ui-sideline-delay 1.0)
  (setq-default lsp-ui-sideline-enable nil
		lsp-ui-doc-enable nil)
  :config
  (which-key-add-keymap-based-replacements lsp-command-map "C-c l u" "lsp-ui")

  (define-key lsp-command-map "ud" #'lsp-ui-peek-find-definitions)
  (define-key lsp-command-map "ur" #'lsp-ui-peek-find-references)
  (define-key lsp-command-map "ui" #'lsp-ui-peek-find-implementation)
  ;;("s" . lsp-ui-peek-find-workspace-symbol)
  (define-key lsp-command-map "uc" #'lsp-ui-peek-find-custom)
  ;; imenu
  (define-key lsp-command-map "um" #'lsp-ui-imenu)
  ;; flycheck
  (define-key lsp-command-map "uf" #'lsp-ui-flycheck-list)
  ;; lsp-ui
  (define-key lsp-command-map "un" #'lsp-ui-find-next-reference)
  (define-key lsp-command-map "up" #'lsp-ui-find-prev-reference)
  )

;; (use-package lsp-treemacs
;;   :diminish
;;   :after (lsp-mode treemacs)
;;   :custom
;;   (lsp-metals-treeview-enable t)
;;   (lsp-metals-treeview-show-when-views-received t))

(use-package lsp-ivy
  :diminish
  :after lsp-mode
  :config
  (define-key lsp-mode-map (kbd "C-c l i") #'lsp-ivy-workspace-symbol)
  )

(use-package tree-sitter
  :defer t)

(use-package tree-sitter-langs
  :after tree-sitter)

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

;; cc-mode
(defun my/c-mode-common-hook ()
  "my/c-mode-common common."
  (c-toggle-auto-newline 1)
  (c-toggle-cpp-indent-to-body 1)
  (c-ms-space-for-alignment-mode 1)
  (hide-ifdef-mode 1)
  (subword-mode 1))
(add-hook 'c-mode-common-hook #'my/c-mode-common-hook)

(setq-default c-default-style '((java-mode . "java")
				(awk-mode . "awk")
				(other . "mylinux")))

(with-eval-after-load 'cc-mode
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
  :defer t
  :init
  (add-hook 'c-mode-common-hook #'preproc-font-lock-mode)
  (setq-default preproc-font-lock-preprocessor-background-face 'font-lock-preprocessor-face))

;; company-c-headers
(use-package company-c-headers
  :preface
  (defun my/c-mode-hook ()
    (my/company-backend-after-load #'company-c-headers))
  :defer t
  :init
  (add-hook 'c-mode #'my/c-mode-hook)
  (add-hook 'c++-mode #'my/c-mode-hook)
  (add-hook 'objc-mode #'my/c-mode-hook))

(use-package clang-format :defer t)

;;__________________________________________________________
;; C++ mode
(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :defer t
  :init
  (add-hook 'c++-mode #'modern-c++-font-lock-mode))

;;__________________________________________________________
;; elisp mode (all after the company declaration)

(add-hook 'emacs-lisp-mode-hook
	  (lambda nil
	    (when (and buffer-file-name
		       (string-match "\\.el\\'" buffer-file-name))
	      (my/company-backend-after-load #'company-elisp))))

;;__________________________________________________________
;; sh mode

(defvaralias 'sh-basic-offset 'tab-width)

(add-hook 'sh-mode-hook (lambda nil
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
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init
  (setq-default markdown-command "multimarkdown"))

;;__________________________________________________________
;; Restructured text
(use-package sphinx-mode
  :defer t
  :init
  (add-hook 'rst-mode-hook #'sphinx-mode))

;;__________________________________________________________
;; ruby-mode
(setq-default ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.rjs\\'" . ruby-mode))

(use-package ruby-tools
  :defer t
  :init
  (add-hook 'ruby-mode-hook #'ruby-tools-mode))

(use-package ruby-electric
  :defer t
  :init
  (add-hook 'ruby-mode-hook  #'ruby-electric-mode))

;;__________________________________________________________
;; Julia Mode
(use-package julia-mode
  :mode "\\.jl\\'")

(use-package flycheck-julia
  :defer t
  :init
  (add-hook 'julia-mode-hook #'flycheck-julia-setup))

;;__________________________________________________________
;; Rust Mode
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package flycheck-rust
  :defer t
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

;;__________________________________________________________
;; Ocaml Mode
(use-package caml
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . caml-mode)))

;;__________________________________________________________
;; D languaje
(use-package d-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.d\\'" . d-mode)))

;;__________________________________________________________
;; Go languaje
(use-package go-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;;__________________________________________________________
;; lua language
(use-package lua-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(use-package company-lua
  :hook (lua-mode . (lambda nil
		      (my/company-backend-after-load #'company-lua)))
  :defer t)

;;__________________________________________________________
;; groovy language

(use-package groovy-mode :defer t)

;;__________________________________________________________
;; systemd mode
(use-package systemd :defer t)

;;__________________________________________________________
;; Use for Qt's .pro and .pri files
(use-package qt-pro-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.pr[io]\\'" . qt-pro-mode))
  (add-to-list 'auto-mode-alist '("\\.moc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.ui\\'" . xml-mode)))

;;__________________________________________________________
;; javascript-mode
;; (use-package js-mode :ensure nil
;;   :mode ("\\.js\\'"))

;;__________________________________________________________
;; xml-mode
(add-to-list 'auto-mode-alist
	     '("\\.\\(ipe\\|qrc\\|svn\\)\\'" . xml-mode))

;;__________________________________________________________
;; splitting
(setq-default windmove-display-no-select t)

(easy-mmode-defmap ctl-x-0-map
  `(("0" . delete-window)
    ([left] . windmove-delete-left)
    ([right] . windmove-delete-right)
    ([up] . windmove-delete-up)
    ([down] . windmove-delete-down))
  "The base keymap for `highlight changes'.")

(define-key ctl-x-map "0" ctl-x-0-map)
(which-key-add-key-based-replacements "C-x 0" "windmove-delete")

(define-key ctl-x-map (kbd "<left>")  #'windmove-left)
(define-key ctl-x-map (kbd "<right>")  #'windmove-right)
(define-key ctl-x-map (kbd "<down>")  #'windmove-down)
(define-key ctl-x-map (kbd "<up>")  #'windmove-up)

(define-key ctl-x-4-map (kbd "<left>")  #'windmove-display-left)
(define-key ctl-x-4-map (kbd "<right>")  #'windmove-display-right)
(define-key ctl-x-4-map (kbd "<up>")  #'windmove-display-up)
(define-key ctl-x-4-map (kbd "<down>")  #'windmove-display-down)

(define-key ctl-x-map (kbd "C-M-<left>")  #'windmove-swap-states-left)
(define-key ctl-x-map (kbd "C-M-<right>")  #'windmove-swap-states-right)
(define-key ctl-x-map (kbd "C-M-<down>")  #'windmove-swap-states-down)
(define-key ctl-x-map (kbd "C-M-<up>")  #'windmove-swap-states-up)

;; repeat-mode
(run-with-idle-timer 0.5 nil #'repeat-mode 1)
(with-eval-after-load 'repeat
  (defvar undo-redo-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "u" #'undo-only)
      (define-key map "r" #'undo-redo)
      (define-key map "U" #'undo)
      map)
    "Keymap to repeat undo-redo key sequences.  Used in `repeat-mode'.")
  (put 'undo-only 'repeat-map 'undo-redo-repeat-map)
  (put 'undo-redo 'repeat-map 'undo-redo-repeat-map)
  (put 'undo 'repeat-map 'undo-redo-repeat-map)
  )

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
  :defer t
  :init
  (add-to-list 'auto-mode-alist
	       '("\\.\\(gpl?\\|plt\\)\\'" . gnuplot-mode)))

;;__________________________________________________________
;; Auto completamiento

(use-package yasnippet        ;; Snippets
  :diminish yas-minor-mode
  :defer 1
  :init
  (setq-default yas-verbosity 1                 ; No need to be so verbose
		yas-wrap-around-region t)
  :config
  (define-key yas-keymap [remap indent-for-tab-command] #'yas-next-field-or-maybe-expand)

  (defun yas-expand-or-insert ()
    (interactive)
    (or (call-interactively #'yas-expand)
	(call-interactively #'yas-insert-snippet)))

  (easy-mmode-defmap yas-minor-basic-map
    '(("d" . yas-load-directory)
      ("i" . yas-insert-snippet)
      ("f" . yas-visit-snippet-file)
      ("n" . yas-new-snippet)
      ("t" . yas-tryout-snippet)
      ("l" . yas-describe-tables)
      ("x" . yas-expand)
      ("y" . yas-expand-or-insert))
    "The keymap used when `yas-minor-mode' is active.")

  (setf (cdr yas-minor-mode-map) nil)  ;; clear yas minor map
  (define-key yas-minor-mode-map (kbd "C-c y") yas-minor-basic-map)
  (which-key-add-keymap-based-replacements yas-minor-mode-map "C-c y" "yasnippet")

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;;__________________________________________________________
;; Chequeo de syntaxis
(use-package flycheck
  :diminish
  :when (< (buffer-size) 200000)
  :defer t
  :preface
  (defun my/flycheck-mode-hook ()
    "Hook to enable flycheck-mode."
    (pcase major-mode
      ('c-mode
       (setq-local flycheck-gcc-language-standard "c17"
		   flycheck-clang-language-standard "c17"))
      ('c++-mode
       (setq-local flycheck-gcc-language-standard "c++17"
		   flycheck-clang-language-standard "c++17")))
    (flycheck-mode 1))
  :init
  (add-hook 'prog-mode-delay-hook #'my/flycheck-mode-hook)
  (setq-default flycheck-display-errors-delay 1.0
		flycheck-keymap-prefix (kbd "C-c a"))
  :config
  (which-key-add-keymap-based-replacements flycheck-mode-map "C-c a" "flycheck")
  (define-key flycheck-command-map "a" #'counsel-flycheck)
  )

(with-eval-after-load 'flymake
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (easy-mmode-defmap flymake-basic-map
    `(("n" . flymake-goto-next-error)
      ("p" . flymake-goto-prev-error)
      ("d" . flymake-show-diagnostic)
      ("b" . flymake-show-diagnostic-buffer)
      ("l" . flymake-switch-to-log-buffer))
    "The base keymap for `flymake-mode'.")

  (define-key flymake-mode-map (kbd "C-c k") flymake-basic-map)
  (which-key-add-keymap-based-replacements flymake-mode-map "C-c k" "flymake")
  (diminish 'flymake-mode))

;;__________________________________________________________
;; Improved help buffer

(use-package helpful
  :defer t
  :diminish
  :init
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
  (global-set-key (kbd "C-h M") #'helpful-macro)
  (global-set-key (kbd "C-h L") #'helpful-callable)
  (global-set-key (kbd "C-h K") #'helpful-key)
  (global-set-key (kbd "C-h P") #'helpful-at-point)
  (global-set-key (kbd "C-h V") #'helpful-variable))

;;__________________________________________________________
;; Chequeo de gramatica
(use-package langtool
  :defer t
  :init
  (setq-default langtool-default-language "en"
		langtool-language-tool-jar "~/gits/languagetool/languagetool-standalone/target/LanguageTool-4.6-SNAPSHOT/LanguageTool-4.6-SNAPSHOT/languagetool-commandline.jar"))

;;__________________________________________________________
;; Email mode for mutt
;;__________________________________________________________

;; Asocia buffers que empiecen con messaje mode

;; message-mode
(my/gen-delay-hook message-mode)

(setq-default message-default-mail-headers "Cc: \nBcc: \n"
	      message-kill-buffer-on-exit t
	      message-send-mail-function #'message-use-send-mail-function
	      mail-header-separator "")

(with-eval-after-load 'message-mode
  (auto-fill-mode t)
  (mail-abbrevs-setup)
  (flyspell-mode t))

(add-to-list 'auto-mode-alist '("/neomut" . message-mode))
(add-to-list 'auto-mode-alist '("neomutt-Ergus-" . message-mode))
(add-to-list 'auto-mode-alist '("draft" . message-mode))

(use-package notmuch
  :init
  (setenv "NOTMUCH_CONFIG" "/home/ergo/almacen/mail/notmuch-config")
  :hook (message-mode . (lambda nil
			  (my/company-backend-after-load #'notmuch-company)))
  :defer t)

;;__________________________________________________________
;; Latex mode
(my/package-install 'auctex)

(setq-default TeX-source-correlate-start-server t
	      TeX-auto-save t
	      TeX-parse-self t
	      LaTeX-babel-hyphen nil
	      TeX-master nil ;; Multidocument
	      LaTeX-indent-level 4
	      LaTeX-item-indent 0)

(with-eval-after-load 'tex
  (TeX-source-correlate-mode 1)
  (add-to-list 'TeX-command-list
	       '("Makeglossaries" "makeglossaries %s" TeX-run-command nil
		 (latex-mode)
		 :help "Run makeglossaries, will choose xindy or makeindex")
	       t)
  (with-eval-after-load 'latex
    (defun my/LaTeX-indent-item nil "Syntactic indentation for itemize like environments to add extra offsets."
	   (save-match-data
	     (let*
		 ((offset
		   (+ LaTeX-indent-level LaTeX-item-indent))
		  (re-beg "\\\\begin{")
		  (re-end "\\\\end{")
		  (re-env "\\(itemize\\|\\enumerate\\|description\\)")
		  (indent (save-excursion
			    (when
				(looking-at
				 (concat re-beg re-env "}"))
			      (end-of-line))
			    (LaTeX-find-matching-begin)
			    (current-column))))
	       (cond
		((looking-at (concat re-beg re-env "}"))
		 (or (save-excursion
		       (beginning-of-line)
		       (ignore-errors
			 (LaTeX-find-matching-begin)
			 (+ (current-column)
			    (if (looking-at (concat re-beg re-env "}"))
				offset
			      LaTeX-indent-level)))
		       indent)))
		((looking-at (concat re-end re-env "}"))
		 indent)
		((looking-at "\\\\item")
		 (+ indent offset))
		(t
		 (+ indent offset LaTeX-indent-level))))))
    (add-to-list 'LaTeX-indent-environment-list '("itemize" my/LaTeX-indent-item))
    (add-to-list 'LaTeX-indent-environment-list '("enumerate" my/LaTeX-indent-item))
    (add-to-list 'LaTeX-indent-environment-list '("description" my/LaTeX-indent-item))))

(add-hook 'LaTeX-mode-hook (lambda nil
			     (flyspell-mode 1)
			     (visual-line-mode 1)
			     (auto-fill-mode 1)))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))


;; auctex-latexmk
(use-package auctex-latexmk
  :defer t
  :init
  (setq-default auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package company-math
  :hook (TeX-mode . (lambda nil
		      (my/company-backend-after-load
		       '(company-math-symbols-latex company-latex-commands))))
  :defer t)

(use-package company-auctex
  :hook (TeX-mode . (lambda nil
		      (my/company-backend-after-load #'company-auctex-init)))
  :defer t)

;; reftex
(setq-default reftex-cite-prompt-optional-args t   ; Prompt for empty optional arguments in cite
	      reftex-cite-format 'biblatex
	      reftex-plug-into-AUCTeX t
	      reftex-insert-label-flags t
	      reftex-save-parse-info t
	      reftex-enable-partial-scans t
	      reftex-use-multiple-selection-buffers t)

(add-hook 'LaTeX-mode-hook #'turn-on-reftex)

(eval-after-load 'reftex '(reftex-isearch-minor-mode))

(use-package company-reftex
  :hook (reftex-mode . (lambda nil
			 (my/company-backend-after-load
			  '(company-reftex-labels company-reftex-citations))))
  :defer t)

;;__________________________________________________________
;;bibtex mode set use biblatex

(setq-default bibtex-dialect 'biblatex)
(add-to-list 'auto-mode-alist '("\\.bib\\'" . bibtex-mode))

(use-package company-bibtex
  :hook (bibtex-mode . (lambda nil
			 (my/company-backend-after-load #'company-bibtex)))
  :defer t)

(use-package ivy-bibtex
  :defer t
  :init
  (setq-default ivy-bibtex-default-action #'bibtex-completion-insert-citation))

;;__________________________________________________________
;; Python mode
(setq-default python-shell-interpreter "ipython3"
	      python-shell-interpreter-args "-i --simple-prompt"
	      python-shell-prompt-detect-failure-warning nil
	      python-check-command "pyflakes"
	      flycheck-python-flake8-executable "flake8")

(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c C-z") #'python-shell))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

;;__________________________________________________________
;; Dired-mode settings (file manager)

(setq-default dired-recursive-copies 'top    		   ;; Always ask recursive copy
	      dired-recursive-deletes 'top   		   ;; Always ask recursive delete
	      dired-dwim-target 'dired-dwim-target-recent  ;; Copy in split mode with p
	      dired-auto-revert-buffer nil   		   ;; auto revert dired
	      dired-listing-switches "-alh"  		   ;; commands to ls
	      dired-hide-details-hide-symlink-targets nil  ;; don't hide linkk targets
	      dired-maybe-use-globstar t)

(with-eval-after-load 'dired
  (require 'dired-x)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map [remap dired-find-file] #'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map [remap dired-up-directory] (lambda nil               ; was dired-up-directory
							  (interactive)
							  (find-alternate-file ".."))))

(use-package dired-sidebar
  :defer t
  :init
  ;;(dired-sidebar-use-term-integration t)
  (setq-default dired-sidebar-theme 'nerd
		dired-sidebar-subtree-line-prefix ".")
  (global-set-key (kbd "C-c b d") #'dired-sidebar-toggle-sidebar))


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
(setq-default ibuffer-default-sorting-mode 'alphabetic)  ;; can use recency)
(global-set-key [remap list-buffers] #'ibuffer)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)
(eval-after-load 'ibuffer
  '(which-key-add-keymap-based-replacements ibuffer--filter-map "G" "Groups"))

(use-package ibuffer-sidebar
  :defer t
  :init
  (global-set-key (kbd "C-c b b") #'ibuffer-sidebar-toggle-sidebar))

(use-package ibuffer-tramp
  :after ibuffer
  :config
  (define-key ibuffer--filter-map "Gt" #'ibuffer-tramp-set-filter-groups-by-tramp-connection))

(use-package ibuffer-project
  :after ibuffer
  :preface
  (defun ibuffer-set-filter-groups-by-project ()
    (interactive)
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (ibuffer-update nil t))
  :config
  (define-key ibuffer--filter-map "Gp" #'ibuffer-set-filter-groups-by-project))

(use-package ibuffer-vc
  :after ibuffer
  :config
  (define-key ibuffer--filter-map "Gv" #'ibuffer-vc-set-filter-groups-by-vc-root))

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
  :defer t
  :init
  (global-set-key (kbd "C-c b n") #'neotree-toggle))

;;__________________________________________________________
;; Ivy (probare un tiempo con helm/ivy)

(use-package headlong :defer t)

(use-package flx :defer t)

(use-package ivy
  :diminish
  :defer t
  :init
  (setq-default ivy-count-format "(%d/%d) "
		ivy-pulse-delay nil
		ivy-use-selectable-prompt t
		ivy-fixed-height-minibuffer t
		ivy-on-del-error-function #'ignore
		ivy-read-action-format-function #'ivy-read-action-format-columns)
  ;; (ivy-use-virtual-buffers t)   ;; Recent files or buffers in ivy
  ;; (ivy-height 10)
  ;; (ivy-wrap t)		      ;; cycle in minibuffer
  :config
  ;; Highlight with arrows by default.
  ;;(add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-arrow))

  (define-key ivy-mode-map (kbd "C-c c c") #'ivy-resume)
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-partial)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  (ivy-mode 1))

;; (use-package hydra :defer t)

;; (use-package ivy-hydra
;;   :init
;;   (setq-default ivy-read-action-function #'ivy-hydra-read-action))

(use-package ivy-avy :after ivy)

(use-package ivy-xref
  :defer t
  :init
  (which-key-add-key-based-replacements "C-c x" "xref")
  :init
  (setq-default xref-show-definitions-function #'ivy-xref-show-defs
		xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (easy-mmode-defmap ivy-xref-basic-map
    `(("d" . xref-find-definitions)
      ("4" . xref-find-definitions-other-window)
      ("a" . xref-find-apropos)
      ("b" . xref-pop-marker-stack) ;; go back
      ("r" . xref-find-references)
      (,(kbd "TAB") . completion-at-point))
    "The base keymap for `ivy-xref'")

  (global-set-key (kbd "C-c x") ivy-xref-basic-map))

(use-package swiper
  :defer t
  :init
  (setq-default swiper-goto-start-of-match t
		swiper-verbose nil)
  (eval-after-load 'isearch
    '(define-key isearch-mode-map (kbd "C-o") #'swiper-isearch-toggle))
  :config
  (define-key swiper-map (kbd "C-o") #'swiper-isearch-toggle)
  ;; (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper-isearch . ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper-isearch-backward . ivy--regex-plus))
  )

(use-package imenu-list
  :defer t
  :init
  (setq-default imenu-list-position 'left)
  (global-set-key (kbd "C-c b i") #'imenu-list-smart-toggle))

(use-package counsel
  :diminish
  :defer 0.25
  :init
  (setq-default counsel-find-file-at-point t       ;; Select file at point
		counsel-preselect-current-file t)   ;; Select current file in list
  :config
  (easy-mmode-defmap counsel-basic-map
    `(;;([remap switch-to-buffer] . counsel-switch-buffer)
      ;;([remap switch-to-buffer-other-window] . counsel-switch-buffer-other-window)
      ("c" . ivy-resume)               ;; resume ivy
      ("a" . counsel-ag)
      ("b" . counsel-ibuffer)          ;; like ibuffer + switch-to-buffer
      ("i" . counsel-imenu)
      ("r" . counsel-rg)	       ;; like git grep
      ("g" . counsel-grep)             ;; grep in local file
      ("G" . counsel-git-grep)         ;; grep in current git repo
      ("e" . counsel-linux-app)        ;; call application
      ("l" . counsel-find-library)     ;; Search lisp libraries
      (,(kbd "SPC") . counsel-register)     ;; list registers
      (,(kbd "RET") . counsel-company)    ;; company completions
      (,(kbd "C-r") . counsel-command-history) ;; command history
      ("p" . counsel-package)          ;; command history
      ("P" . counsel-list-processes)   ;; command history
      ;; counsel-file commands
      ("fg" . counsel-git)             ;; find file in git rempo
      ("fj" . counsel-file-jump)       ;; file in subdir
      ("fl" . counsel-locate)          ;; locate command (como search)
      ("fr" . counsel-recentf)
      ("fz" . counsel-fzf)
      ("fb" . counsel-buffer-or-recentf))
    "The base keymap for `counsel-mode'.")

  (define-key counsel-mode-map (kbd "C-c c") counsel-basic-map)
  (which-key-add-keymap-based-replacements counsel-mode-map
    "C-c c" "counsel"
    "C-c c f" "counsel-file")
  (counsel-mode 1)
  ;; match by words
  ;; (add-to-list 'ivy-re-builders-alist '(counsel-M-x . ivy--regex-fuzzy))
  )

(use-package amx :defer t) ;; Complete history

;; recentf
(with-eval-after-load 'recentf
  (recentf-mode 1))

 ;; Complete history


(use-package counsel-gtags
  :diminish
  :load-path (lambda nil (my/load-path "~/gits/emacs_clones/emacs-counsel-gtags/"))
  :bind-keymap ("C-c g" . counsel-gtags-command-map)
  :init
  (setq-default counsel-gtags-debug-mode t
		counsel-gtags-use-dynamic-list nil)
  (which-key-add-key-based-replacements "C-c g" "counsel-gtags")
  :config
  (which-key-add-keymap-based-replacements counsel-gtags-mode-map
    "C-c g 4" "counsel-gtags-other")

  (defun my/counsel-gtags-hook ()
    (my/company-backend-after-load #'company-gtags))

  ;; Promote company-gtags to the beginning.
  (add-hook 'counsel-gtags-mode-hook #'my/counsel-gtags-hook)
  (add-hook 'c-mode-hook #'my/counsel-gtags-hook)
  (add-hook 'c++-mode-hook #'my/counsel-gtags-hook)
  (add-hook 'objc-mode-hook #'my/counsel-gtags-hook)

  (counsel-gtags-mode 1))

(use-package global-tags ;; gtags with xref integration
  :after counsel-gtags   ;; don't remove this.
  :demand t
  :config
  (which-key-add-keymap-based-replacements counsel-gtags-mode-map
    "C-c g x" "global-tags")
  ;; assert the :after counsel-gtags
  (define-key counsel-gtags-command-map "xc" #'global-tags-create-database)
  (define-key counsel-gtags-command-map "xu" #'global-tags-update-database)

  (add-to-list 'xref-backend-functions #'global-tags-xref-backend)
  (add-to-list 'project-find-functions #'global-tags-try-project-root))

(use-package dumb-jump
  :bind-keymap ("C-c j" . dumb-jump-mode-map)
  :defer t
  :init
  (setq-default dumb-jump-selector 'ivy
		dumb-jump-disable-obsolete-warnings t)
  (which-key-add-key-based-replacements "C-c j" "dumb-jump")
  :config
  (define-key dumb-jump-mode-map "j" #'dumb-jump-go)
  (define-key dumb-jump-mode-map "o" #'dumb-jump-go-other-window)
  (define-key dumb-jump-mode-map "e" #'dumb-jump-go-prefer-external)
  (define-key dumb-jump-mode-map "4" #'dumb-jump-go-prefer-external-other-window)
  (define-key dumb-jump-mode-map "i" #'dumb-jump-go-prompt)
  (define-key dumb-jump-mode-map "q" #'dumb-jump-quick-look)
  (define-key dumb-jump-mode-map "b" #'dumb-jump-back)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;__________________________________________________________
;; Magit and git packages

(use-package magit :defer t
  :init
  (setq-default magit-completing-read-function #'ivy-completing-read ;; this is autoset
		magit-define-global-key-bindings nil
		magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
		;;magit-bury-buffer-function #'magit-mode-quit-window
		)
  :config
  (add-hook 'after-save-hook (lambda nil
			       (unless (file-remote-p default-directory)
				 (magit-after-save-refresh-status))))

  (add-hook 'magit-log-mode-hook (lambda nil
				   (setq-local show-trailing-whitespace nil
					       tab-width 4))))

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
  :init
  (setq-default git-commit-summary-max-length 68)
  :config
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)

  (add-hook 'git-commit-setup-hook (lambda nil
				     (setq-local fill-column 72)
				     (git-commit-turn-on-flyspell))))


;; smerge
(setq-default smerge-diff-buffer-name "*smerge-diff*"
	      smerge-command-prefix (kbd "C-c s"))

(defun my/enable-smerge-maybe ()
  "Auto-enable `smerge-mode' when merge conflict is detected."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(with-eval-after-load 'smerge
  (which-key-add-keymap-based-replacements smerge-mode-map
    "C-c s" "smerge"
    "C-c s =" "smerge-diff"))

(add-hook 'find-file-hook #'my/enable-smerge-maybe)
(add-hook 'magit-diff-visit-file-hook #'my/enable-smerge-maybe)

;; diff-hl
(use-package diff-hl
  :preface
  (defun my/diff-hl-mode ()
    (when (or (and buffer-file-name
		   (not (file-remote-p buffer-file-name)))
	      (eq major-mode 'vc-dir-mode))
      (turn-on-diff-hl-mode)
      (unless (display-graphic-p)
	(diff-hl-margin-mode 1))))
  :defer t
  :init
  (add-hook 'prog-mode-delay-hook #'my/diff-hl-mode)
  (add-hook 'vc-dir-mode-hook #'my/diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
  :config
  ;; Add the hook only after the package is loaded because they are not autoloads.
  (add-hook 'magit-pre-refresh-hook (lambda nil
				      (unless (file-remote-p default-directory)
					(diff-hl-magit-pre-refresh))))
  (add-hook 'magit-post-refresh-hook (lambda nil
				       (unless (file-remote-p default-directory)
					 (diff-hl-magit-post-refresh)))))

;;__________________________________________________________
;; Ensamblador nasm
(use-package nasm-mode
  :mode ("\\.asm\\'" "\\.s\\'"))

(use-package flymake-nasm
  :defer t
  :init
  (add-hook 'asm-mode-hook-hook #'flymake-nasm-setup))

;;__________________________________________________________
;; CMake
(use-package cmake-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake(.in)?\\'" . cmake-mode)))

(use-package cmake-font-lock
  :defer t
  :preface
  (defun my/cmake-font-lock ()
    (let ((auto-refresh-defaults (boundp 'font-lock-keywords)))
      (cmake-font-lock-activate)
      (when auto-refresh-defaults
	(font-lock-refresh-defaults))))
  :init
  (add-hook 'cmake-mode-hook #'my/cmake-font-lock))

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
  "Execute shell COMMAND on buffer overwriting but preserve position."
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
  :defer t
  :init
  (global-set-key (kbd "M-<up>") #'move-dup-duplicate-up)
  (global-set-key (kbd "M-<down>") #'move-dup-duplicate-down)
  (global-set-key (kbd "C-M-<up>") #'move-dup-move-lines-up)
  (global-set-key (kbd "C-M-<down>") #'move-dup-move-lines-down)
  (global-set-key (kbd "C-M-<left>") (lambda nil (interactive) (transpose-words -1)))
  (global-set-key (kbd "C-M-<right>") #'transpose-words)
  (global-set-key (kbd "M-<left>") (lambda nil (interactive) (transpose-chars -1)))
  (global-set-key (kbd "M-<right>") #'transpose-chars))

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
  :defer t
  :init
  (eval-after-load 'isearch
    '(define-key isearch-mode-map (kbd "C-'") #'avy-isearch))

  (global-set-key (kbd "C-'") avy-basic-map)
  (which-key-add-key-based-replacements "C-'" "avy")
  ;; (avy-timeout-seconds 0.75)
  (setq-default ;;avy-style 'at        ;; default 'at-full
		avy-all-windows nil    ;; commands only in this window
		avy-all-windows-alt t  ;; with prefix commands in all windows
		avy-case-fold-search nil
		avy-highlight-first t
		avy-indent-line-overlay t ;; show highlight after non-whitespace
		avy-keys (number-sequence ?a ?z) ;; Order of proposals
		)
  :config
  (with-eval-after-load 'repeat
    (defvar avy-repeat-map
      (let ((map (make-sparse-keymap)))
	(define-key map "p" #'avy-prev)
	(define-key map "n" #'avy-next)
	(define-key map "'" #'avy-resume)
	map)
      "Keymap to repeat avy key sequences.")
    (put 'avy-prev 'repeat-map 'avy-repeat-map)
    (put 'avy-next 'repeat-map 'avy-repeat-map)))

(use-package avy-zap
  :defer t
  :init
  (global-set-key (kbd "M-Z") #'avy-zap-up-to-char-dwim)
  (global-set-key [remap zap-to-char] #'avy-zap-to-char-dwim))

;;__________________________________________________________
;; Arduino Mode

(use-package arduino-mode
  :mode ("\\.ino\\'" "\\.pde\\'"))

(use-package company-arduino
  :hook (arduino-mode . (lambda nil
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
  :init
  (setq-default arduino-cli-warnings 'all
		arduino-cli-verify t
		arduino-cli-mode-keymap-prefix (kbd "C-c C-t"))
  :config
  (arduino-cli-mode)
  (which-key-add-key-based-replacements "C-c C-t" "arduino-cli-mode"))

;;__________________________________________________________
;; Multiple Cursors

(global-unset-key (kbd "C-c <down-mouse-1>"))
(use-package multiple-cursors  ;; Multiple cursors package
  :defer t
  :init
  (easy-mmode-defmap mc-basic-map
    `(("l" . mc/edit-lines)
      ("a" . mc/mark-all-like-this)
      ("w" . mc/mark-all-words-like-this)
      ("r" . mc/mark-all-in-region)
      ("s" . mc/mark-all-in-region-regexp)
      ("e" . mc/mark-more-like-this-extended)
      ("n" . mc/mark-next-like-this)
      ("p" . mc/mark-previous-like-this)
      ("\M-f" . mc/mark-next-like-this-word)
      ("\M-b" . mc/mark-previous-word-like-this)
      ("\M-p" . mc/mark-pop)
      ("#" . mc/insert-numbers)
      ("L" . mc/insert-letters)
      ("\C-a" . mc/edit-beginnings-of-lines)
      ("\C-e" . mc/edit-ends-of-lines))
    "The base keymap for `multicursor'")

  (global-set-key (kbd "C-c m") mc-basic-map)
  (which-key-add-key-based-replacements "C-c m" "multi-cursors")

  (setq-default mc/always-run-for-all t
		mc/always-repeat-command t
		mc/edit-lines-empty-lines 'ignore))

(use-package iedit
  :defer t
  :init
  (setq-default iedit-auto-recenter nil)
  (define-key mc-basic-map "i" #'iedit-mode))

;;__________________________________________________________
;; Web mode
(use-package web-mode
  :mode ("\\.\\(p\\|dj\\)?html\\'"
	 "\\(\\.tpl\\)?\\.php\\'" "\\.[agj]sp\\'"
	 "\\.as[cp]x\\'" "\\.erb\\'")
  :init
  (setq-default web-mode-markup-indent-offset tab-width
		web-mode-css-indent-offset tab-width
		web-mode-code-indent-offset tab-width
		web-mode-enable-auto-pairing t
		web-mode-enable-auto-closing t
		web-mode-enable-css-colorization t))

(use-package company-web
  :hook (web-mode . (lambda nil
		      (my/company-backend-after-load #'company-web-html)))
  :defer t)

;; (use-package web-mode-edit-element
;;   :hook (web-mode . web-mode-edit-element-minor-mode))

;;__________________________________________________________
;; nginx mode
(use-package nginx-mode
  :mode ("sites-\\(?:available\\|enabled\\)\\'" "nginx\\.config\\'"))

(use-package company-nginx
  :hook (nginx-mode . (lambda nil
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
  :defer t
  :init
  (add-hook 'json-mode-delay-hook #'flymake-json-load))

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
  :defer t
  :init
  (add-hook 'yaml-mode-delay-hook #'flymake-yaml-load))

(use-package sudo-edit :defer t)

(use-package evil
  :defer t
  :init
  (setq-default evil-esc-delay 0.001
		evil-want-keybinding nil
		show-paren-when-point-inside-paren t)
  :config
  ;; Modeline color
  (defconst original-background (face-attribute 'mode-line :background))

  (add-hook 'evil-normal-state-entry-hook
	    (lambda nil
	      (set-face-attribute 'mode-line nil :background (my/named-color brightblack))))
  (add-hook 'evil-insert-state-entry-hook
	    (lambda nil
	      (set-face-attribute 'mode-line nil :background original-background)))
  (add-hook 'evil-visual-state-entry-hook
	    (lambda nil
	      (set-face-attribute 'mode-line nil :background (my/named-color green)))))

(use-package evil-collection
  :defer t
  :init
  (setq-default evil-collection-setup-minibuffer t)
  (add-hook 'evil-mode-hook #'evil-collection-init))

(use-package composable
  :diminish
  ;; :after which-key
  :preface
  :load-path (lambda nil (my/load-path "~/gits/emacs_clones/composable/"))
  :init
  (setq-default composable-mode-debug-level 3)
  :config
  (composable-mode)       ; Activates the default keybindings
  (composable-mark-mode))
					; Use composable with C-SPC

(use-package automark
  :diminish
  :load-path (lambda nil (my/load-path "~/gits/emacs_clones/automark-mode/"))
  :config
  (automark-mode 1))

(use-package slime
  :defer t
  :init
  (setq-default inferior-lisp-program "sbcl"
		slime-contribs '(slime-fancy)))

;; Navegacion por objetos... no lo he probado
(use-package objed
  :commands objed-mode)

;; Emacs en less y otros comandos
(use-package e2ansi
  :defer t)

(use-package i3wm-config-mode
  :mode "/i3/config\\'")

(use-package mutt-mode
  :mode "muttrc")

(provide 'init)

;;; init.el ends here
