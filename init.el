;;; init.el --- Emacs Initialization and Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;; Code:

;;__________________________________________________________
;; Internal Options

(prefer-coding-system 'utf-8)	        ;; Encoding
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default ;; tab-always-indent 'complete   ;; make tab key do indent only
	      ;; tab-first-completion 'word
	      ring-bell-function #'ignore
	      user-full-name "Ergus"
	      initial-scratch-message (format ";; Welcome %s!!" user-full-name)

	      inhibit-startup-message t
	      inhibit-startup-screen t
	      ;;tab-width 4                 ;; Tabulador a 4
	      ;;indent-tabs-mode t          ;; Indent with tabs
	      ;;fill-column 80              ;; default is 70
	      make-backup-files nil         ;; Sin copias de seguridad
	      create-lockfiles nil          ;; No lock files, good for tramp
	      visible-bell nil              ;; Flash the screen (def)
	      confirm-kill-processes nil    ;; no ask kill processes on exit
	      ;; read-key-delay 0.01           ;; already default
	      recenter-redisplay nil
	      ;;recenter-positions '(top middle bottom)
	      ;; line-move-visual nil       ;; move cursor visual lines
	      backward-delete-char-untabify-method nil ;; Don't untabify on backward delete

	      split-width-threshold 140     ;; Limit for vertical split (default 160)
	      ;; kill-whole-line t
	      kill-region-dwim 'emacs-word
	      load-prefer-newer t
	      ;; mark-even-if-inactive nil     ;; no mark no region
	      mark-ring-max 128             ;; Max number of marks in the ring
	      set-mark-command-repeat-pop t ;; Repeat pop mark with C-SPC
	      next-screen-context-lines 5   ;; Lines of continuity when scrolling
	      fast-but-imprecise-scrolling t
	      redisplay-skip-fontification-on-input t ;; Skip ‘fontification_functions‘ when there is input pending.
	      jit-lock-defer-time 0                   ;; similar to redisplay-skip-fontification-on-input
	                                              ;; This should make input smoother

	      x-stretch-cursor t                  ;; Draw cursor as wide as the gliph below
	      scroll-error-top-bottom t           ;; Move cursor before error scroll
	      scroll-preserve-screen-position t   ;; Cursor keeps screen pos
	      scroll-margin 1                     ;; Margen al borde
	      scroll-step 1                       ;; Scroll step (better conservatively)
	      scroll-conservatively 1000
	      window-combination-resize t         ;; Windows resize proportional
	      x-wait-for-event-timeout nil        ;; Not wait for events in X (when built with X)
	      pgtk-wait-for-event-timeout nil     ;; Not wait for events in pgtk
	      ;; jit-lock-stealth-load 60           ;; load of fontification (def: 200)
	      jit-lock-stealth-nice 0.2           ;; Time between fortifications (def: 0.5)
	      jit-lock-stealth-time 2             ;; Time to wait before fortifications (def: nil)
	      inhibit-default-init t              ;; Avoid emacs default init
	      term-suppress-hard-newline t        ;; Text can resize
	      echo-keystrokes 0.001                ;; Unfinished bindings in the echo area
	      confirm-kill-emacs nil              ;; No confirm exit emacs
	      disabled-command-function nil
	      auto-save-default nil               ;; No autosave
	      auto-save-list-file-name nil
	      ;; minibuffer interaction
	      ;; minibuffer-message-timeout 1     ;; default 2
	      read-quoted-char-radix 16           ;; Read number of chars with C-q
	      ;; kill-buffer-query-functions nil     ;; Functions to call before quering a buffer (nil default)
	                                          ;; Default asks if process running.
	      kill-do-not-save-duplicates t       ;; duplicate kill ring entries
	      kill-ring-max (* kill-ring-max 2)   ;; increase kill ring

	      eval-expression-print-length nil
	      eval-expression-print-level nil
	      enable-remote-dir-locals t              ;; Open remote dir locals in remote files.

	      suggest-key-bindings t                  ;; Ivy ya hace lo que esta opcion
	      truncate-lines t
	      ;; auto-hscroll-mode 'current-line         ;; scroll horizontally 1 line not all
	      save-interprogram-paste-before-kill t   ;; Save clipboard before replace
	      minibuffer-eldef-shorten-default t

	      ;; M-x show context-local commands
	      use-short-answers t                     ;; Use y or n to exit and other shorter answers.
	      ;; y-or-n-p-use-read-key t                 ;; use readkey and not minibuffer for y or n answers
	      goto-line-history-local t               ;; Buffer local goto-line history
	      switch-to-buffer-obey-display-actions t ;; switching the buffer respects display actions
	      bookmark-menu-confirm-deletion t        ;; ask confirmation to delete bookmark
	      bookmark-fontify t                      ;; Colorize bookmarked lines with bookmark-face
	      bookmark-save-flag 1                    ;; Save bookmarks immediately when added

	      register-use-preview t                  ;; newer interface to show registers

	      ;; translate-upper-case-key-bindings nil ;; Make keybindings case sensitive (inhibit binding translation)
	      outline-minor-mode-use-buttons t      ;; Use buttons to hide/show outlines

	      help-window-select t                  ;; always select help windoes
	      help-window-keep-selected t           ;; Reuse *help* buffer when available
	      history-delete-duplicates t           ;; delete duplicates in commands history
	      history-length 200
	      find-library-include-other-files nil  ;; find-library only shows libraries, not random files.
	      view-read-only t                      ;; buffers visiting files read-only do so in view mode
	      kill-read-only-ok t                   ;; don’t signal an error for killing read-only text.
	      debugger-stack-frame-as-list t        ;; display call stack frames as lists.
	      async-shell-command-display-buffer nil ;;command buffer wait until there is output
	      shell-kill-buffer-on-exit t
	      ;;large-file-warning-threshold nil    ;; no warning when the file is too big
	      proced-enable-color-flag t            ;; colors in proced
	      )

;; Vertical window divider
(set-display-table-slot standard-display-table
			'vertical-border
			(make-glyph-code ?\u2502))  ;; also works 2503, it is wider
;;__________________________________________________________
;; use-package

;; Function to see the dependencies list.
;; (defvar my/require-tree nil)
;; (defun require--advice (orig-fun feature &rest args)
;;   (setq my/require-tree
;; 	(append my/require-tree
;; 		(list (let ((my/require-tree (list feature)))
;; 			(apply orig-fun feature args)
;; 			my/require-tree)))))

;; (advice-add 'require :around 'require--advice)

;; (debug-on-entry #'package--download-one-archive)

(setq-default package-native-compile t
	      package-quickstart t)

(eval-after-load 'package
  '(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

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
				    (if init-file-debug
				      (message "Running delay hook for %s: %s"
					       `,mode-name
					       ,delayhook))
				    (my/unset-gc)
				    (run-hooks ',delayhook)
				    (my/restore-gc))))
			      (current-buffer)))
       (add-hook ',modehook (function ,funame)))))

(defmacro my/repeat-keymap (newkeymap keymap &rest defs)
  "Generate a NEWKEYMAP and then add it to a another KEYMAP.

This is inteded to add the same bindings to a keymap like `C-x
M-<left>' and repeat with M-<left>."
  (declare (indent 2))
  `(progn
     (defvar-keymap ,newkeymap :repeat t ,@defs)
     ;; Add the keys with same suffix to `keymap'
     ,@(let ((sets))
	 (while-let ((key (car defs))
		     (value (cadr defs)))
	   (unless (eq key :doc)
	     (push `(keymap-set ,keymap ,key ,value) sets))
	   (setq defs (cddr defs)))
	 (nreverse sets))))

(if init-file-debug
    (progn
      (require 'use-package-ensure)
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

(defun my/find-init ()
  "Open the init file to edit it."
  (interactive)
  (find-file user-init-file))

(use-package esup :defer t)

;;__________________________________________________________
;; Config file not here to not track it
(setq-default custom-file
	      (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  (message "Creating %s" custom-file))
(load custom-file)

;;__________________________________________________________
;; Keybindings
(use-package diminish :defer t)   ;; if you use :diminish

;; which-key
(setq-default which-key-idle-delay 2.0
	      which-key-show-early-on-C-h t
	      which-key-idle-secondary-delay 0.01
	      which-key-side-window-max-height 0.15
	      ;; which-key-dont-use-unicode t
	      which-key-is-verbose init-file-debug
	      which-key-show-remaining-keys t
	      which-key-lighter nil
	      ;;which-key-popup-type 'minibuffer
	      ;;which-key-show-prefix 'mode-line
	      which-key-separator ": ")

(which-key-mode t)
(which-key-add-key-based-replacements
    "C-x r" "rectangle||register"
    "C-x n" "narrow"
    "C-x p" "project"
    "C-x RET" "coding-system"
    "C-x @" "event-apply-modifier"
    "C-x ESC" "repeat-command"
    "C-x 8" "unicode"
    "C-x x" "buffer"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x w" "window"
    "C-x C-k" "kmacro")
(eval-after-load 'which-key '(diminish 'which-key-mode))

;; sidebar
(defvar-keymap my/sidebar-map
  :doc "Keymap to toggle sidebars.")
(keymap-global-set "C-c b" (cons "sidebars" my/sidebar-map))

(defvar-keymap my/ctrl-c-c
  :doc "The base keymap for `C-c c'."
  "l" #'find-library)
(keymap-global-set "C-c c" (cons "my/ctrl-c-c" my/ctrl-c-c))

;;__________________________________________________________
;; Some internal packages to defer them

;; Modeline
(setq-default mode-line-position-column-line-format '(" (%l,%C)")  ;; column number start on 1
              ;; mode-line-compact t                                  ;; no spaces on ml
	      mode-line-frame-identification " "                   ;; no F1 in term
              mode-line-front-space " "                            ;; no - on the very left
              mode-line-end-spaces " "                             ;; no ---- on the right.
              mode-line-mule-info "")                               ;; no UUU: on the left.

(column-number-mode t)               ;; Column number
(line-number-mode t)                 ;; Line number
(size-indication-mode t)             ;; Size in modeline

;; Line numbers and fill column
(setq-default display-line-numbers-widen t)     ;; keep line numbers inside a narrow
(global-display-line-numbers-mode t)            ;; line numbers on the left
(global-display-fill-column-indicator-mode t)

;; Save history
(savehist-mode t)

;; Compress and delete selection
(auto-compression-mode t)               ;; Uncompress on the fly
(delete-selection-mode t)               ;; Override selection

;; imenu
(setq-default imenu-use-markers nil
	      imenu-auto-rescan t
	      imenu-max-item-length 256)
(with-eval-after-load 'imenu
  (add-hook 'imenu-after-jump-hook #'pulse-momentary-highlight-one-line))

;; uniquify
(setq-default uniquify-buffer-name-style 'forward) ;; default 'post-forward-angle-brackets

;; saveplace
(save-place-mode 1)                           ;; Remember point in files
(setq save-place-ignore-files-regexp  ;; Modified to add /tmp/* files
      (replace-regexp-in-string "\\\\)\\$" "\\|^/tmp/.+\\)$"
				save-place-ignore-files-regexp t t))

;; show-parent
(setq-default show-paren-delay 0
	      show-paren-context-when-offscreen t ;; show context in the echo area
	      ;; show-paren-when-point-inside-paren t
	      blink-matching-paren nil)      ;; not show matching parent in echo when closing

;; autorevert
(setq-default ffap-machine-p-known 'accept   ;; stop ffap from pinging random hosts
	      ffap-require-prefix t          ;; require prefix for ffap
	      dired-at-point-require-prefix t ;; Como ffap-require-prefix para directorios
	      auto-revert-verbose nil        ;; not show message when file changes
	      auto-revert-mode-text ""
	      auto-revert-avoid-polling t)   ;; don't do pooling for autorevert (use notifications).)

(run-with-idle-timer 1 nil (lambda ()
			     (ffap-bindings)
			     (recentf-mode 1)
			     (global-auto-revert-mode 1))) ;; Autoload files changed in disk

;; recentf
(setq-default recentf-max-saved-items 48     ;; Max items saved
	      recentf-auto-cleanup nil)      ;; Make cleanup when idle for 10 seconds. (default 'mode)

(with-eval-after-load 'recentf
  (run-with-idle-timer 10 nil #'recentf-cleanup))

;;__________________________________________________________
;; Profiler
(add-hook 'profiler-report-mode-hook #'hl-line-mode)

(autoload #'profiler-running-p "profiler")
(defvar my/profile-start-time nil)
(defvar my/profile-start-gcs-done nil)
(defvar my/profile-start-gcs-elapsed nil)

(defun my/profiler-set (arg)
  "Change the profiler status and reset some report variables."
  (interactive "P")
  (let ((profiler-on (profiler-running-p 'cpu)))
    (cond
     ((and arg (not profiler-on))
      (profiler-reset)
      (garbage-collect)
      (setq my/profile-start-time (current-time)
	    my/profile-start-gcs-done gcs-done
	    my/profile-start-gcs-elapsed gc-elapsed)
      (profiler-start 'cpu))
     ((and (not arg) profiler-on)
      (profiler-stop)
      (message "Profiled: %s secs & %s gcs-done %s gcs-elapsed"
	       (float-time (time-subtract (current-time) my/profile-start-time))
	       (- gcs-done my/profile-start-gcs-done)
	       (- gc-elapsed my/profile-start-gcs-elapsed))
      (profiler-report))
     (t (user-error
	 (format "Profiler status (%s) and arg (%s) mistmatch"
		 profiler-on arg))))))

(defun my/profiler-toggle ()
  (interactive)
  (my/profiler-set (not (profiler-running-p 'cpu))))

(defmacro my/with-profiler (&rest body)
  "Execute function body with profiler."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
	 (my/profiler-set 1)
	 ,@body)
     (my/profiler-set nil)))

(keymap-global-set "M-P" 'my/profiler-toggle)

(defun my/package-delete (pkg)
  "Remove package PKG if it is installed."
  (when (package-installed-p pkg)
    (package-delete (package-get-descriptor pkg))))

;; Shows the function in spaceline
(eval-after-load 'which-func '(diminish 'which-func-mode))
(setq-default which-func-update-delay 0.2)  ;; Substitutes idle-update-delay

;; delay hooks
(my/gen-delay-hook text-mode)
(my/gen-delay-hook prog-mode)
(my/gen-delay-hook conf-mode)

;; elec-pair
;; (eval-after-load 'elec-pair
;;   '(add-to-list 'electric-pair-pairs '(?< . ?>) t))

(defun my/delete-trailing-function ()
  "Hook that removes trailing whitespaces when RET at end of line."
  (when (and (not electric-indent-mode)
	     (eq last-command-event ?\n)
	     (eq (char-before) ?\n))
    (save-excursion
      (backward-char 1)
      (when (and (eolp))
        (delete-horizontal-space t)))))
(add-hook 'post-self-insert-hook #'my/delete-trailing-function)

;; Show the tab indicator symbol in whitespace mode
(with-eval-after-load 'whitespace
  (setq whitespace-style '(faces tab-mark)
	whitespace-display-mappings `((tab-mark ?\t [,(make-glyph-code ?» 'whitespace-tab) ?\t] ))
	))

(defun my/delayed-common-hook ()
  "Enable electric-pair-local-mode"
  (setq-local show-trailing-whitespace t  ;; Show trailing whitespaces
	      indicate-empty-lines t      ;; Show empty lines at end of file
	      )
  (whitespace-mode 1)
  (electric-pair-local-mode 1))

;; New indent bars mode ready to test: https://github.com/jdtsmith/indent-bars
;; (use-package indent-bars
;;   :config
;;   (require 'indent-bars-ts) 		; not needed with straight
;;   :custom
;;   (indent-bars-treesit-support t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   ;; Add other languages as needed
;;   (indent-bars-treesit-scope '((python function_definition
;; 				       class_definition
;; 				       for_statement
;; 				       if_statement
;; 				       with_statement
;; 				       while_statement)))
;;   ;; Note: wrap may not be needed if no-descend-list is enough
;;   ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
;;   ;;				      list list_comprehension
;;   ;;				      dictionary dictionary_comprehension
;;   ;;				      parenthesized_expression subscript)))
;;   :hook ((python-base-mode yaml-mode) . indent-bars-mode))

(add-hook 'prog-mode-delay-hook #'my/delayed-common-hook)
(add-hook 'text-mode-delay-hook #'my/delayed-common-hook)
(add-hook 'conf-mode-delay-hook #'my/delayed-common-hook)

;; hl-line
(keymap-global-set "M-s h L" #'hl-line-mode)
(eval-after-load 'hl-line '(diminish 'hl-line-mode))
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; hilit-chg changes
(defvar-keymap highlight-changes-map
  :doc "The base keymap for `highlight changes'."
  "c" #'highlight-changes-mode
  "v" #'highlight-changes-visible-mode
  "r" #'highlight-changes-remove-highlight
  "n" #'highlight-changes-next-change
  "p" #'highlight-changes-previous-change
  "f" #'highlight-changes-rotate-faces
  "b" #'highlight-compare-buffers
  "d" #'highlight-compare-with-file)
(keymap-global-set "M-s h c" (cons "highlight-changes" highlight-changes-map))

;; winner
(setq-default winner-dont-bind-my-keys t)
(winner-mode t)

;; There is already a winner-repeat-map with different bindings
;; so I cannot use the same, becaus eas it is loaded latter,
;; it will be overwritten
(my/repeat-keymap my/winner-repeat-map ctl-x-4-map
  :doc "Repeat map for `winner' commands."
  "u"  #'winner-undo
  "r"  #'winner-redo)

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; abbrev
(which-key-add-key-based-replacements "C-x a" "abbrev")
(eval-after-load 'abbrev '(diminish 'abbrev-mode))


;; eldoc
(setq-default eldoc-idle-delay 2                              ;; default 0.5
	      eldoc-print-after-edit t                        ;; only show after edit
	      eldoc-minor-mode-string nil                     ;; nothing in the modeline
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

;; hideif mode
(setq-default hide-ifdef-shadow t
	      hide-ifdef-initially t)
(with-eval-after-load 'hideif
  (diminish 'hide-ifdef-mode)
  (diminish 'hide-ifdef-hiding))

;; subword
(eval-after-load 'subword '(diminish 'subword-mode))

;; vc
(setq-default vc-follow-symlinks t          ;; Open links not open
	      vc-handled-backends '(Git Hg) ;; Only git or mercurial
	      vc-display-status nil         ;; No info on the modeline.
	      vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
					   vc-ignore-dir-regexp
					   tramp-file-name-regexp))
(which-key-add-key-based-replacements "C-x v" "vc")

;; Context Menu
(context-menu-mode 1)

;; Man
(setq-default Man-notify-method 'pushy)

;; dabbrev
(autoload #'dabbrev-filter-elements "dabbrev")

(defun my/dabbrev--select-project-buffersffers ()
  "Dabbrev list of buffers in the same project and apply the filters."
  (let ((pr (project-current nil)))
    (dabbrev-filter-elements
     buffer (if pr (project-buffers pr) (buffer-list))
     (and (not (eq (current-buffer) buffer))
	  (not (dabbrev--ignore-buffer-p buffer))
	  (boundp 'dabbrev-friend-buffer-function)
	  (funcall dabbrev-friend-buffer-function buffer)))))

(setq-default dabbrev-check-all-buffers nil
	      dabbrev-ignored-buffer-regexps '("\\`[ *]")
	      ;; Buffers to add if dabbrev-check-other-buffers is non-nil
	      dabbrev-select-buffers-function #'my/dabbrev--select-project-buffers)

;; completion
(setq-default completion-show-help nil              ;; Don't show help header in completion buffer
	      completion-auto-help 'visible         ;; Update completions when visible and no hide
	      completion-auto-select 'second-tab    ;; Show completions on second tab (default nil)
	      ;; minibuffer-visible-completions t      ;; Jury completions selection
	      completion-auto-wrap t                ;; wrap movement
	      completions-detailed t                ;; show more detailed completions
	      completions-format 'one-column        ;; Vertical completion list
	      completions-max-height 15
	      completion-styles '(substring partial-completion emacs22)
	      ;; M-x show context-local commands
	      read-extended-command-predicate #'command-completion-default-include-p
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t
	      completion-ignore-case t

	      completion-auto-deselect t            ;; De-select completions on write
	      completions-sort 'historical          ;; alphabetical + historical
	      
	      minibuffer-completion-auto-choose nil ;; no insert completions in minib
	      )

(defun my/fido-mode ()
  "Command to toggle fido-vertial-mode and disable completions."
  (interactive)
  (if (or fido-mode fido-vertical-mode)
      (progn
	(fido-vertical-mode -1)
	(fido-mode -1)
	(setq completion-auto-help 'visible))

    (fido-vertical-mode 1)
    (setq completion-auto-help nil)))

;; project
(setq-default project-vc-include-untracked nil
	      project-mode-line t)

;; {previous,next}-buffer only move within this project
(defun with-current-project (funct)
  "Call FUNCT setting `switch-to-prev-buffer-skip'."
  (let ((switch-to-prev-buffer-skip
	 (lambda (_window buffer _bury-or-kill)
	   (when-let ((pr1 (project-current)))
	     (with-current-buffer buffer
	       (not (and buffer-file-name
			 (eq pr1 (project-current)))))))))
    (call-interactively funct)))

(defun project-next-buffer ()
  "Next buffer within project."
  (interactive)
  (with-current-project #'next-buffer))

(defun project-previous-buffer ()
  "Previous buffer within project."
  (interactive)
  (with-current-project #'previous-buffer))

(my/repeat-keymap my/project-prefix-map project-prefix-map
  :doc "Next buffer in current project."
  "C-<right>" #'project-next-buffer
  "C-<left>" #'project-previous-buffer)

;; These two must be enabled/disabled together
;; (setq-default enable-recursive-minibuffers t) ;; Enable nesting in minibuffer
;; (minibuffer-depth-indicate-mode 1)            ;; Mostrar nivel de nesting en minibuffer

(add-hook 'minibuffer-setup-hook #'my/unset-gc)
(add-hook 'minibuffer-exit-hook #'my/restore-gc)

;; Arrows up/down search prefix in history like `history-search-backward' in bash
(keymap-set minibuffer-local-map "<down>" #'next-complete-history-element)
(keymap-set minibuffer-local-map "<up>" #'previous-complete-history-element)

(defun my/completion-setup-hook ()
  "My hook for Completions window."
  (with-current-buffer standard-output
    (setq-local mode-line-format nil)
    (display-line-numbers-mode -1)))

(add-hook 'completion-setup-hook #'my/completion-setup-hook 10)

(defconst my/display-buffer-at-bottom
  '((display-buffer-reuse-mode-window display-buffer-at-bottom)
    (dedicated . t)
    (window-height . 0.3)
    (window-width . 1.0)
    (preserve-size . (t . t))
    (inhibit-same-window . t))
  "Windows configuration for display-buffer-alist")

;;__________________________________________________________
;; Isearch

(setq-default search-nonincremental-instead nil    ;; No incremental if enter & empty
	      lazy-highlight-no-delay-length 1     ;; normal delay
	      ;; lazy-highlight-initial-delay 0       ;; old config replaced by lazy-highlight-no-delay-length
	      isearch-allow-scroll t               ;; Permit scroll can be 'unlimited
	      isearch-lazy-count t
	      search-ring-max 256
	      regexp-search-ring-max 256
	      isearch-yank-on-move 'shift          ;; Copy text from buffer with meta
	      ;; isearch-wrap-function #'ignore       ;; Look at the emacs-major-version check
	      ;; isearch-wrap-pause t                 ;; Disable wrapping nil.
	      isearch-repeat-on-direction-change t ;; Don't go to the other end on direction change
	      isearch-lax-whitespace nil
	      ;; isearch-regexp-lax-whitespace t      ;; swiper like fuzzy search
	      ;; search-whitespace-regexp ".*?"
	      ;; Emacs version > 28
	      lazy-highlight-no-delay-length 1     ;; use this instead of lazy-highlight-initial-delay
	      isearch-allow-motion t
	      isearch-forward-thing-at-point '(region symbol sexp word)
	      ;; isearch-motion-changes-direction t
	      )

(defun my/isearch-forward ()
  (interactive)
  (let ((search-whitespace-regexp ".*?")
	(search-spaces-regexp ".*?")
	(isearch-lax-whitespace t))
    (call-interactively 'isearch-forward)
    ))

(with-eval-after-load 'isearch
  (defun my/isearch-exit-other-end ()
    (interactive)
    (when isearch-other-end
      (goto-char isearch-other-end))
    (call-interactively #'isearch-exit))

  (keymap-set isearch-mode-map "C-RET" #'my/isearch-exit-other-end)
  (keymap-set isearch-mode-map "C-<return>" #'my/isearch-exit-other-end)
;;  (keymap-set isearch-mode-map "<remap> <isearch-abort>" #'isearch-exit)
  (keymap-set isearch-mode-map "<remap> <isearch-delete-char>" #'isearch-del-char)

  (keymap-set search-map "." #'isearch-forward-thing-at-point)
  (which-key-add-key-based-replacements "M-s h" "highlight")

  (when (fboundp #'avy-isearch)
    (keymap-set isearch-mode-map "C-'" #'avy-isearch))

  (when (fboundp  #'iedit-mode-from-isearch)
    (keymap-set isearch-mode-map "C-c m i" #'iedit-mode-from-isearch))
  )

(use-package phi-search :defer t)

(setq-default list-matching-lines-jump-to-current-line t)

(with-eval-after-load 'replace  ;; is where occur resides
  ;; With error follow this is pointless.
  (keymap-set occur-mode-map "SPC" #'occur-mode-display-occurrence)
  (add-hook 'occur-hook (lambda ()
			  (beginning-of-line)
			  (recenter nil t)))

  (add-hook 'occur-mode-hook (lambda ()
			       (setq-local window-size-fixed 'height)
			       (hl-line-mode 1)
			       (display-line-numbers-mode -1)
			       (switch-to-buffer-other-window "*Occur*")
			       ;; (next-error-follow-minor-mode 1)
			       ))

  (add-hook 'occur-mode-find-occurrence-hook
	    (lambda ()
	      (let ((win (get-buffer-window "*Occur*")))
		(when (and win
			   (eq this-command #'occur-mode-goto-occurrence))
		  (quit-restore-window win)
		  (isearch-done)))))

  (add-to-list 'display-buffer-alist `("*Occur*" . ,my/display-buffer-at-bottom)))

;;__________________________________________________________
;; The Colors I am using my own theme
(load-theme 'simple-16)

(defun my/set-font ()
  "Conditionally set the Hack font."
  (cond
   ((member "Hack" (font-family-list))
    (set-face-attribute 'default nil :family "Hack" :height 105))
   ((member "Cascadia Mono" (font-family-list))
    (set-face-attribute 'default nil :family "Cascadia Mono" :height 105))))

(cond
 ((daemonp) (add-hook 'server-after-make-frame-hook
		      (lambda ()
			(when (display-graphic-p)
			  (my/set-font)))))
 ((display-graphic-p) (my/set-font)))

(defalias 'my/named-color 'simple-16-theme-color)

;;__________________________________________________________
;;Packages options
;;__________________________________________________________

;;__________________________________________________________
;; Some bindings
(keymap-global-set "<remap> <delete-char>" #'delete-forward-char) ;; delete respecting with C-d
(keymap-global-set "<remap> <count-words-region>" #'count-words)  ;; count on whole file or region if active


;;__________________________________________________________
;; compile
(setq-default compilation-scroll-output nil
	      compilation-context-lines t   ;; Don't scroll compilation buffer
	      compilation-always-kill t)

;;; Display compilation buffer at buttom
(add-to-list 'display-buffer-alist `((major-mode . compilation-mode) . ,my/display-buffer-at-bottom))

(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook
	    (lambda ()
	      (setq window-size-fixed 'width))))

;;__________________________________________________________
;; ssh
(setq-default tramp-auto-save-directory
	      (expand-file-name "tramp-autosave-dir" user-emacs-directory)
	      tramp-default-method "ssh"                   ;; Already default
	      remote-file-name-inhibit-cache 60            ;; Default 10
	      tramp-completion-reread-directory-timeout 120;; Default 10
	      password-cache-expiry 3600                   ;; Cache for 1 hour
	      tramp-use-scp-direct-remote-copying t        ;; copy directly between remote hosts
	      remote-file-name-inhibit-locks t          ;; I know that different Emacs sessions are not modifying the same remote file
	      tramp-verbose (if init-file-debug 10 3)      ;; Default 3 always
	      ;; tramp-use-ssh-controlmaster-options nil      ;; use system control master.
	      tramp-use-connection-share nil
	      tramp-completion-use-auth-sources nil        ;; not use auth-sources in tramp
	      )

(with-eval-after-load 'tramp
  ;; Tramp don't read the auth-sources on sudo-edit
  (connection-local-set-profile-variables 'my/tramp-profile
					  '((auth-sources . nil)))
  (connection-local-set-profiles '(:application tramp :protocol "sudo") 'my/tramp-profile)

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-process-environment
               (format "DISPLAY=%s" (getenv "DISPLAY"))))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
	 ("/sshd?_config\\'" . ssh-config-mode)
	 ("/known_hosts\\'" . ssh-known-hosts-mode)
	 ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

;; Edit files as sudo
(use-package sudo-edit :defer t)

;;__________________________________________________________
;; splitting
(setq-default windmove-display-no-select t) ;; select windows after displaying it

(defvar-keymap my/0-map
  :doc "The base keymap for `highlight changes'."
  "0" #'delete-window
  "<left>" #'windmove-delete-left
  "<right>" #'windmove-delete-right
  "<up>" #'windmove-delete-up
  "<down>" #'windmove-delete-down)

;; (keymap-set ctl-x-map "0" my/0-map)
;; (which-key-add-key-based-replacements "C-x 0" "windmove-delete")

;; Direct shortcut without prefix.
(keymap-set ctl-x-map "<left>" #'windmove-left)
(keymap-set ctl-x-map "<right>" #'windmove-right)
(keymap-set ctl-x-map "<down>" #'windmove-down)
(keymap-set ctl-x-map "<up>" #'windmove-up)

(keymap-set ctl-x-map "S-<left>" #'windmove-swap-states-left)
(keymap-set ctl-x-map "S-<right>" #'windmove-swap-states-right)
(keymap-set ctl-x-map "S-<down>" #'windmove-swap-states-down)
(keymap-set ctl-x-map "S-<up>" #'windmove-swap-states-up)

(keymap-set ctl-x-4-map "<left>" #'windmove-display-left)
(keymap-set ctl-x-4-map "<right>" #'windmove-display-right)
(keymap-set ctl-x-4-map "<down>" #'windmove-display-down)
(keymap-set ctl-x-4-map "<up>" #'windmove-display-up)

(keymap-set ctl-x-map "0" (cons "windmove-delete" my/0-map))

;;__________________________________________________________
;; tab-bar
(setq-default tab-bar-tab-hints t  ;; show tab numbers
	      tab-bar-close-last-tab-choice 'tab-bar-mode-disable ;; When close last
	      tab-bar-show 1)
(which-key-add-key-based-replacements "C-x t" "tab-bar")  ;; by default

(defvar-keymap my/tmux-like-keymap
  :doc "A keymap that emulates some of the tmux bindings."
  "i" #'tab-new
  "k" #'tab-close
  "1" #'tab-close-other
  "r" #'tab-rename
  "0" (cons "windmove-delete" my/0-map)
  "v" #'split-window-below
  "h" #'split-window-right
  "b" #'switch-to-buffer-other-tab
  "d" #'dired-other-tab

  "<left>" #'windmove-left
  "<right>" #'windmove-right
  "<down>" #'windmove-down
  "<up>" #'windmove-up)

;; Add repeatable bindings to my/tmux-like-keymap
(my/repeat-keymap my/tmux-repeat-map my/tmux-like-keymap
  :doc "Repeat map for tmux prefix"
  "C-<left>" #'tab-previous
  "C-<right>" #'tab-next
  "M-S-<left>" #'tab-bar-move-tab-backward
  "M-S-<right>" #'tab-bar-move-tab

  "S-<left>" #'windmove-swap-states-left
  "S-<right>" #'windmove-swap-states-right
  "S-<down>" #'windmove-swap-states-down
  "S-<up>" #'windmove-swap-states-up)

(keymap-global-set "C-z" my/tmux-like-keymap)

;;__________________________________________________________
;; Two options for diffs
(setq-default
 ediff-window-setup-function #'ediff-setup-windows-plain
 ;; default ediff-split-window-function #'split-window-vertically
 ediff-split-window-function  (lambda (&optional arg)
				(if (> (frame-width) 150)
				    (split-window-horizontally arg)
				  (split-window-vertically arg))))
(with-eval-after-load 'winner
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;; more like vimdiff
(use-package vdiff :defer t
  :bind-keymap ("C-c d" . vdiff-mode-prefix-map)
  :init
  (setq-default vdiff-auto-refine t)
  (which-key-add-key-based-replacements "C-c d" "vdiff")
  :config
  (which-key-add-key-based-replacements "C-c d i" "vdiff-toggle")
  )

;;__________________________________________________________
;; terms
(defvar-keymap my/term-keymap
  :doc "Keymap for terminal commands")
(keymap-global-set "C-c t" (cons "term" my/term-keymap))

(use-package vterm :defer t
  :hook (vterm-mode . (lambda nil
			(display-fill-column-indicator-mode -1)
			(auto-fill-mode -1)))
  :init
  (setq-default vterm-kill-buffer-on-exit t
		vterm-max-scrollback 10000)
  :config
  ;; Add find-file-other-window to accepted commands
  (add-to-list 'vterm-eval-cmds
	       '("find-file-other-window" find-file-other-window))

  (keymap-unset vterm-mode-map "C-z")  ;; I use C-x to change tab
  (keymap-unset vterm-mode-map "M-O")  ;; Fix arrow keys in vterm
  (keymap-set vterm-mode-map "C-c C-z" #'vterm-send-C-z)
  (keymap-set vterm-mode-map "C-c C-x" #'vterm-send-C-x)
  (keymap-set vterm-mode-map "C-c [" #'vterm-copy-mode)
  (keymap-set vterm-mode-map "C-c ]" #'vterm-yank)
  (keymap-set vterm-copy-mode-map "M-w" #'vterm-copy-mode-done)
  )

(use-package vterm-toggle :defer t
  :init
  (setq-default vterm-toggle-scope 'project
		vterm-toggle-project-root t    ;; Already default
		vterm-toggle-fullscreen-p nil  ;; Already default
		)
  (keymap-set my/term-keymap "t" #'vterm-toggle)
  :config
  (keymap-set vterm-mode-map "M-RET" #'vterm-toggle-insert-cd)
  (add-to-list 'display-buffer-alist `(,vterm-buffer-name . ,my/display-buffer-at-bottom)))

(use-package emamux :defer t
  :bind-keymap (("C-c u" . emamux:keymap))
  :init
  (which-key-add-key-based-replacements "C-c u" "emamux")
  :config
  (setq emamux:keymap (make-sparse-keymap))
  (if (emamux:in-tmux-p)
      (progn
	(keymap-set emamux:keymap "u" #'emamux:run-command)
	(keymap-set emamux:keymap "r" #'emamux:run-region)
	(keymap-set emamux:keymap "0" #'emamux:close-panes)
	(keymap-set emamux:keymap "k" #'emamux:close-panes)
	(keymap-set emamux:keymap "i" #'emamux:inspect-runner)
	(keymap-set emamux:keymap "k" #'emamux:interrupt-runner)
	(keymap-set emamux:keymap "s" #'emamux:send-command)
	(keymap-set emamux:keymap "<up>" #'emamux:run-last-command)
	(keymap-set emamux:keymap "C-y" #'emamux:yank-from-list-buffers)
	;; (keymap-set emamux_keymap "M-k" #'emamux:clear-runner-history)
	;; (keymap-set emamux_keymap "c"   #'emamux:new-window)
	;; (keymap-set emamux_keymap "C"   #'emamux:clone-current-frame)
	;; (keymap-set emamux_keymap "2"   #'emamux:split-window)
	;; (keymap-set emamux_keymap "3"   #'emamux:split-window-horizontally)
	)
    (message "TMUX not running in this terminal")
    (keymap-global-unset "C-c u")
    ))

(use-package pkgbuild-mode
  :mode "/PKGBUILD$")

(use-package tmux-mode :defer t)

;;__________________________________________________________
;; Better shell (for ssh)
(use-package better-shell :defer t
  :init
  (keymap-set my/term-keymap "b" #'better-shell-shell))

(use-package shell-command+ :defer t
  :init
  (keymap-global-set "<remap> <shell-command>" #'shell-command+))

;;__________________________________________________________
;; eshell mouse

(defun my/with-face (str &rest face-plist)
    (propertize str 'face face-plist))

(defun my/eshell-prompt-function ()
  "Personalized Eshell prompt."
  (concat
   (my/with-face (concat (user-login-name) "@" (system-name))
		 :foreground (simple-16-theme-color green))
   (my/with-face (concat ":" (abbreviate-file-name (eshell/pwd)))
		 :foreground (simple-16-theme-color blue))
   (if (= (file-user-uid) 0) " #" " $")
   `,(my/with-face "\n>" :foreground (simple-16-theme-color cyan))
   " "))

(setq-default eshell-history-append t   ;; No override eshell history; append
	      eshell-prompt-function #'my/eshell-prompt-function
	      eshell-highlight-prompt nil
	      eshell-buffer-name "*Eshell*")

;;__________________________________________________________
;; Clipboard copy and paste with: M-w & C-c v
(use-package xclip
  :preface
  (setq-default xclip-method (cond
			      ((or (display-graphic-p)  ;; graphic or linux terminal
				   (string-equal (getenv "TERM") "linux"))
			       nil)
			      ((and (string-equal "x11" (getenv "XDG_SESSION_TYPE"))
				    (executable-find "xclip")) ;; x11
			       'xclip)
			      ((and (string-equal "wayland" (getenv "XDG_SESSION_TYPE"))
				    (executable-find "wl-copy")) ;; wayland
			       'wl-copy)
			      ((message "Error xclip failed to get environment OR external program") nil)
			      ))
  :if xclip-method
  :config
  (xclip-mode 1))

;;__________________________________________________________
;; xterm mouse
(setq-default mouse-drag-mode-line-buffer t)

(unless (or (display-graphic-p)
	    (string-equal (getenv "TERM") "linux"))
  (xterm-mouse-mode t))                    ;; mover el cursor al click

(if (fboundp #'mouse-wheel-mode)
    (progn
      (setq-default mouse-wheel-scroll-amount '(3             ;; No modifier
						((control) . 9)
						((meta) . hscroll)
						((shift) . text-scale)) ;; in terminal does not work
		    mouse-wheel-tilt-scroll t          ;; horizontal scrolling with touchpad
		    mouse-wheel-progressive-speed nil
		    mouse-scroll-delay 0)
      (mouse-wheel-mode 1))                    ;; Explicit call mouse-wheel-mode AFTER setting mouse-wheel-scroll-amount

  ;; Else set them manually, will be overridden latter.
  (keymap-global-set "<mouse-4>" #'scroll-down-command)
  (keymap-global-set "<mouse-5>" #'scroll-up-command))

(keymap-global-set "<remap> <scroll-up-command>" #'scroll-up-line)
(keymap-global-set "<remap> <scroll-down-command>" #'scroll-down-line)

;;__________________________________________________________
;; Redefine and remap some commands.

(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (call-interactively 'back-to-indentation)
    (and (<= oldpos (point))
	 (/= (line-beginning-position) oldpos)
	 (call-interactively 'beginning-of-line))))

(keymap-global-set "<remap> <move-beginning-of-line>" #'my/smart-beginning-of-line)

;;__________________________________________________________
;; Undo
(setq-default undo-only t)               ;; undo does not go throw redo entries
(keymap-global-set "C-M-/" #'undo-redo)  ;; For gui; in tty "C-M-/" == "C-M-_"
;; (global-set-key (kbd "C-M-_") #'undo-redo) already set by default

(defvar-keymap undo-redo-repeat-map
  :doc "Keymap to repeat undo-redo key sequences."
  "u" #'undo
  "r" #'undo-redo)
(put #'undo 'repeat-map 'undo-redo-repeat-map)
(put #'undo-redo 'repeat-map 'undo-redo-repeat-map)

;; (use-package undo-propose :defer t)

(use-package vundo :defer t
  :init
  (setq-default vundo-compact-display t)
  (keymap-set undo-redo-repeat-map "v" #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;__________________________________________________________
;; Cycle string capitalization for programming:
;; "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
(use-package string-inflection :defer t
  :init
  (keymap-global-set "C-c SPC" #'string-inflection-all-cycle)

  :config
  (defvar-keymap string-inflection-repeat-map
    :doc "Keymap to repeat inflection. Used in `string-inflection-all-cycle'."
    "SPC" #'string-inflection-all-cycle)

  (put #'string-inflection-all-cycle 'repeat-map 'string-inflection-repeat-map))

;;__________________________________________________________
;; Mark column 80 when crossed

(use-package highlight-indent-guides :defer t
  :diminish
  :init
  (setq-default highlight-indent-guides-auto-enabled nil
		highlight-indent-guides-method 'character)
  (keymap-global-set "M-s h i" #'highlight-indent-guides-mode)
  :config
  (set-face-attribute 'highlight-indent-guides-character-face nil
		      :foreground (my/named-color brightblack)))

;;__________________________________________________________
;; Flyspell (Orthography)
(setq-default ispell-following-word t ;;Check word around point not only before
	      ispell-quietly t)       ;; Suppress messages in ispell-word

;; Flyspell
(setq-default flyspell-use-meta-tab nil       ;; Not correct with M-TAB
	      flyspell-mode-line-string nil   ;; Not show Fly in modeline
	      flyspell-delay 1                ;; default 3
	      flyspell-sort-corrections t     ;; Alphabetically sort corrections
	      flyspell-issue-welcome-flag nil ;; no message on start
	      flyspell-issue-message-flag nil ;; no message when checking
	      )

(add-hook 'prog-mode-delay-hook #'flyspell-prog-mode)
(add-hook 'text-mode-delay-hook #'turn-on-flyspell)

(with-eval-after-load 'flyspell
  (defvar-keymap flyspell-basic-map
    :doc "The base keymap for `flyspell-mode'."
    "r" #'flyspell-region
    "b" #'flyspell-buffer
    "n" #'flyspell-goto-next-error)

  (setf (cdr flyspell-mode-map) nil)  ;; clear yas minor map
  (keymap-set flyspell-mode-map "C-c f" (cons "flyspell" flyspell-basic-map)))

(use-package flyspell-correct
  :diminish
  :after flyspell
  :config
  (keymap-set flyspell-basic-map "w" #'flyspell-correct-wrapper)
  (keymap-set flyspell-basic-map "f" #'flyspell-correct-at-point)
  (keymap-set flyspell-basic-map "C-n" #'flyspell-correct-next)
  (keymap-set flyspell-basic-map "C-p" #'flyspell-correct-previous)
  )

;;__________________________________________________________
;; Completions

(use-package eglot :defer t
  :init
  (setq-default eglot-events-buffer-config '(:size 2000000 :format lisp)
		eglot-send-changes-idle-time 1.0  ;; Server idle
		eglot-extend-to-xref t            ;; Include external headers and sources
		eglot-ignored-server-capabilities '(:inlayHintProvider
						    :documentRangeFormattingProvider
						    :documentOnTypeFormattingProvider)
		)

  ;; (setq-default eglot-stay-out-of '(eldoc))
  :config
  (add-to-list 'eglot-server-programs '((cuda-mode) "clangd"))

  (add-hook 'eglot-managed-mode-hook
	    (lambda ()
	      (when (eldoc--supported-p)
		(eldoc-mode (if (eglot-managed-p) 1 -1))))))

;; (use-package consult-eglot :defer t)

(use-package corfu
  ;; Optional customizations
  :defer 0.2
  :init
  (setq-default corfu-cycle t)      ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :config
  (global-corfu-mode)
  (keymap-set corfu-mode-map "M-RET" #'completion-at-point)
  (keymap-set corfu-map "M-RET" #'corfu-quit)
  )

;; Completion preview mode
(use-package completion-preview :ensure nil
  :hook ((prog-mode-delay . completion-preview-mode)
	 (text-mode-delay . completion-preview-mode)
	 (conf-mode-delay . completion-preview-mode)
	 (message-mode-delay . completion-preview-mode)
	 (shell-mode . completion-preview-mode)
	 (eshell-mode . completion-preview-mode)
	 (vterm-mode . completion-preview-mode))
  :config
  (keymap-set completion-preview-active-mode-map "TAB" #'completion-preview-complete)
  )

(use-package corfu-terminal
  :defer 0.2
  :config
  (corfu-terminal-mode +1))

(use-package cape
  :defer 0.2
  :init
  (setq-default cape-dabbrev-check-other-buffers nil) ;; default t

  (defvar-keymap my/cape-basic-map
    :doc "The keymap used when `cape-basic-map' is active."
    "p" 'completion-at-point ;; capf
    "t" 'complete-tag        ;; etags
    "d" 'cape-dabbrev        ;; or dabbrev-completion
    "f" 'cape-file
    "k" 'cape-keyword
    "s" 'cape-symbol
    "a" 'cape-abbrev
    "i" 'cape-ispell
    "l" 'cape-line
    "w" 'cape-dict
    "x" 'cape-tex
    "g" 'cape-sgml
    "r" 'cape-rfc1345)

  (setq-local completion-at-point-functions
              (list (cape-capf-super #'cape-dabbrev #'cape-keyword)))

  ;;(add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)

  (keymap-global-set "C-c p" (cons "cape" my/cape-basic-map))

  (add-hook 'TeX-mode-hook
	    (lambda nil
	      (add-to-list 'completion-at-point-functions #'cape-tex))))


;;__________________________________________________________
;; C common mode (for all c-like languages)
(defun ms-space-for-alignment-hook ()
  "Make the current line use tabs for indentation and spaces for alignment.

It is intended to be called from the hook
`c-special-indent-hook'.  It assumes that `indent-tabs-mode' is
non-nil and probably assumes that `c-basic-offset' is the same as
`tab-width'."
  (when (and indent-tabs-mode
	     (= c-basic-offset tab-width))
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
      (back-to-indentation))))

(define-minor-mode c-ms-space-for-alignment-mode
  "Enable indent with tabs align with spaces."
  :global nil
  :init-value nil
  (if c-ms-space-for-alignment-mode
      (add-hook 'c-special-indent-hook #'ms-space-for-alignment-hook nil t)
    (remove-hook 'c-special-indent-hook #'ms-space-for-alignment-hook t)))

;;==============================
;; Special function to (un)indent nested namespaces in C++

(defun ms-unindent-nested-namespace ()
  "Indent namespaces properly.
Nested namespaces should not be indented with new indentations."
  (save-excursion
    (back-to-indentation)
    (let ((initial-pos (point))
          (syn-elt (car c-syntactic-context)))
      (when (and (eq (c-langelem-sym syn-elt) 'innamespace)
                 (looking-at-p "namespace[[:blank:]]+[[:alnum:]:]+[[:space:]]*{")
                 (re-search-backward "namespace[[:blank:]]+[[:alnum:]:]+[[:space:]]*{" nil t))
        (let ((end (point))
              (start (line-beginning-position)))
          (goto-char initial-pos)
          (delete-horizontal-space)
          (insert-buffer-substring-no-properties (current-buffer) start end)
          )))))

(define-minor-mode c++-unindent-namespace-mode
  "Enable indent with tabs align with spaces."
  :global nil
  :init-value nil
  (if c++-unindent-namespace-mode
      (add-hook 'c-special-indent-hook #'ms-unindent-nested-namespace nil t)
    (remove-hook 'c-special-indent-hook #'ms-unindent-nested-namespace t)))

;;====================
;; cc-mode
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
		 (c-doc-comment-style . doxygen)    ;; Use doxygen comments
		 (c-cleanup-list empty-defun-braces ;; {}
				 brace-else-brace   ;; } else {
				 brace-elseif-brace ;; } else if {
				 defun-close-semi   ;; }; after class
				 )
		 (c-hanging-braces-alist (defun-open before after) ;; function\n{\n
					 (brace-list-open)         ;; brace
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
				  )))

  (defun my/c-mode-common-hook ()
    "my/c-mode-common common."
    (c-toggle-auto-newline 1)
    (c-toggle-cpp-indent-to-body 1)
    (c-ms-space-for-alignment-mode 1)
    ;; (hide-ifdef-mode 1)
    (subword-mode 1))
  (add-hook 'c-mode-common-hook #'my/c-mode-common-hook))

(use-package google-c-style :defer t
  :after cc-mode
  :config
  (c-add-style "Google" google-c-style))

;; (use-package preproc-font-lock :defer t ;; Preprocessor
;;   :init
;;   (add-hook 'c-mode-common-hook #'preproc-font-lock-mode)
;;   (setq-default preproc-font-lock-preprocessor-background-face 'font-lock-preprocessor-face))

(use-package clang-format :defer t)

;;__________________________________________________________
;; C++ mode
(use-package modern-cpp-font-lock :defer t
  :diminish modern-c++-font-lock-mode
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

;;__________________________________________________________
;; elisp mode (all after the company declaration)

;; (add-hook 'emacs-lisp-mode-hook
;; 	  (lambda nil
;; 	    (when (and buffer-file-name
;; 		       (string-match "\\.el\\'" buffer-file-name))
;; 	      (my/company-backend-after-load #'company-elisp))))

;;__________________________________________________________
;; sh mode
(defvaralias 'sh-basic-offset 'tab-width)

(add-hook 'sh-mode-hook (lambda nil
			  (setq-local indent-tabs-mode t
				      tab-width 4)))

;;__________________________________________________________
;; Cuda
(use-package cuda-mode :defer t
  :preface
  (when (file-exists-p "/mnt/casa/gits/emacs_clones/cuda-mode/")
    (add-to-list 'load-path "/mnt/casa/gits/emacs_clones/cuda-mode")))

;;__________________________________________________________
;; OpenCL Mode
(use-package opencl-mode
  :mode "\\.cl\\'")

;;__________________________________________________________
;; Markdown mode
;; (use-package markdown-mode
;;   :mode (("README\\.md\\'" . gfm-mode))
;;   :init
;;   (setq-default markdown-indent-on-enter nil))
(use-package markdown-ts-mode)


;;__________________________________________________________
;; Restructured text
(use-package sphinx-mode :defer t
  :init
  (add-hook 'rst-mode-hook #'sphinx-mode))

;;__________________________________________________________
;; ruby-mode
;; Remap ruby-mode with the tree-sitter alternative
(setq-default ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.rjs\\'" . ruby-mode))

;;__________________________________________________________
;; Julia Mode

(use-package julia-ts-mode :defer t)

;;__________________________________________________________
;; Ocaml Mode
(use-package caml :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . caml-mode)))

;;__________________________________________________________
;; D languaje
(use-package d-mode :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.d\\'" . d-mode)))

;;__________________________________________________________
;; lua language
(use-package lua-mode :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;__________________________________________________________
;; groovy language
(use-package groovy-mode :defer t)

;;__________________________________________________________
;; systemd mode
(use-package systemd :defer t)

;;__________________________________________________________
;; haskell mode
(use-package haskell-mode :defer t)

;;__________________________________________________________
;; xml-mode
(add-to-list 'auto-mode-alist
	     '("\\.\\(ipe\\|qrc\\|svn\\)\\'" . xml-mode))
;;__________________________________________________________
;; repeat-mode
(setq-default repeat-check-key nil
	      repeat-exit-key (kbd "RET"))
(repeat-mode 1)

;; Bind repeat to next/prev buffer
(my/repeat-keymap my/next-prev-repeat-map ctl-x-map
  :doc "Repeat map for `next|prev-buffer' commands."
  "C-<left>" #'previous-buffer
  "C-<right>" #'next-buffer)

;;__________________________________________________________
;; Lines enabling gnuplot-mode
(use-package gnuplot :defer t
  :init
  (add-to-list 'auto-mode-alist
	       '("\\.\\(gpl?\\|plt\\)\\'" . gnuplot-mode)))

;;__________________________________________________________
;; Auto complete with snippets
(use-package yasnippet        ;; Snippets
  :diminish yas-minor-mode
  :defer t
  :init
  (setq-default yas-verbosity (if init-file-debug 4 0) ; No need to be so verbose
		;; yas-wrap-around-region t
		)
  (defvar-keymap yas-minor-basic-map
    :doc "The keymap used when `yas-minor-mode' is active."
    "d" #'yas-load-directory
    "f" #'yas-visit-snippet-file
    "n" #'yas-new-snippet
    "t" #'yas-tryout-snippet
    "l" #'yas-describe-tables
    "x" #'yas-expand
    "i" #'yas-insert-snippet)

  (keymap-global-set "C-c y" (cons "yasnippet" yas-minor-basic-map))

  (autoload #'yas-expand "yasnippet" "Insert snippet" t)
  (autoload #'yas-insert-snippet "yasnippet" "Insert snippet" t)
  (autoload #'yas-expand-or-insert "yasnippet" "Insert snippet" t)

  :config
  (setf (cdr yas-minor-mode-map) nil)  ;; clear yas minor map
  (yas-global-mode 1))

(use-package yasnippet-snippets :after yasnippet)

;;__________________________________________________________
;; Chequeo de syntaxis
;; (use-package flycheck :defer t
;;   :diminish
;;   :when (< (buffer-size) 200000)
;;   :preface
;;   (defun my/flycheck-mode-hook ()
;;     "Hook to enable flycheck-mode."
;;     (pcase major-mode
;;       ('c-mode
;;        (setq-local flycheck-gcc-language-standard "c17"
;; 		   flycheck-clang-language-standard "c17"))
;;       ('c++-mode
;;        (setq-local flycheck-gcc-language-standard "c++20"
;; 		   flycheck-clang-language-standard "c++20")))
;;     (flycheck-mode 1))
;;   :init
;;   ;;(add-hook 'prog-mode-delay-hook #'my/flycheck-mode-hook)
;;   (setq-default flycheck-display-errors-delay 0.5
;; 		flycheck-idle-change-delay 1.0
;; 		flycheck-keymap-prefix (kbd "C-c a"))
;;   :config
;;   (which-key-add-keymap-based-replacements flycheck-mode-map "C-c a" "flycheck"))

(setq-default flymake-no-changes-timeout 1.0
	      flymake-wrap-around nil
	      ;; flymake-show-diagnostics-at-end-of-line t  ;; I want to try that
	      flymake-mode-line-format nil)

(with-eval-after-load 'flymake
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (defvar-keymap flymake-basic-map
    :doc "The base keymap for `flymake-mode'."
    "d" #'flymake-show-diagnostic
    "b" #'flymake-show-buffer-diagnostics
    "l" #'flymake-switch-to-log-buffer)

  (my/repeat-keymap flymake-repeat-map flymake-basic-map
    "n" #'flymake-goto-next-error
    "p" #'flymake-goto-prev-error)

  (keymap-set flymake-mode-map "C-c k" (cons "flymake" flymake-basic-map)))

;;__________________________________________________________
;; Chequeo de gramatica
(use-package languagetool :defer t
  :init
  (setq-default languagetool-java-arguments '("-Dfile.encoding=UTF-8"
					      "-cp" "/usr/share/languagetool:/usr/share/java/languagetool/*")
		languagetool-console-command "org.languagetool.commandline.Main"
		languagetool-server-command "org.languagetool.server.HTTPServer"))

;;__________________________________________________________
;; Email mode for mutt
;;__________________________________________________________
;; message-mode
(my/gen-delay-hook message-mode)

(setq-default send-mail-function #'smtpmail-send-it
	      smtpmail-default-smtp-server "smtp.aol.com"
	      smtpmail-local-domain "aol.com"
	      smtpmail-stream-type  'ssl
	      smtpmail-smtp-service 465
	      smtpmail-debug-info init-file-debug   ;; show delivery info
	      ;; smtpmail-debug-verb init-file-debug   ;; instruct the server to be verbose
	      message-default-mail-headers "Cc: \nBcc: \n"
	      message-kill-buffer-on-exit t
	      message-send-mail-function #'message-use-send-mail-function
	      mail-header-separator ""
	      auth-sources '("/mnt/casa/mail/authinfo.gpg")
	      user-mail-address "spacibba@aol.com"
	      )

(with-eval-after-load 'message-mode
  (auto-fill-mode t)
  (mail-abbrevs-setup)
  (flyspell-mode t))

(add-to-list 'auto-mode-alist '("/neomut" . message-mode))
(add-to-list 'auto-mode-alist '("neomutt-Ergus-" . message-mode))
(add-to-list 'auto-mode-alist '("draft" . message-mode))

(use-package notmuch :defer t
  :init
  (setenv "NOTMUCH_CONFIG" (expand-file-name "/mnt/casa/mail/notmuch-config"))
  )

;;__________________________________________________________
;; Latex mode
(use-package auctex :defer t)

(setq-default TeX-source-correlate-start-server t
	      TeX-auto-save t
	      TeX-parse-self t
	      LaTeX-babel-hyphen nil
	      TeX-master nil         ;; Multidocument
	      LaTeX-indent-level 4
	      LaTeX-item-indent 0
	      ;; TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o"))
	      ;; TeX-PDF-mode t          ;; Already default t
	      ;; TeX-show-compilation t  ;; Show compilation buffer.
	      )

(with-eval-after-load 'latex ;; needed for LaTeX-indent-environment-list
  (defun my/LaTeX-indent-item ()
    "Syntactic indentation for itemize like environments to add extra offsets."
    (save-match-data
      (let* ((offset (+ LaTeX-indent-level LaTeX-item-indent)) ;; item indent
	     (re-beg "\\\\begin{")
	     (re-end "\\\\end{")
	     (re-env "\\(itemize\\|\\enumerate\\|description\\)")
	     (indent (save-excursion
		       (when (looking-at (concat re-beg re-env "}"))
			 (end-of-line))
		       (LaTeX-find-matching-begin)
		       (current-column))))
	(cond
	 ((looking-at (concat re-beg re-env "}"))
	  ((save-excursion
	     (beginning-of-line)
	     (ignore-errors
	       (LaTeX-find-matching-begin)
	       (+ (current-column)
		  (if (looking-at (concat re-beg re-env "}"))
		      offset
		    LaTeX-indent-level)))
	     indent)))
	 ((looking-at (concat re-end re-env "}")) indent)
	 ((looking-at "\\\\item") (+ indent offset))
	 (t (+ indent offset LaTeX-indent-level))))))
  (add-to-list 'LaTeX-indent-environment-list '("itemize" my/LaTeX-indent-item))
  (add-to-list 'LaTeX-indent-environment-list '("enumerate" my/LaTeX-indent-item))
  (add-to-list 'LaTeX-indent-environment-list '("description" my/LaTeX-indent-item))
  (add-to-list 'TeX-command-list '("Make" "make -k" TeX-run-compile nil t))
  )

(add-hook 'TeX-mode-hook (lambda ()
			   (LaTeX-math-mode 1)
			   ;; (auto-fill-mode 1)           ;; It causes issues and M-q saves the day.
			   (TeX-source-correlate-mode 1))) ;; open PDF in the edditing page
(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))

;; reftex
(setq-default reftex-cite-prompt-optional-args t   ; Prompt for empty optional arguments in cite
	      reftex-cite-format 'biblatex
	      reftex-plug-into-AUCTeX t
	      reftex-insert-label-flags t
	      reftex-save-parse-info t
	      reftex-enable-partial-scans t
	      reftex-use-multiple-selection-buffers t)

(add-hook 'TeX-mode-hook #'turn-on-reftex)

(eval-after-load 'reftex '(reftex-isearch-minor-mode))

;;__________________________________________________________
;;bibtex mode set use biblatex
(setq-default bibtex-dialect 'biblatex)
(add-to-list 'auto-mode-alist '("\\.bib\\'" . bibtex-mode))

;;__________________________________________________________
;; Python mode
;; There is a tree-sitter module for this, I will try that

(setq-default python-shell-interpreter "ipython"
              python-shell-interpreter-args "--simple-prompt"
	      ;;python-shell-prompt-detect-failure-warning nil
	      python-check-command "pyflakes"
	      ;; flycheck-python-flake8-executable "flake8"
	      python-indent-block-paren-deeper t
	      python-shell-dedicated 'project
	      python-shell-completion-native-enable t
	      python-forward-sexp-function nil
	      )

(eval-after-load 'python
  '(keymap-set python-mode-map "C-c C-z" #'python-shell))

(use-package py-vterm-interaction :defer t
  :init
  (add-hook 'python-ts-mode-hook #'py-vterm-interaction-mode)
  (setq-default py-vterm-interaction-repl-program "ipython3 -i"
		py-vterm-interaction-silent-cells t))

;;__________________________________________________________
;; Dired-mode settings (file manager)
(setq-default dired-recursive-copies 'top                  ;; Always ask recursive copy
	      dired-recursive-deletes 'top                 ;; Always ask recursive delete
	      dired-dwim-target 'dired-dwim-target-recent  ;; Copy in split mode with p
	      dired-auto-revert-buffer (lambda (dirname)
					 (and (not (file-remote-p dirname))
					      (dired-directory-changed-p dirname))) ;; auto revert dired
	      ;; dired-listing-switches "-alh"                ;; commands to ls
	      dired-listing-switches "-agho --group-directories-first"
	      dired-isearch-filenames 'dwim
	      dired-hide-details-hide-symlink-targets nil  ;; don't hide linkk targets
	      dired-maybe-use-globstar t                   ;; use shell's globstar
	      dired-kill-when-opening-new-dired-buffer t   ;; kill when opening a new directory.
	      dired-mouse-drag-files t
	      dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open")
					     ("\\.jpe?g\\'" "xdg-open")
					     ("\\.png\\'" "xdg-open")
					     ("\\.gif\\'" "xdg-open")))

(with-eval-after-load 'dired
  (require 'dired-x)
  (keymap-set dired-mode-map "<mouse-2>" #'dired-mouse-find-file)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(use-package dired-sidebar :defer t
  :init
  ;;(dired-sidebar-use-term-integration t)
  (setq-default dired-sidebar-theme 'nerd
		dired-sidebar-subtree-line-prefix ".")
  (add-hook 'dired-sidebar-mode-hook (lambda ()
				       (unless (file-remote-p default-directory)
					 (auto-revert-mode 1))))
  (keymap-set my/sidebar-map "d" #'dired-sidebar-toggle-sidebar))

;; __________________________________________________________
;; Templates Projects
(use-package ptemplate :defer t)

(use-package ptemplate-templates
  :after ptemplate
  :config
  (ptemplate-templates-mode 1))
;;__________________________________________________________
;; ibuffer
(setq-default ibuffer-default-sorting-mode 'alphabetic  ;; can use recency)
	      ibuffer-use-other-window t                ;; ibuffer in other windows
	      ibuffer-jump-offer-only-visible-buffers t)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)
(eval-after-load 'ibuffer
  '(which-key-add-keymap-based-replacements ibuffer--filter-map "G" "Groups"))

(use-package ibuffer-sidebar :defer t
  :init
  (keymap-set my/sidebar-map "b" #'ibuffer-sidebar-toggle-sidebar))

;; Sidebar Dired+ibuffer (de emacs defaults)
(defun my/sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (dired-sidebar-toggle-sidebar))

(keymap-set my/sidebar-map "s" #'my/sidebar-toggle)

;;__________________________________________________________
;; neotree (Like dired sidebar but a bit fancier.)
(use-package neotree :defer t
  :init
  (keymap-set my/sidebar-map "n" #'neotree-toggle))

;;__________________________________________________________
;; Ivy (probare un tiempo con helm/ivy)
(use-package flx :defer t)

(with-eval-after-load 'xref
  (setq-default xref-search-program 'ripgrep
		xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom
		xref-show-xrefs-function #'xref-show-definitions-buffer-at-bottom)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

(defvar-keymap my/xref-basic-map
    :doc "The base keymap for `xref'."
    "d" #'xref-find-definitions
    "4" #'xref-find-definitions-other-window
    "a" #'xref-find-apropos
    "r" #'xref-find-references
    "TAB" #'completion-at-point)

(my/repeat-keymap my/xref-repeat-map my/xref-basic-map
    "p" #'xref-go-back
    "n" #'xref-go-forward)

(put #'xref-find-definitions 'repeat-map my/xref-repeat-map)
(put #'xref-find-references 'repeat-map my/xref-repeat-map)

(keymap-global-set "C-c x" (cons "xref" my/xref-basic-map))

(use-package imenu-list :defer t
  :init
  (setq-default imenu-list-position 'left
		imenu-list-focus-after-activation t
		imenu-list-size 0.2)
  (keymap-set my/sidebar-map "i" #'imenu-list-smart-toggle)
  :config
  (add-hook 'imenu-list-after-jump-hook #'pulse-momentary-highlight-one-line))

(use-package dumb-jump :defer t
  :bind-keymap ("C-c j" . dumb-jump-mode-map)
  :init
  (setq-default dumb-jump-selector 'completing-read
		dumb-jump-disable-obsolete-warnings t
		dumb-jump-prefer-searcher 'rg
		dumb-jump-quiet (not init-file-debug)
		dumb-jump-debug init-file-debug)
  (which-key-add-key-based-replacements
    "C-c j" "dumb-jump"
    "C-c j 4" "other-window")
  :config
  (keymap-set dumb-jump-mode-map "j" #'dumb-jump-go)
  (keymap-set dumb-jump-mode-map "4 j" #'dumb-jump-go-other-window)
  (keymap-set dumb-jump-mode-map "e" #'dumb-jump-go-prefer-external)
  (keymap-set dumb-jump-mode-map "4 e" #'dumb-jump-go-prefer-external-other-window)
  (keymap-set dumb-jump-mode-map "i" #'dumb-jump-go-prompt)
  (keymap-set dumb-jump-mode-map "l" #'dumb-jump-quick-look)
  (keymap-set dumb-jump-mode-map "p" #'dumb-jump-back)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package project-multi-mode :defer t :ensure nil
  :preface
  (when (file-exists-p "/mnt/casa/gits/emacs_clones/project-multi/")
    (add-to-list 'load-path "/mnt/casa/gits/emacs_clones/project-multi/"))
  :init
  :hook ((emacs-startup . project-multi-mode)))

(use-package gtags-mode :defer t
  :preface
  (when (file-exists-p "/mnt/casa/gits/emacs_clones/gtags-mode/")
    (add-to-list 'load-path "/mnt/casa/gits/emacs_clones/gtags-mode/"))
  :init
  (setq-default gtags-mode-lighter "")
  :hook ((emacs-startup . gtags-mode)))

;;__________________________________________________________
;; Magit and git packages
(use-package magit :defer t
  :init
  (setq-default magit-define-global-key-bindings nil
		magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
		;; This may help for tramp
		auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p

		;; magit-completing-read-function #'ivy-completing-read ;; this is not needed anymore.
		;;magit-bury-buffer-function #'magit-mode-quit-window
		)
  :config
  (keymap-unset magit-section-mode-map "C-<tab>" t) ;; magit-section-cycle shadows tab next

  (add-hook 'after-save-hook (lambda nil
			       (unless (file-remote-p default-directory)
				 (message "Refreshing magit for: %s" default-directory)
				 (magit-after-save-refresh-status))))

  (add-hook 'magit-log-mode-hook (lambda nil
				   (setq-local show-trailing-whitespace nil
					       tab-width 4))))
(use-package git-modes
  :mode (("\\.gitattributes\\'" . gitattributes-mode)
	 ("\\.gitconfig\\'" . gitconfig-mode)
	 ("\\.gitignore\\'" . gitignore-mode)))

(use-package git-timemachine :defer t)

;; (use-package git-commit :defer t
;;   :mode ("COMMIT_EDITMSG" . git-commit-setup)
;;   :init
;;   (setq-default git-commit-summary-max-length 68)
;;   :config
;;   (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)

;;   (add-hook 'git-commit-setup-hook (lambda nil
;; 				     (setq-local fill-column 72)
;; 				     (git-commit-turn-on-flyspell))))

(use-package with-editor
  :hook ((term-exec . with-editor-export-editor)
	 (shell-mode . with-editor-export-editor)
	 (eshell-mode . with-editor-export-editor)
	 (vterm-mode . with-editor-export-editor)))

;; smerge
(setq-default smerge-diff-buffer-name "*smerge-diff*"
	      smerge-command-prefix "\C-cs")

(defun my/enable-smerge-maybe ()
  "Auto-enable `smerge-mode' when merge conflict is detected."
  (when (or (not large-file-warning-threshold)
	    (< (buffer-size) large-file-warning-threshold))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
	(smerge-mode 1)))))

(with-eval-after-load 'smerge-mode
  (my/repeat-keymap smerge-repeat-map smerge-basic-map
    "n" #'smerge-next
    "p" #'smerge-prev
    "a" #'smerge-keep-all
    "b" #'smerge-keep-base
    "l" #'smerge-keep-lower
    "u" #'smerge-keep-upper)

  (which-key-add-keymap-based-replacements smerge-mode-map
    "C-c s" "smerge"
    "C-c s =" "smerge-diff"))

(add-hook 'find-file-hook #'my/enable-smerge-maybe)
(add-hook 'magit-diff-visit-file-hook #'my/enable-smerge-maybe)

;; diff-hl
(use-package diff-hl :defer t
  :preface
  (defun my/diff-hl-mode ()
    (turn-on-diff-hl-mode)
    (unless (display-graphic-p)
      (diff-hl-margin-mode 1)))
  :init
  (setq-default diff-hl-disable-on-remote t)
  (add-hook 'prog-mode-delay-hook #'my/diff-hl-mode)
  (add-hook 'vc-dir-mode-hook #'my/diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
  :config
  ;; Add the hook only after the package is loaded because they are not autoloads.
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;;__________________________________________________________
;; Ensamblador nasm
(use-package nasm-mode
  :mode ("\\.asm\\'" "\\.s\\'"))

(use-package flymake-nasm :defer t
  :init
  (add-hook 'asm-mode-hook-hook #'flymake-nasm-setup))

;;__________________________________________________________
;; Cobol
(use-package cobol-mode
  :mode ("\\.\\(cobc?\\|cbl\\|cpy\\)\\'"))

;;__________________________________________________________
;; my/ namespacs utilities.

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

(defun my/var-to-clipboard (var)
  "Put the current file name on the clipboard."
  (interactive
   (let* ((var-at-point (variable-at-point))
	  (default-var (and (custom-variable-p var-at-point) var-at-point)))
     (list (read-variable
	    (format-prompt "Set variable" default-var) default-var))))
  (let ((value (format "%s" (symbol-value var))))
    (kill-new value)
    (message "Copy value of %s to clipboard: %s " var value)))

;;__________________________________________________________
;; Move current line up and down C-M-<arrow> and duplicate

(defun my/untranspose-words (arg)
  (interactive "*p")
  (transpose-words (- arg)))

(defun my/untranspose-chars (arg)
  (interactive "*p")
  (transpose-chars (- arg)))

(my/repeat-keymap transpose-repeat-map ctl-x-map
  :doc "The keymap for `transpose-repeat' commands."
  "C-M-<left>" #'my/untranspose-words
  "C-M-<right>" #'transpose-words
  "M-<left>" #'my/untranspose-chars
  "M-<right>" #'transpose-chars)

;;__________________________________________________________
;; Move current line up and down C-M-<arrow> and duplicate
(use-package move-dup :defer t
  :init
  (my/repeat-keymap move-dup-repeat-map ctl-x-map
    :doc "The keymap for `move-dup-repeat' commands."
    "C-M-<up>" #'move-dup-move-lines-up
    "C-M-<down>" #'move-dup-move-lines-down
    "M-<up>" #'move-dup-duplicate-up
    "M-<down>" #'move-dup-duplicate-down))

;;__________________________________________________________
;; avy mode

(use-package avy :defer t
  :preface
  (defvar-keymap avy-basic-map
    :doc "The base keymap for `avy-mode'."
    "C-'" #'avy-goto-char-timer
    "'" #'avy-goto-char-timer
    "c" #'avy-goto-char
    "2" #'avy-goto-char-2
    "C-f" #'avy-goto-char-in-line
    "w" #'avy-goto-word-or-subword-1 ;; Alternative avy-goto-word
    "W" #'avy-goto-word-0            ;; All words, no initial char
    "C-s" #'avy-goto-word-1-below
    "C-r" #'avy-goto-word-1-above
    "M-b" #'avy-goto-word-0-above
    "M-f" #'avy-goto-word-0-below
    "s" #'avy-goto-symbol-1
    "C-a" #'avy-goto-line
    "C-e" #'avy-goto-end-of-line
    "C-n" #'avy-goto-line-below
    "C-p" #'avy-goto-line-above
    "C-w" #'avy-move-region
    "C-k" #'avy-kill-region
    "M-w" #'avy-kill-ring-save-region
    "C-SPC" #'avy-pop-mark
    "i" #'avy-copy-region)
  :init
  (keymap-global-set "C-'" avy-basic-map)
  ;; (which-key-add-key-based-replacements "C-'" "avy")
  (setq-default ;; avy-style 'at               ;; default 'at-full
		;; avy-all-windows nil         ;; commands only in this window
		;; avy-all-windows-alt t       ;; with prefix commands in all windows
		;; avy-case-fold-search nil    ;; Non-nil ignore case
		avy-timeout-seconds 0.3      ;; timeout for avy timer
		avy-highlight-first t
		avy-indent-line-overlay t    ;; show highlight after non-whitespace
		avy-keys '(?z ?x ?c ?v ?b ?n ?m
			      ?a ?s ?d ?f ?g ?h ?j ?k ?l
			      ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p) ;; Order of proposals
		)

  (my/repeat-keymap avy-repeat-map avy-basic-map
    :doc "Keymap to repeat some avy key sequences."
    "p" #'avy-prev
    "n" #'avy-next
    "r" #'avy-resume))

(keymap-global-set "M-Z" #'zap-up-to-char)

(use-package avy-zap :defer t
  :after avy
  :init
  (keymap-set avy-basic-map "z" #'avy-zap-to-char-dwim)
  (keymap-set avy-basic-map "u" #'avy-zap-up-to-char-dwim))

(use-package deadgrep :defer t
  :init
  (keymap-set my/ctrl-c-c "r" #'deadgrep)
  :config
  (keymap-set deadgrep-mode-map "SPC" (lambda ()
					(interactive)
					(next-error-follow-mode-post-command-hook))))

(use-package urgrep :defer t
  :init
  (setq-default urgrep-preferred-tools '(git-grep grep)
		urgrep-case-fold 'smart))


;;__________________________________________________________
;; Arduino Mode

;; (use-package arduino-mode
;;   :mode ("\\.ino\\'" "\\.pde\\'"))

(use-package arduino-cli-mode :defer t
  :init
  (setq-default arduino-cli-warnings 'all
		arduino-cli-verify t
		arduino-cli-mode-keymap-prefix (kbd "C-c C-t"))
  :config
  (arduino-cli-mode 1)
  (which-key-add-key-based-replacements "C-c C-t" "arduino-cli-mode"))

;;__________________________________________________________
;; Multiple Cursors

(use-package iedit :defer t
  :init
  (setq-default iedit-toggle-key-default nil) ;; this avoids calling iedit-update-key-bindings
  )

(global-unset-key (kbd "C-c <down-mouse-1>"))
(use-package multiple-cursors :defer t
  :init
  (setq-default mc/always-run-for-all t
		mc/always-repeat-command t
		mc/edit-lines-empty-lines 'ignore)

  (defvar-keymap mc-basic-map
    :doc "The base keymap for `multicursor'."
    "l" #'mc/edit-lines
    "a" #'mc/mark-all-dwim
    "d" #'mc/mark-all-like-this-dwim
    "." #'mc/mark-all-symbols-like-this
    "w" #'mc/mark-all-words-like-this
    "r" #'mc/mark-all-in-region
    "s" #'mc/mark-all-in-region-regexp
    "e" #'mc/mark-more-like-this-extended
    "n" #'mc/mark-next-like-this
    "p" #'mc/mark-previous-like-this
    "M-f" #'mc/mark-next-like-this-word
    "M-b" #'mc/mark-previous-word-like-this
    "M-p" #'mc/mark-pop
    "#" #'mc/insert-numbers
    "L" #'mc/insert-letters
    "C-a" #'mc/edit-beginnings-of-lines
    "C-e" #'mc/edit-ends-of-lines)

  (keymap-global-set "C-c m" (cons "multi-cursors" mc-basic-map))

  (when (fboundp #'iedit-mode)
    (keymap-set mc-basic-map "i" #'iedit-mode)))

;;__________________________________________________________
;; nginx mode
(use-package nginx-mode
  :mode ("sites-\\(?:available\\|enabled\\)\\'" "nginx\\.config\\'"))

;; (use-package company-nginx :defer t
;;   :hook (nginx-mode . (lambda nil
;; 			(my/company-backend-after-load #'company-nginx))))

(use-package lice :defer t
  :init
  (setq-default lice:copyright-holder "Jimmy Aguilar Mena"))
(use-package lorem-ipsum :defer t)

;;__________________________________________________________
;; csv mode
(use-package csv-mode :defer t
  :init
  (setq-default csv-align-style 'auto))

;;__________________________________________________________
;; Protobuf mode
(use-package protobuf-ts-mode
  :mode "\\.proto\\'")

;;__________________________________________________________

(use-package evil :defer t
  :init
  (setq-default evil-esc-delay 0.001
		evil-want-keybinding nil
		evil-undo-system 'undo-redo

		evil-want-C-u-scroll t
		evil-want-fine-undo t
		)
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

(use-package evil-collection :defer t
  :init
  (setq-default evil-collection-setup-minibuffer t)
  (add-hook 'evil-mode-hook #'evil-collection-init))

(use-package evil-leader :defer t
  :init
  (add-hook 'evil-mode-hook #'global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
   "e" 'find-file
   "f" 'projectile-find-file
   "b" 'switch-to-buffer
   "k" 'kill-buffer
   "1" 'delete-other-windows
   "m" 'helm-bookmarks
   "0" 'toggle-fullscreen
   "w" 'whitespace-mode
   ";" 'comment-line)
  )


;; (use-package composable
;;   :diminish
;;   :preface
;;   (my/load-path "/mnt/casa/gits/emacs_clones/composable/")
;;   :init
;;   (setq-default composable-mode-debug-level (if init-file-debug 3 0)
;; 		composable-mode-line-color "green")
;;   :config
;;   (composable-mode) 		;; Activates the default keybindings
;;   (composable-mark-mode))
;; Use composable with C-SPC

;; (use-package automark
;;   :diminish
;;   :preface
;;   (my/load-path "~/gits/emacs_clones/automark-mode/")
;;   :config
;;   (automark-mode 1))

(use-package slime :defer t
  :init
  (setq-default inferior-lisp-program "sbcl"
		slime-contribs '(slime-fancy)))

;; Navegacion por objetos... no lo he probado
(use-package objed
  :commands objed-mode)

;; Emacs en less y otros comandos
(use-package e2ansi :defer t)

;; Colaborative edditing
(use-package crdt :defer t)

(use-package i3wm-config-mode
  :mode "/i3/config\\'")

(use-package mutt-mode
  :mode "muttrc")

(use-package nftables-mode
  :mode "nftables.conf")

(use-package fancy-compilation
  :after compile
  :init
  (setq-default fancy-compilation-quiet-prelude nil
		fancy-compilation-quiet-prolog nil)
  :config
  (fancy-compilation-mode))

(use-package compile-multi :defer t)

(use-package scopeline :defer t)

(use-package debbugs :defer t)

(use-package goto-chg
  :defer t
  :init
  (keymap-global-set "C-x r a" #'goto-last-change))

(use-package citre :defer t
  :commands (citre-update-tags-file
	     citre-update-this-tags-file
	     citre-edit-tags-file-recipe
	     citre-create-tags-file
	     citre-global-create-database
	     citre-global-update-database)
  :init
  (with-eval-after-load 'cc-mode
    (require 'citre-lang-c))
  (with-eval-after-load 'dired
    (require 'citre-lang-fileref)))

;;__________________________________________________________
;; Enable tree-sitter for some modes by default if the tree-sitter
;; directory exists

(when (file-exists-p (expand-file-name "tree-sitter" user-emacs-directory))

  (setq-default toml-ts-mode-indent-offset 4
		cmake-ts-mode-indent-offset 4
		json-ts-mode-indent-offset 4
		rust-ts-mode-indent-offset 4
		go-ts-mode-indent-offset 4
		treesit-font-lock-level 4)
  (defvaralias 'c-ts-mode-indent-offset 'tab-width)

  (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

  (add-to-list 'auto-mode-alist
               '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.\\(ba\\)?sh\\'" . bash-ts-mode))

  (add-to-list 'auto-mode-alist
               '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
                 . dockerfile-ts-mode))

  (setq-default c-ts-mode-indent-style 'linux)

  (add-hook 'c++-ts-mode-hook (lambda ()
				(setq-local tab-width 4)))

  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode))
  )

;;__________________________________________________________

(provide 'init)

;;; init.el ends here
