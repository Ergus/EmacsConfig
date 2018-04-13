;;; init.el --- Emacs Initialization and Configuration
;; Copyright (C) 2018 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;; Code:

;; For using Melpa and Elpa
;;______________________________________
(require 'package)      ;; You might already have this line
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         )
      )
(package-initialize)                          ;; You might already have this line
(add-to-list 'load-path "~/.emacs.d/lisp/")

(eval-when-compile
  (require 'use-package)
  (setq-default use-package-verbose t)
  )

(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(require 'mocp)
;;________________________________________________________________
;; Dired-mode settings

;; A few customizations:
;; Among them: make copy and delete in dired recursive.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
 (quote
  ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top))
 '(ecb-options-version "2.50")
 '(large-file-warning-threshold 100000000)
 '(mumamo-submode-indent-offset 4)
 '(org-agenda-files (quote ("~/file.org")))
 '(package-selected-packages
 (quote
  (diminish flyspell-correct-ivy helm-c-yasnippet helm-smex helm-tramp helm-cscope xcscope counsel-etags counsel-gtags ggtags helm-gtags magit bbdb- counsel-bbdb highlight-blocks counsel-notmuch counsel-tramp highlight-indent-guides highlight-parentheses smex counsel ivy multi-term bongo flycheck-ycmd company-ycmd ycmd modern-cpp-font-lock anzu smart-mode-line clean-aindent-mode multiple-cursors d-mode jabber exwm benchmark-init tabbar cobol-mode shell-pop smart-tabs-mode elscreen yasnippet yaxception flycheck-clang-analyzer flycheck-julia langtool company-go auctex company-auctex sphinx-mode jedi qt-pro-mode opencl-mode flyspell-popup alert anaconda-mode async bbdb bind-key cl-generic cmake-mode company concurrent emms flycheck jedi-core js2-mode let-alist math-symbol-lists polymode popup with-editor sunrise-x-buttons sunrise-commander sr-speedbar ruby-tools ruby-electric nasm-mode markdown-mode+ hlinum highlight go-snippets go-mode gnuplot-mode flycheck-rust flycheck-irony flycheck-cstyle f90-interface-browser elpa-mirror ecb company-statistics company-quickhelp company-math company-lua company-jedi company-irony-c-headers company-irony company-c-headers company-bibtex company-anaconda cmake-project bbdb-vcard bbdb-handy)))
 '(same-window-buffer-names
 (quote
  ("*eshell*" "*Python*" "*shell*" "*Buffer List*" "*scheme*" "*")))
 '(show-paren-mode t))

;;    General settings
;;____________________________________________________________
(require 'bind-key)
(setq-default show-trailing-whitespace t ;;
			  tab-width 4                ;; Tabulador a 4
			  make-backup-files nil      ;; Sin copias de seguridad (torvalds-mode)
			  visible-bell t             ;; Flash the screen on error.
			  scroll-step 1)             ;; Scroll one by one

(savehist-mode t)              	 ;; Historial
(auto-compression-mode t)      	 ;; Uncompress on the fly:
(show-paren-mode t)            	 ;; Highlight couple parentesis
(electric-pair-mode t)         	 ;; Autoannadir parentesis
(auto-revert-mode t)             ;; Autoload files changed in disk

;;____________________________________________________________
;; Status bar (mode line in emacs)
(sml/setup)
(setq sml/theme 'light)

;;____________________________________________________________
;;  Font lock
(global-font-lock-mode t)      ;; Use font-lock everywhere.
(setq font-lock-maximum-decoration t)


;;____________________________________________________________
;; Miscelania
;;(setq-default tab-always-indent t)            ;; make tab key always call a indent command.
;;(setq-default tab-always-indent nil)          ;; make tab key call indent command or insert tab character
;;(setq-default tab-always-indent 'complete)    ;; make tab key do indent first then completion.

;;(desktop-save-mode 1)
;;(electric-indent-mode -1)                    ;; Corrige indentacion con tab o enter (now default)

(use-package clean-aindent-mode :ensure t       ;; Elimina el indentado extra si no se ha llenado
  :bind("RET" . newline-and-indent)
  :init
  (electric-indent-mode -1)  ; no electric indent, auto-indent is sufficient
  (clean-aindent-mode t)
  (setq clean-aindent-is-simple-indent t))

;;____________________________________________________________
;; Menu bar
(global-set-key (kbd "M-f") 'menu-bar-open)

;;____________________________________________________________
;; Clipboard
(defun my/xclipboard () "Define my clipboard functions with xsel."
       (defun xcopy () "Copies selection to x-clipboard."
              (interactive)
              (if (display-graphic-p)
                  (progn
                    (message "Yanked region to x-clipboard!")
                    (call-interactively 'clipboard-kill-ring-save)
                    )
                (if (region-active-p)
                    (progn
                      (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
                      (message "Yanked region to clipboard!")
                      (deactivate-mark))
                  (message "No region active; can't yank to clipboard!")))
              )

       (defun xpaste () "Pastes from x-clipboard."
              (interactive)
              (if (display-graphic-p)
                  (progn
                    (clipboard-yank)
                    (message "graphics active")
                    )
                (insert (shell-command-to-string "xsel -o -b"))
                )
              )

       (global-set-key (kbd "C-c c") 'xcopy)
       (global-set-key (kbd "C-c v") 'xpaste)
       )

(my/xclipboard)
(delete-selection-mode)  ;; Sobreescribe seleccion al pegar

;;  More Misc
;;____________________________________________________________
(setq-default vc-follow-symlinks nil;; Open links not open
	      transient-mark-mode t ;; Highlight marked region
	      line-number-mode t    ;; Display line numbers
	      column-number-mode t) ;; Display column numbers

(global-linum-mode 1)  ;; Numero de linea a la izquierda
(setq-default linum-format "%4d\u2502") ;; Formato linea
;;(setq-default linum-format "%4d|")      ;; Formato linea

;;hlinum
;;____________________________________________________________
(use-package hlinum :ensure t ;; resalta el numero de la linea
  :config
  (hlinum-activate)             ;; Highlight linenum
  ;; Keep highlighted linenum in all buffers
  ;;(setq-default linum-highlight-in-all-buffersp t)
  )

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  ;;(indent-according-to-mode)
  )

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  ;;(indent-according-to-mode)
  )

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)

;;____________________________________________________________
;;  Seleccionar con el mouse
(require 'mouse)
(xterm-mouse-mode t)         ;; mover el cursor al click
(defun track-mouse (e))
(setq-default mouse-sel-mode t) ;; Mouse selection

(mouse-wheel-mode t)         ;; scrolling con el mouse

(blink-cursor-mode 0)        ;; Parpadeo del cursor modo texto
(set-mouse-color "white")    ;; Flechita del mouse que se vea
(set-cursor-color "white")   ;; Set cursor and mouse colours

;; Multiple Cursors
;;____________________________________________________________
(global-unset-key (kbd "C-c <down-mouse-1>"))
(use-package multiple-cursors  :ensure t ;; Multiple cursors package
  :bind (("C-c m" . mc/edit-lines)
		 ("C-c n" . mc/mark-next-like-this)
		 ("C-c p" . mc/mark-previous-like-this)
         ("C-c <mouse-1>" . mc/add-cursor-on-click)))

;;  EXTRAS VARIOS
;;____________________________________________________________
(require 'sr-speedbar)           ;; Panel lateral
(global-set-key (kbd "M-s") 'sr-speedbar-toggle)
(setq sr-speedbar-right-side nil)

;;  Files and directories
;;____________________________________________________________
;; M-o: avoid seeing all the backup files.
;; C-x C-j: enter dired/dired-x mode.
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))

;; Because Explorer and Finder have a mapping from file type to application,
;; we need to tell emacs- what to do with each file type.
(setq-default dired-guess-shell-alist-user
              (list
               (list "\\.\\(ps\|ps.gz\|eps\|eps.gz\|pdf\|PDF\\)$" "evince")
               (list "\\.\\(rgb\|tiff\|tif\|xbm\|gif\|pgm\|ppm\|bmp\|tga\\)$" "eog ")
               (list "\\.\\(ppm\|gif\|png\|jpg\|JPG\\)$" "eog")
               (list "\\.\\(avi\|wav\|flv\|mov\|3gp\\)$" "vlc")
               ))

;;____________________________________________________________
;;  The Colors

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
	   ;; color de la linea en el panel activo
	   (set-face-attribute  'mode-line nil :foreground "black" :background "gray90":box '(:line-width 1 :style released-button))
	   (set-face-attribute  'mode-line-inactive nil :foreground "white" :background "gray10" :box '(:line-width 1 :style released-button))
       )

(my/colors)

;;____________________________________________________________
;; Lineas de Indentado
(use-package highlight-indent-guides :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "gray20")
  )

;;____________________________________________________________
;; Resalta parentesis entorno al cursor
(use-package highlight-parentheses :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (custom-set-faces
   '(hl-paren-face ((t (:weight ultra-bold))) t))
  (setq hl-paren-colors
		(quote
		 ("brightgreen" "IndianRed1" "IndianRed3" "IndianRed4"))))

;;____________________________________________________________
;; Resalta scopes entorno al cursor
(use-package highlight-blocks :ensure t
  :config
  (define-key function-key-map "\e[1;5R" [C-f3])
  (global-set-key (kbd "C-<f3>") 'highlight-blocks-now)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(highlight-blocks-depth-2-face ((t (:background "gray15"))))
   '(highlight-blocks-depth-3-face ((t (:background "gray20"))))
   '(highlight-blocks-depth-4-face ((t (:background "gray25"))))
   '(highlight-blocks-depth-5-face ((t (:background "gray30"))))
   '(highlight-blocks-depth-6-face ((t (:background "gray35"))))
   '(highlight-blocks-depth-7-face ((t (:background "gray40"))))
   '(highlight-blocks-depth-8-face ((t (:background "gray45"))))
   '(highlight-blocks-depth-9-face ((t (:background "gray50"))))
   )
  )

;;____________________________________________________________
;; Resaltar parentesis a pares permanentemente... no me gusto
;;(use-package rainbow-delimiters :ensure t
;;  :config
;;  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;  (custom-set-faces
;;   '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "blue"))))
;;   '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "magenta"))))
;;   '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightgreen"))))
;;   '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightyellow"))))
;;   '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightcyan"))))
;;   '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightmagenta"))))
;;   '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
;;   '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow"))))
;;   '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan"))))))

;;____________________________________________________________
;; Flyspell

(use-package flyspell :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (use-package flyspell-popup :ensure t
	:bind ("C-; . flyspell-popup-correct")
	)
  (use-package flyspell-correct-ivy
	:commands (flyspell-correct-ivy)
	:bind (:map flyspell-mode-map
				("C-;" . 'flyspell-correct-previous-word-generic))
	:init
	(setq flyspell-correct-interface #'flyspell-correct-ivy))

  )

;;________________________________
;; {c/c++}-mode
;;________________________________

;;_______________________________________
;; Mark lined linger 80

(use-package column-enforce-mode :ensure t
  :hook prog-mode
  :config
  (column-enforce-mode t)
  (setq column-enforce-comments nil)
  ;;(setq column-enforce-column <your desired column>)
  (custom-set-faces '(column-enforce-face
					  ((t (:background "gray90" :foreground "black")))))
  )

;;_______________________________________
;; Indent with tabs align with spaces
(use-package smart-tabs-mode :ensure t
  :config
  (smart-tabs-insinuate 'c 'c++))

;;_______________________________________
;; Improve completion for cmake project
;;(use-package cmake-ide :ensure t
;;  :config
;;  (add-hook 'c-mode-common-hook #'cmake-ide-setup))

;;_______________________________________
;; Cscope for c-mode
(use-package xcscope :ensure t
  :init
  (add-hook 'c-mode-common-hook 'cscope-setup)
  :bind (("C-c ," . cscope-find-global-definition-no-prompting)
		 ("C-c d" . cscope-find-global-definition)
		 ("C-c f" . cscope-find-this-symbol)
		 ("C-c *" . cscope-pop-mark)))

;;_______________________________________
;; C common mode (for all c-like languajes)

(defun my/c-mode-common-hook () "My hook for C and C++."
	   (c-set-offset 'cpp-macro 0 nil)
	   (message "Loaded my/c-mode common"))

(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

;;_______________________________________
;; c++-init-hook (before all the others)

(defun my/c-init-hook ()
  "The initialization hook for 'c-mode' run before any other hooks."
  (setq c-doc-comment-style
        '((java-mode . javadoc)
          (pike-mode . autodoc)
          (c-mode    . javadoc)
          (c++-mode  . javadoc)))

  (setq c-basic-offset 4   ;; Default is 2
		c-indent-level 4       ;; Default is 2
		;;c-set-style "linux"
		indent-tabs-mode t)
  (setq-default c-default-style
				'((java-mode . "java")
				  (awk-mode . "awk")
				  (other . "linux"))))

;; This hook run before any other hook in c-mode
(add-hook 'c-initialization-hook 'my/c-init-hook)

;;_____________________________________________________
;; Agrega doble indentation a clases y simple a structs

(defun my/c++-lineup-inclass (langelem) "LANGELEM Offset struct vs class."
       (let ((inclass (assoc 'inclass c-syntactic-context)))
         (save-excursion
           (goto-char (c-langelem-pos inclass))
           (if (or (looking-at "struct")
                   (looking-at "typedef struct"))
               '+
             '++))))

;;_____________________________________________________
;; C++ mode

(defun my/c++-mode-hook () "My C++-Mode hook function."
	   (setq flycheck-gcc-language-standard "c++11")
	   (require 'modern-cpp-font-lock)
	   (modern-c++-font-lock-global-mode t)
       (c-set-offset 'inclass 'my/c++-lineup-inclass)
	   (message "Loaded my c++-mode")
       )

(add-hook 'c++-mode-hook 'my/c++-mode-hook)

;; Even if the file extension is just .c or .h, assume it is a C++ file:
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))

;;_____________________________________________________
;; Cuda
(use-package cuda-mode :ensure t
  :mode "\\.cu\\'")

;;________________________________
;; OpenCL Mode
(use-package opencl-mode :ensure t
  :mode "\\.cl\\'")

;;________________________________
;; Markdown mode
(use-package markdown-mode :ensure t
  :mode "\\.md\\'"
  :config
  (flyspell-mode 1))

;;______________________________________
;; Restructured text
(use-package rst-mode
  :mode "\\.rst\\'"
  :config
  (use-package sphinx-mode :ensure t
    :hook rst-mode)
  (flyspell-mode 1))

;;________________________________
;; Makefile
(use-package makefile-mode
  :mode (".*Makefile.*" "\\.mak"))

;;________________________________
;; ruby-mode
(use-package ruby-mode :ensure t
  :mode ("\\.rb\\'" "\\.rjs\\'" "\\Rakefile\\'" "\\Gemfile\\'")
  :config
  (require 'ruby-tools)
  (use-package ruby-electric :ensure t
    :hook ruby-electric-mode
    )
  (setq-default ruby-indent-level 2)
  )
;;________________________________
;; Julia Mode
(use-package julia-mode :ensure t
  :mode "\\.jl\\'"
  :config
  (use-package flycheck-julia :ensure t
    :commands (flycheck-julia-setup)
    :init
    (add-hook 'julia-mode-hook #'flycheck-mode)
  ))

;;________________________________
;; Rust Mode
(use-package rust-mode :ensure t
  :mode "\\.rs\\'"
  :config
  (use-package flycheck-rust :ensure t
    :commands (flycheck-rust-setup)
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    ))

;;________________________________
;; D languaje
(use-package d-mode :ensure t :mode "\\.d\\'")

;;________________________________
;; Go languaje
(use-package go-mode :ensure t
  :mode "\\.go\\'"
  :config
  (use-package company-go :ensure t
    :config
    (add-to-list 'company-backends 'company-go))

  (use-package go-snippets :ensure t)
  )

;;________________________________
;; lua language
(use-package lua-mode :ensure t
  :mode "\\.lua\\'")

;;________________________________
;; systemd mode
(use-package systemd :ensure t
  :mode ("\\.service\\'" "\\.timer\\'" "\\.target\\'"
		 "\\.mount\\'" "\\.socket\\'" "\\.slice\\'"
		 "\\.automount\\'" ))

;;________________________________
;; DOS batch files
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;;________________________________
;; qt-pro-mode
;; Use for Qt's .pro and .pri files
(require 'qt-pro-mode)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))
(add-to-list 'auto-mode-alist '("\\.moc\\'" . c++-mode)) ;; Treat .moc files (Qt) as C++
(add-to-list 'auto-mode-alist '("\\.ui\\'" . xml-mode))  ;; Treat .ui files (Qt) as XML

;;________________________________
;;    javascript-mode
(use-package js2-mode :ensure t
  :mode ("\\.js\\'")
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode))
;; ________________________________
;; xml-mode
(use-package xml-mode
  :mode ("\\.ipe\\'" "\\.qrc\\'" "\\.svn\\'"))

;;_____________________________________
;; Completion
'(completion-ignored-extensions
  (quote ("CVS/" ".o" "~" ".bin" ".lbin" ".fasl" ".ufsl" ".a" ".ln" ".blg" ".bbl"
          ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".class" ".fas"
          ".lib" ".x86f" ".sparcf" ".lo" ".la" ".toc" ".log" ".aux" ".cp" ".fn"
          ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".lbl"
          ".out" ".brf" ".ncb" ".sln" ".suo" ".vcproj.AD.ghali.user" ".idb" ".pdb"
          ".synctex.gz" ".svn")
         )
  )

;;_____________________________________
;; splitting
(setq split-width-threshold 110)        ;; Original value 240 ancho minimo limite para split vertical

;; Move split keybindings
(windmove-default-keybindings)          ;; Use Shift+arrow_keys to move cursor around split panes
(setq-default windmove-wrap-around t )  ;; when cursor is on edge, move to the other side, as in a toroidal space

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

;;________________________________________________________________
;;    Do not publish my email on Usenet
(setq user-full-name       "Jimmy Aguilar Mena")

;;________________________________________________________________
;;    Don't display initial logo
(setq inhibit-startup-message t
	  inhibit-startup-screen t)

;;________________________________________________________________
;;    Mark user-written files (for subsequent searching)
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map ";" 'mark-cpp-tex-files)
             ))

;;________________________________________________________________
;; Confirmation for to exit emacs
(defalias 'yes-or-no-p 'y-or-n-p)     ;; Reemplazar "yes" por "y" en el prompt
;;(fset 'yes-or-no-p 'y-or-n-p)       ;; Igual que el anterior
(setq confirm-kill-emacs 'y-or-n-p)   ;; Puede ser 'nil o 'y-or-n-p

;;_____________________________________
;; Selective auto-fill
(defun selective-auto-fill-disabling-hook ()
  "Check to see if we should disable autofill."
  (save-excursion
    (when (re-search-forward "DoNotAutoFillThisFile" 1000 t)
      (auto-fill-mode -1))))

(add-hook 'find-file-hooks 'selective-auto-fill-disabling-hook)

;;________________________________________
;; Lines enabling gnuplot-mode
(use-package gnuplot-mode :ensure t
  :mode ("\\.gp\\'" "\\.gpl\\'" "\\.plt\\'"))

;;______________________________________
;; Auto completamiento

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  (yas-reload-all)
  :config
  (message "Loaded yasnippet"))

(use-package company :ensure t
  :defer 10
  :diminish company-mode
  :bind (("M-RET" . company-complete))
  :preface
  ;; enable yasnippet everywhere
;;  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
;;  (defun company-mode/backend-with-yas (backend)
;;	(if (or
;;		 (not company-mode/enable-yas)
;;		 (and (listp backend) (member 'company-yasnippet backend)))
;;		backend
;;	  (append (if (consp backend) backend (list backend))
;;			  '(:with company-yasnippet))))
  :config
  (setq company-idle-delay 0              ;; no delay for autocomplete
		company-minimum-prefix-length 2
		company-tooltip-limit 20)
  (setq company-backends
		'((company-files          ; files & directory
		   company-keywords       ; keywords
		   company-capf
		   company-yasnippet
		   )
		  (company-abbrev company-dabbrev)
		  ))
  (global-company-mode t)
  )

(use-package rtags
  :config
  (defun my/rtags-common-hook () "My rtags loader."
		 (setq rtags-path "/usr/bin")
 		 (rtags-start-process-unless-running)
 		 (setq rtags-autostart-diagnostics t)
 		 (rtags-diagnostics)
 		 (setq rtags-completions-enabled t)
		 (setq rtags-display-result-backend 'ivy)
		 ;;(setq rtags-display-result-backend 'helm)

		 (define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
		 (define-key c-mode-base-map (kbd "M-/") (function rtags-find-references-at-point))
		 (rtags-enable-standard-keybindings)

 		 (use-package company-rtags
 		   :config
 		   (push 'company-rtags company-backends))

 		 (use-package flycheck-rtags
 		   :after flycheck
 		   :config
 		   (defun my/flycheck-rtags-hook () "My flycheck-rtags hook."
 				  (flycheck-select-checker 'rtags)
 				  (setq-local rtags-periodic-reparse-timeout 10)
				  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate.
				  (setq-local flycheck-check-syntax-automatically nil)
 				  (setq rtags-enable-unsaved-reparsing t)
 				  (message "Loaded flycheck-rtags"))
 		   (add-hook 'rtags-mode-hook #'my/flycheck-rtags-hook)))

  (add-hook 'c-mode-common-hook #'my/rtags-common-hook))

;;(use-package irony :ensure t
;;  :config
;;  (defun my/irony-mode-hook () "My irony mode hook."
;;		 (company-irony-setup-begin-commands)
;;
;;		 (use-package company-irony
;;		   :config
;;		   (add-to-list 'company-backends #'company-irony))
;;
;;		 (use-package company-irony-c-headers
;;		  :config
;;		  (add-to-list 'company-backends #'company-irony-c-headers))
;;
;;		 (use-package flycheck-irony :ensure t
;;		   :after flycheck
;;		   :config
;;		   (add-hook 'irony-mode-hook #'flycheck-irony-setup)
;;		   (message "Loaded flycheck-irony"))
;;
;;		 ;;(require 'company-c-headers)
;;		 ;;(add-to-list 'company-backends '(company-c-headers)))
;;
;;		 (define-key irony-mode-map [remap completion-at-point]
;;		   'irony-completion-at-point-async)
;;		 (define-key irony-mode-map [remap complete-symbol]
;;		   'irony-completion-at-point-async)
;;
;;		 (irony-cdb-autosetup-compile-options)
;;		 (message "Loaded Irony-Mode"))
;;
;;  ;;(add-hook 'c++-mode-hook 'irony-mode)
;;  (add-hook 'c-mode-hook 'my/irony-mode-hook)
;;  (add-hook 'objc-mode-hook 'my/irony-mode-hook))
;;  ;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

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

;;______________________________________
;; Chequeo de syntaxis
(use-package flycheck :ensure t
  :init (global-flycheck-mode)
  :config

;;  (require 'flycheck-cstyle)
;;  (flycheck-cstyle-setup)
;;  (flycheck-add-next-checker 'c/c++-clang '(warning . cstyle))

;;  (require 'flycheck-clang-analyzer)
;;  (flycheck-clang-analyzer-setup)

  (use-package flycheck-color-mode-line :ensure t
	:init
	(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
	)
  )

;;______________________________________
;; Chequeo de gramatica
(use-package langtool :ensure t
  :defer t
  :config
  (setq langtool-default-language "en")
  (setq langtool-language-tool-jar "/home/ergo/gits/languagetool/languagetool-standalone/target/LanguageTool-4.2-SNAPSHOT/LanguageTool-4.2-SNAPSHOT/languagetool-commandline.jar")
  ;;(setq langtool-language-tool-jar "/home/ergo/.emacs.d/LanguageTool-3.7/languagetool-commandline.jar")
  )

;;______________________________________
;; EMMS mode.
(use-package emms :ensure t
  :defer t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq-default emms-source-file-default-directory "~/almacen/Musica/") )

;;______________________________________
;; Email mode for mutt
;;_____________________________________
;; bbdb
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
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (abbrev-mode 1)
  (mail-abbrevs-setup)
  )

;; Autocompleta direcciones
(use-package notmuch :ensure t
  :defer t
  :config
  (require 'notmuch-address)
  (setq notmuch-address-command "/home/ergo/gits/notmuch-addrlookup-c/notmuch-addrlookup"))

;;______________________________________
;; Latex mode
(defun my/LaTeX-mode-hook () "My LaTeX Mode hook."
       (setq reftex-cite-format 'biblatex
			 reftex-plug-into-AUCTeX t
             LaTeX-eqnarray-label "eq"
             LaTeX-equation-label "eq"
             LaTeX-figure-label "fig"
             LaTeX-table-label "tab"
             LaTeX-myChapter-label "chap"
             TeX-PDF-mode t
             TeX-auto-save t
             TeX-newline-function 'reindent-then-newline-and-indent
             TeX-parse-self t
			 LaTeX-always-use-Biber t
			 reftex-bibliography-commands '("Bibliography")
             LaTeX-section-hook '(LaTeX-section-heading
								  LaTeX-section-title
								  LaTeX-section-toc
								  LaTeX-section-section
								  LaTeX-section-label)
             )
       ;; (setq LaTeX-item-indent 0)
       (setq-default TeX-master nil)
       (turn-on-auto-fill)
       (visual-line-mode)
       (LaTeX-math-mode)
       (flyspell-mode 1)
       (turn-on-reftex)
       (add-to-list 'TeX-output-view-style
                    '("^pdf$" "." "evince %o %(outpage)"))

	   (require 'company-math)
	   (require 'company-auctex)
	   (company-auctex-init)
       (add-to-list 'company-backends '(company-math-symbols-latex
                                        company-latex-commands))
       (flyspell-buffer)
       (message "Loaded my Latex mode")
       )

(use-package auctex :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (add-hook 'LaTeX-mode-hook 'my/LaTeX-mode-hook))

(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)

;;______________________________________
;;bibtex mode set use biblatex
(use-package bibtex-mode
  :mode "\\.bib\\'"
  :config
  (bibtex-set-dialect 'biblatex)
  (use-package company-bibtex :ensure t
	:config
	(add-to-list 'company-backends 'company-bibtex)))

;;______________________________________
;; Python mode
(use-package anaconda-mode :ensure t
  :hook python-mode
  :init (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config

  (use-package company-anaconda :ensure t
    :init (add-to-list 'company-backends 'company-anaconda))
  )

;;______________________________________
;; IDO siempre (probare un tiempo con helm/ivy)
;;(require 'ido)
;;(ido-mode t)

;;______________________________________
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

;;______________________________________
;; Ivy (probare un tiempo con helm/ivy)
(use-package ivy :ensure t
  :bind ("C-c C-r" . ivy-resume)
  :config
  (defalias 'list-buffers 'ibuffer) ; make ibuffer default
  (ivy-mode 1)

  (setq ivy-use-virtual-buffers t
		ivy-count-format "(%d/%d) "
		ivy-wrap t
		enable-recursive-minibuffers t)

  (use-package swiper :ensure t
	:bind ("C-s" . swiper))

  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel :ensure t
  :config
  (counsel-mode 1)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-x l") 'counsel-locate)

;;  (use-package counsel-gtags :ensure t
;;	:diminish counsel-gtags-mode
;;	:hook c-mode-common
;;	:config
;;	(counsel-gtags-mode 1)
;;	(add-to-list 'company-backends 'company-gtags)
;;
;;	(define-key counsel-gtags-mode-map (kbd "C-c d") 'counsel-gtags-find-definition)
;;	(define-key counsel-gtags-mode-map (kbd "C-c r") 'counsel-gtags-find-reference)
;;	(define-key counsel-gtags-mode-map (kbd "C-c s") 'counsel-gtags-find-symbol)
;;	(define-key counsel-gtags-mode-map (kbd "C-c <") 'counsel-gtags-go-backward)
;;	(define-key counsel-gtags-mode-map (kbd "C-c >") 'counsel-gtags-go-forward)
;;	)
  )

;;______________________________________
;; Magit
(use-package magit :ensure t)

;;______________________________________
;; Ensamblador
(use-package nasm-mode :ensure t
  :mode ("\\.asm\\'" "\\.s\\'"))

;;______________________________________
;; CMake
(use-package cmake-mode :ensure t
  :mode ("/CMakeLists\\.txt\\'" "\\.cmake\\'")
  :init
  :config
  (add-to-list 'company-backends 'company-cmake))

(use-package cmake-font-lock :ensure t
  :after (cmake-mode)
  :config
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate)
  (cmake-font-lock-activate)
  )
;;______________________________________
;; Cobol
(use-package cobol-mode :ensure t
  :mode ("\\.cobc\\'" "\\.cob\\'" "\\.cbl\\'" "\\.cpy\\'"))

;;______________________________________
;; ssh
(require 'tramp)
(setq tramp-default-method "ssh")
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '("/\\.ssh/config\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/known_hosts\\'" . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;;______________________________________
;; Jabber
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

;;______________________________________
;;; EXWM

(use-package exwm
  :if (string= (getenv "exwm_enable") "yes")
  :ensure nil
  :demand t
  :config

  ;;(exwm-enable)
  (require 'exwm-config)
  (exwm-config-ido)
  ;;(exwm-config-default)

  (setq exwm-workspace-show-all-buffers t ;; share exwm buffers in all workspaces
	exwm-layout-show-all-buffers t
	exwm-workspace-number 2)          ;; 0 - 3, zero based

  (exwm-input-set-key (kbd "s-&")
		      (lambda (command)
			(interactive (list (read-shell-command "$ ")))
			(start-process-shell-command command nil command)))


  (use-package exwm-randr
    :config
    ;;(setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "DP1" 2 "DP2"))
    (setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "HDMI1"))
    (add-hook 'exwm-randr-screen-change-hook
	      (lambda ()
		(start-process-shell-command
		 "xrandr" nil "xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1366x768 --pos 0x312 --rotate normal --output DP1 --off --output HDMI2 --off --output HDMI1 --mode 1920x1080 --pos 1366x0 --rotate normal --output DP2 --off"
		 ;;"xrandr" nil "xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1366x768 --pos 554x1080 --rotate normal --output DP1 --mode 1680x1050 --pos 1920x304 --rotate normal --output HDMI2 --off --output HDMI1 --off --output DP2 --mode 1920x1080 --pos 0x0 --rotate normal"
		 )))

    (exwm-randr-enable)
    )

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)  ;; exwm system tray

  (require 'exwm-cm)
  (exwm-cm-enable)


  (add-hook 'exwm-manage-finish-hook
	    (lambda ()
	      (when-let ((target (cdr (assoc exwm-class-name exwm-workspace-window-assignments))))
		(exwm-workspace-move-window target))))
  (exwm-enable)
  )

;;______________________________________
;;; org-gcal

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

  (custom-set-faces
   '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
   '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
   '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
   '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
   '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
   '(cfw:face-grid ((t :foreground "DarkGrey")))
   '(cfw:face-default-content ((t :foreground "#bfebbf")))
   '(cfw:face-periods ((t :foreground "cyan")))
   '(cfw:face-day-title ((t :background "grey10")))
   '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
   '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
   '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
   '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
   '(cfw:face-today ((t :background: "grey10" :weight bold)))
   '(cfw:face-select ((t :background "#2f2f2f")))
   '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
   '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
   '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold))))
  )

(provide 'init)
;;; init.el ends here
