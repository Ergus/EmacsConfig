 
;;; Commentary:
;;-*-Emacs-Lisp-*-
;; .emacs

;;; Code:
;;    General settings
;;________________________________________________________________

(auto-compression-mode 1)                   ;; If we read a compressed file, uncompress it on the fly:
(setq tab-width 4)                          ;; Ajustar tabulador a 4


;;(setq-default tab-always-indent t)        ;; make tab key always call a indent command.
;;(setq-default tab-always-indent nil) ;; make tab key call indent command or insert tab character, depend on cursor position
;(setq-default tab-always-indent 'complete) ;; make tab key do indent first then completion.

(setq make-backup-files nil)                ;; Sin copias de seguridad (torvalds-mode)
;;(desktop-save-mode 1)
(electric-indent-mode 1)                    ;; Corrige indentacion con tab de acuerdo al modo y al poner enter
(setq-default visible-bell t)               ;; Flash the screen on error; don't beep.

;;    More Misc
;;________________________________________________________________

(setq-default transient-mark-mode t)        ;; Highlight the marked region.

(setq-default line-number-mode 't)          ;; Permanent display of line numbers
(setq-default column-number-mode 't)        ;; Permanent display of column numbers

(global-linum-mode 1)                       ;; Muestra numero de linea a la izquierda
(setq-default linum-format "%4d\u2502")     ;; Formato en que se imprime la linea

;;(set-face-attribute 'linum nil :background "brightblack" :foreground "green")  ;; resalta la linea actual
;;(require 'hlinum)                         ;;resalta el color de la linea actual

;;    Font lock
;;________________________________________________________________
(global-font-lock-mode t)                   ;; Use font-lock everywhere. (resaltado en todos lod modos)

(setq font-lock-maximum-decoration t)       ;; We have CPU to spare; highlight all syntax categories.

(blink-cursor-mode 0)                       ;; Parpadeo del cursor
(set-mouse-color "white")                   ;; Flechita del mouse que se vea
(set-cursor-color "white")                  ;; Set cursor and mouse colours:

(setq scroll-step 1)                        ;; Ordinarily emacs jumps by half a page when scrolling -- reduce:

;; And finally, the most useful addition to .emacs: the ability to
;; scroll from the keyboard (what is everyone else using!?)
(global-set-key "\M-U" 'up-semi-slow)
(global-set-key "\M-D" 'down-semi-slow)

;; Seleccionar con el mouse
;;________________________________________________________________
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

(mouse-wheel-mode t)                        ;; scrolling con el mouse

;; EXTRAS VARIOS
;;________________________________________________________________
(require 'speedbar)
(setq speedbar-show-unknown-files t)         ;; speedy bar show unknown files
(global-set-key [(f12)] 'speedbar-get-focus) ;; F12 para mostrar/ocultar

(show-paren-mode t)                          ;; Highlight couple parentesis

(electric-pair-mode t)                       ;; Autoannadir parentesis y llaves de cierre

;; Autoañadir paréntesis o llaves de cierre (comandos antiguos)
;;(setq skeleton-pair t)                     ;; do not insert newline after skeleton insertation
;;(global-set-key "[" 'skeleton-pair-insert-maybe) ;; do this for (,{,',\,<

;;    Files and directories
;;________________________________________________________________

;; dired-x is a nice substitute for Windows Explorer and OSX's Finder.
;; M-o: avoid seeing all the backup files.
;; C-x C-j: enter dired/dired-x mode.
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x")))
	  )


;; Because Explorer and Finder have a mapping from file type to application,
;; we need to tell emacs- what to do with each file type.

(setq-default dired-guess-shell-alist-user
       (list
       (list "\\.ipe$"    "ipe ")
       (list "\\.xml$"    "ipe ")
       (list "\\.ps$"     "evince")
       (list "\\.ps.gz$"  "evince")
       (list "\\.eps$"    "evince")
       (list "\\.eps.gz$" "evince")
       (list "\\.pdf$"    "evince")
       (list "\\.PDF$"    "evince")
       (list "\\.\\(rgb\|tiff\|tif\|xbm\|gif\|pgm\|ppm\|bmp\|tga\\)$"  "eog ")
       (list "\\.ppm$" "eog")
       (list "\\.gif$" "eog")
       (list "\\.png$" "eog")
       (list "\\.jpg$" "eog")
       (list "\\.JPG$" "eog")
       (list "\\.avi$" "mplayer")
       (list "\\.sc$" "showcase")
       (list "\\.wav$" "mplayer")
       (list "\\.flv$" "mplayer")
       (list "\\.mov$" "mplayer")
       (list "\\.3gp$" "mplayer")
       (list "\\.drawtool$" "drawtool ")
       ))

;;    Define a few colours that look good on reverse video:
;;________________________________________________________________

(set-background-color "black")
(set-foreground-color "white")

(set-face-foreground 'bold "LightGoldenrod")
(set-face-foreground 'bold-italic "grey20")
(set-face-foreground 'italic "yellow3")
(set-face-foreground 'font-lock-preprocessor-face "DarkMagenta")
(set-face-foreground 'font-lock-comment-face "cyan1")
(set-face-foreground 'font-lock-string-face "red4")	;;strings en el programa
(set-face-foreground 'font-lock-doc-face "LightSalmon2")
(set-face-foreground 'font-lock-function-name-face "white") ;;funciones nombres
(set-face-foreground 'font-lock-variable-name-face "white") ;;variables nombres
(set-face-foreground 'font-lock-type-face "green")
(set-face-foreground 'font-lock-keyword-face "DarkGoldenrod")
(set-face-attribute 'highlight nil :foreground "red")

(set-face-attribute 'secondary-selection nil :background "darkblue" :foreground "skyblue")

;; search C-s, resalta lo que encuentra
(set-face-attribute 'isearch nil :background "blue" :foreground "white")
;; selection C-space resalta lo que se marca
(set-face-attribute 'region nil :background "white" :foreground "black")

;;    class-name-faceSettings for compilation
;;________________________________________________________________

(global-set-key "\M-C" 'compile)  ;; We set a key-binding for this often-used command "compile"

;; The following two commands load the source file automatically and point
;; to the error/warning line. This make emacs essentially a (mouse-free) IDE:
(global-set-key [f11] 'previous-error)
(global-set-key [f12] 'next-error)


;;    Settings for compilation scrolling
;;________________________________________________________________
(setq-default compilation-scroll-output t)   ;; scroll the *compilation* buffer window as output appears
(setq-default compile-auto-highlight t)      ;; (setq compilation-window-height 20)

;;________________________________________________________________
;;    Settings for modes
;;________________________________________________________________

;;________________________________
;;    c++-mode

;; Integracion con cmake
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode))
  )

(defun my/c-format ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0) ; The brace that opens a substatement block
  (c-set-offset 'brace-list-open 0)   ; Open brace of an enum or static array list

  (c-set-offset 'namespace-open 0)
  (c-set-offset 'innamespace '+)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'brace-list-open 0)

  (c-set-offset 'inclass '++)
  (c-set-offset 'access-label '-)
  (c-set-offset 'topmost-intro '0)

  (c-set-offset 'cpp-macro '0)

  (c-set-offset 'namespace-close '+)
  (c-set-offset 'defun-close '+)
  (c-set-offset 'block-close '+)
  (c-set-offset 'class-close '+)
  (c-set-offset 'inline-close '+)
  (c-set-offset 'brace-list-close '+)

  (c-set-offset 'inline-open '+)
  (c-set-offset 'case-label '+)
  )

(defun my/c-mode-hook ()
  (my/c-format)
  
  (irony-mode)
  
  (maybe-cmake-project-hook)
  )
(add-hook 'c-mode-common-hook 'my/c-mode-hook)
(add-hook 'c++-mode-common-hook 'my/c-mode-hook)

;; Even if the file extension is just .c or .h, assume it is a C++ file:
(setq auto-mode-alist (cons '("\\.c\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.fx\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cc\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cpp\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cxx\\'" . c++-mode) auto-mode-alist))

;; Treat vertex and fragment shaders as C programs
(setq auto-mode-alist (cons '("\\.fsh\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vsh\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vert\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.frag\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vert.txt\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.frag.txt\\'" . c-mode) auto-mode-alist))

;;________________________________
;; Markdown
(setq auto-mode-alist (cons '("\\.md\\'" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode 'turn-on-flyspell)

;;________________________________
;; Makefile
(setq auto-mode-alist (cons '(".*Makefile.*" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mak" . makefile-mode) auto-mode-alist))

;;________________________________
;; HTML
(setq auto-mode-alist (cons '("\\.rhtml\\'" . html-mode) auto-mode-alist)) ;; Treat .rhtml files as HTML

;;________________________________
;; ruby-mode
(setq-default ruby-indent-level 2)
;; Treat .rjs files as Ruby
(setq auto-mode-alist (cons '("\\.rjs\\'" . ruby-mode) auto-mode-alist))
;; Rakefiles are Ruby files:
(setq auto-mode-alist (cons '("\\Rakefile\\'" . ruby-mode) auto-mode-alist))
;; So is Gemfile:
(setq auto-mode-alist (cons '("\\Gemfile\\'" . ruby-mode) auto-mode-alist))
;; Treat .vssettings files (Visual Studio) as XML
(setq auto-mode-alist (cons '("\\.vssettings\\'" . xml-mode) auto-mode-alist))

;;________________________________
;; Julia Mode
(setq auto-mode-alist (cons '("\\.jl\\'" . julia-mode) auto-mode-alist))

;;________________________________
;; Rust Mode
(setq auto-mode-alist (cons '("\\.rs\\'" . rust-mode) auto-mode-alist))

;;________________________________
;; D languaje
(add-to-list 'auto-mode-alist '("\\.d\\'" . D-mode))

;;________________________________
;; Go languaje
(defun my/go-mode-hook ()
  (require 'go-mode-autoloads)
  (setq tab-width 2)
  (setq indent-tabs-mode 1)
  ('my/c-format)
  )
(add-hook 'go-mode-hook 'my/go-mode-hook)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;;________________________________
;; lua language
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'hs-minor-mode)  ;; If you want to use hideshow, turn on hs-minor-mode or add this:

;;________________________________
;; OpenCL Mode
;; (require 'opencl-mode)
;; (add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))

;;________________________________
;;    DOS batch files
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;;________________________________
;;    qt-pro-mode
;; Use for Qt's .pro and .pri files
(progn (require 'qt-pro "~/.emacs.d/plugins/qt-pro.el")
       (add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))
       )
(setq auto-mode-alist (cons '("\\.moc\\'" . c++-mode) auto-mode-alist)) ;; Treat .moc files (Qt) as C++
(setq auto-mode-alist (cons '("\\.ui\\'" . xml-mode) auto-mode-alist))  ;; Treat .ui files (Qt) as XML

;;________________________________
;;    javascript-mode
(setq auto-mode-alist (cons '("\\.js\\'" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.java\\'" . java-mode) auto-mode-alist))

;; no good:
;;(autoload 'javascript-mode "javascript-mode")
;;(setq auto-mode-alist
;;      (cons '("\\.js\\'" . javascript-mode) auto-mode-alist))

;; ________________________________
;;    xml-mode
(setq auto-mode-alist (cons '("\\.ipe\\'" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.qrc\\'" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.svg\\'" . xml-mode) auto-mode-alist))

;;________________________________________________________________
;;    Dired-mode settings

;; A few customizations:
;; Among them: make copy and delete in dired recursive.

 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top))
 '(ecb-options-version "2.40")
 '(large-file-warning-threshold 100000000)
 '(mumamo-submode-indent-offset 4)
 '(package-selected-packages
   (quote
    (use-package systemd ssh-tunnels ssh-config-mode ssh sr-speedbar sphinx-mode sphinx-frontend skewer-mode shell-pop ruby-tools ruby-electric ruby-additional python-mode pyenv-mode peg org2jekyll org-wc org-table-comment org-sync org-ref org-protocol-jekyll org-octopress org-journal org-jekyll org-gnome notmuch nasm-mode markdown-mode+ magit langtool jekyll-modes hlinum highlight helm-systemd helm-git-grep helm-git-files helm-git helm-flycheck helm-firefox helm-emms helm-company helm-bibtexkey helm-R go-snippets go-mode gnuplot-mode gnuplot gmail2bbdb gmail-message-mode function-args fsm fortune-cookie flyspell-correct-helm flycheck-ycmd flycheck-rust flycheck-package flycheck-irony flycheck-haskell flycheck-d-unittest flycheck-cython flycheck-cstyle flycheck-color-mode-line flycheck-clangcheck flycheck-clang-tidy f90-interface-browser elpa-mirror eink-theme ein-mumamo eclipse-theme ecb docker-tramp d-mode cssh connection company-ycmd company-ycm company-statistics company-quickhelp company-math company-lua company-jedi company-irony-c-headers company-irony company-emoji company-cmake company-c-headers company-bibtex company-auctex company-anaconda color-theme-zenburn color-theme-x color-theme-vim-insert-mode color-theme-tango color-theme-modern cmake-project cmake-font-lock cargo bbdb-vcard bbdb-handy bbdb-ext bbdb- awk-it auto-yasnippet auto-virtualenv auto-dictionary alect-themes)))
 '(same-window-buffer-names
   (quote
    ("*eshell*" "*Python*" "*shell*" "*Buffer List*" "*scheme*" "*")))
 '(show-paren-mode t))

;;_____________________________________
;;    Completion

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

(setq split-width-threshold 110)             ;; Original value 240 ancho minimo limite para split vertical
;;(split-window-right)                       ;; Splits vertically on startup

;; Move split keybindings
(windmove-default-keybindings 'meta)         ;; Use Shift+arrow_keys to move cursor around split panes
(setq-default windmove-wrap-around t )       ;; when cursor is on edge, move to the other side, as in a toroidal space

(when (fboundp 'winner-mode) (winner-mode 1));; recuperar Split configuration con C-c left/right

;;________________________________________________________________
;;    Do not publish my email on Usenet
(setq user-full-name       "Jimmy Aguilar Mena")

;;________________________________________________________________
;;    Don't display initial logo
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;;________________________________________________________________
;;    Mark user-written files (for subsequent searching)
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map ";" 'mark-cpp-tex-files)
             ))

;;________________________________________________________________
;; Confirmation for to exit emacs
(fset 'yes-or-no-p 'y-or-n-p)         ;; Reemplazar "yes" por "y" en el prompt
(setq confirm-kill-emacs 'y-or-n-p)   ;; Puede ser 'nil o 'y-or-n-p

;;_____________________________________
;; Indent html by 4
(setq-default sgml-basic-offset 4)

;;    Confirm css is an adequate mode for qml files.
(setq auto-mode-alist (cons '("\\.qml\\'" . css-mode) auto-mode-alist))

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
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.gpl$" . gnuplot-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.plt$" . gnuplot-mode)) auto-mode-alist))

;;______________________________________
;; For using Melpa and elpa

(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") )
(package-initialize) ;; You might already have this line


;;______________________________________
;; Auto completamiento

(require 'yasnippet)
(yas-global-mode 1)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Colores de la ventana de company
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

(setq company-idle-delay 0)            ;; company complete inmediatamente
(setq company-minimum-prefix-length 2) ;; minumo de letras para empezar a completar

(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my/irony-mode-hook ()
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends '(company-irony company-irony-c-headers))
  
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  )
(add-hook 'irony-mode-hook 'my/irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;(require 'ycmd)
;;(add-hook 'after-init-hook #'global-ycmd-mode)
;;(set-variable 'ycmd-server-command '("python" ""))

;; Add yasnippet support for all company backends
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

;;______________________________________
;; Chequeo de syntaxis
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  )
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;______________________________________
;; Chequeo de gramatica
(require 'langtool)
(setq langtool-language-tool-jar "/home/ergo/.emacs.d/LanguageTool-3.1/languagetool-commandline.jar")

;;______________________________________
;; EMMS mode
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq-default emms-source-file-default-directory "~/almacen/Musica/")

;;______________________________________
;; Email mode for mutt
(defun my/mail-mode-hook ()
  (auto-fill-mode 1)
  (abbrev-mode 1)
  (mail-abbrevs-setup 1)
  )
(add-hook 'mail-mode-hook 'my/mail-mode-hook)
;;(add-hook 'mail-mode-hook 'mail-abbrevs-setup)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; Asocia buffers que empiecen con messaje mode
(defun my/message-mode-hook ()
  (auto-fill-mode 1)
  (turn-on-flyspell 1)
  (abbrev-mode 1)
  (mail-abbrevs-setup 1)
  )
(add-hook 'message-mode-hook 'my/message-mode-hook)
(add-to-list 'auto-mode-alist '("mutt-Ergus-*" . message-mode))

;; Autocompleta direcciones
(require 'notmuch-address)
(setq notmuch-address-command "/home/ergo/almacen/mail/notmuch-addrlookup-c/notmuch-addrlookup")
(notmuch-address-message-insinuate)

;;______________________________________
(hlinum-activate)                                 ;; Highlight linenum
(setq-default linum-highlight-in-all-buffersp t)  ;; Keep highlighted linenum in all buffers

;;______________________________________
;; Latex mode

(load "auctex.el" nil t t)

(require 'company-auctex)
(defun my/LaTeX-mode-hook ()
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq LaTeX-item-indent 0)

  (add-to-list 'TeX-output-view-style
	       '("^pdf$" "." "evince %o %(outpage)"))
  
  (setq-local company-backends
	      (append '(company-math-symbols-latex
			company-latex-commands
			company-math-symbols-unicode)
		      company-backends)
	      )
  (company-auctex-init)
  (turn-on-auto-fill)
  (turn-on-flyspell)
  )

(add-hook 'Tex-mode-hook #'my/LaTeX-mode-hook)

(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq LaTeX-eqnarray-label "eq"
      LaTeX-equation-label "eq"
      LaTeX-figure-label "fig"
      LaTeX-table-label "tab"
      LaTeX-myChapter-label "chap"
      TeX-auto-save t
      TeX-newline-function 'reindent-then-newline-and-indent
      TeX-parse-self t
      LaTeX-section-hook
      '(LaTeX-section-heading
	LaTeX-section-title
	LaTeX-section-toc
	LaTeX-section-section
	LaTeX-section-label)
      )

;;bibtex mode set use biblatex
(defun my/bibtex-mode-hook () (bibtex-set-dialect 'biblatex) )
(add-hook 'bibtex-mode-hook #'my/bibtex-mode-hook)

;;______________________________________
;; Python mode
(defun my/python-mode-hook ()
  (anaconda-mode)
  (add-to-list 'company-backends '(company-jedi company-anaconda))
  )
(add-hook 'python-mode-hook 'my/python-mode-hook)

;;______________________________________
;; IDO siempre
(require 'ido)
(ido-mode t)

;;______________________________________
;; Ensamblador
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode))

;;______________________________________
;; CMake
(require 'cmake-mode)
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)

(defun my/cmake-mode-hook ()
  (add-to-list 'company-backends 'company-cmake)
  (cmake-font-lock-activate)
  )
(add-hook 'cmake-mode-hook 'my/cmake-mode-hook)

;;______________________________________
;; Restructured text
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))
(add-hook 'rst-mode-hook #'turn-on-flyspell)

;;; .emacs ends here
