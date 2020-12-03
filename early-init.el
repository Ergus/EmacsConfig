;;; early-init.el --- Emacs 27 early init file
;; Copyright (C) 2019 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq-default file-name-handler-alist nil
	      message-log-max 16384
	      gc-cons-threshold most-positive-fixnum   ;; Defer Garbage collection
	      gc-cons-percentage 1.0)

(add-hook 'window-setup-hook
          (lambda ()
            (setq-default file-name-handler-alist file-name-handler-alist-old
			  gc-cons-threshold 800000
			  gc-cons-percentage 0.1)
	    ;; (garbage-collect)
	    (let ((curtime (current-time)))

	      (message "Times: init:%.06f total:%.06f gc-done:%d"
		       (float-time (time-subtract after-init-time before-init-time))
		       (float-time (time-subtract curtime before-init-time))
		       gcs-done)
	      ))
	  t)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode   -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(menu-bar-mode   -1)
(tooltip-mode    -1)			;; Tool tip in the echo
(electric-indent-mode -1)
;;(flymake-mode    -1)

;;(setq-default package-enable-at-startup nil)
;;-------------------- Some tricks--------------------------

(provide 'early-init)
;;; early-init.el ends here
