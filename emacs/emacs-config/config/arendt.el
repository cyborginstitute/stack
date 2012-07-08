;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Startup and Behavior Controls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq desktop-load-locked-desktop t)
(setq load-path (cons "~/emacs" load-path))
(setq custom-file "~/emacs/custom.el")

(add-to-list 'load-path "~/emacs/ljupdate")
(add-to-list 'load-path "~/emacs/snippet")
(add-to-list 'load-path "~/emacs/helm")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")

(require 'tycho-display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Machine Specific Configuration Section
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tychoish-display)

(require 'tycho-w3m)
(require 'tycho-mail)
(require 'tycho-lisp)
(require 'tycho-python)
(require 'tycho-magit)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load the real init.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tycho-emacs)
(require 'tycho-init)

(menu-bar-mode -1)

