;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Startup and Behavior Controls 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/emacs" load-path)) 

(setq custom-file "~/emacs/custom.el")
(add-to-list 'load-path "~/emacs/ljupdate")
(add-to-list 'load-path "~/emacs/snippet")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")

(require 'tycho-display) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Machine Specific Configuration Section
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tychoish-console)
(setq bookmark-default-file "~/emacs/bookmarks/remote"
      bookmark-save-flag 1)

(if (file-directory-p "~/emacs/backup")
    (setq backup-directory-alist '(("." . "~/emacs/backup")))
  (message "Directory does not exist: ~/emacs/backup"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Load the real init
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tycho-init)

(menu-bar-mode -1)