(provide 'tycho-init)

(defun tychoish-init-work ()
  (interactive)
  (setq bookmark-default-file "~/emacs/bookmarks/arendt-work-bookmarks"
        w3m-session-file "~/.emacs.d/arendt-work-w3m-session"
        bookmark-save-flag 1)
  (setq desktop-base-file-name "arendt-work-desktop")
  (setq abbrev-file-name "~/.emacs.d/work-abbrev-defs")
  (setq helm-c-adaptive-history-file "~/.emacs.d/arendt-work-helm-c-adaptive-history")
  (defalias 'todo 'work-todo)
  (defalias 'flow 'work-flow)
  (require 'work-deft)
  (message "loaded emacs-work"))

(defun tychoish-init-default ()
  (interactive)
  (setq bookmark-default-file "~/emacs/bookmarks/arendt-tychoish"
        w3m-session-file "~/.emacs.d/arendt-tychoish-w3m-session"
        bookmark-save-flag 1)
  (setq desktop-base-file-name "arendt-tychoish-desktop")
  (setq abbrev-file-name "~/.emacs.d/tychoish-abbrev-defs")
  (setq helm-c-adaptive-history-file "~/.emacs.d/arendt-tychoish-helm-c-adaptive-history")
  (defalias 'todo 'tychoish-todo)
  (defalias 'flow 'tychoish-flow)
  (require 'edit-server)
  (edit-server-start)
  (require 'tycho-deft)
  (message "loaded emacs-tychoish"))

(defun tychoish-init-generic ()
  (interactive)
      (setq bookmark-default-file "~/emacs/bookmarks/arendt-generic-tychoisxoh"
        w3m-session-file "~/emacs.d/arendt-tychoish-generic-w3m-session"
        bookmark-save-flag 1)
  (setq desktop-base-file-name "arendt-generic-desktop")
  (setq helm-c-adaptive-history-file "~/.emacs.d/arendt-generic-helm-c-adaptive-history")
  (defalias 'todo 'tychoish-todo)
  (defalias 'flow 'tychoish-flow)
  (require 'tycho-deft)
  (message "loaded emacs-generic"))

(cond
 ((equal (pwd) "Directory ~/")
  (tychoish-init-default))
 ((equal (pwd) "Directory ~/work/")
  (tychoish-init-work))
 ((equal (pwd) "Directory ~/")
  (tychoish-init-default))
 ((equal (pwd) "Directory ~/work/")
  (tychoish-init-work))
 ((equal (pwd) "Directory ~/emacs/")
  (tychoish-init-generic))
 ((equal (boundp 'server-name) nil)
  (tychoish-init-generic))
 ((equal server-name "tychoish")
  (tychoish-init-default))
 ((equal server-name "work")
  (tychoish-init-work)))
