(require 'deft)
(provide 'tycho-deft)

(setq deft-extension "mdwn")
(setq deft-directory "~/wiki/bwo/")
(setq deft-text-mode 'markdown-mode)
(setq deft-use-filename-as-title t)
; (setq deft-auto-save-interval nil)

(defun tychoish-todo ()
  (interactive)
  (find-file-read-only "~/wiki/todo.mdwn")
  (visual-line-mode 0)
  (revbufs))
(defun tychoish-flow ()
  (interactive)
  (find-file "~/wiki/flow.mdwn")
  (visual-line-mode 0))
(defun tychoish-generic-sphinx-compile ()
  (interactive)
  (compile "make SPHINXOPTS='-c ./ -N' -C ../ html"))

(defun deft-file-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun tychoish-deft-create (title)
  "Create a new rhizome post."
  (interactive "sBwO Title: ")
  (let ((draft-file (concat deft-directory
                            (deft-file-make-slug title)
                            "."
                            deft-extension)))
    (if (file-exists-p draft-file)
        (find-file draft-file)
      (find-file draft-file)
      (insert (title)))))

(global-set-key (kbd "C-c C-d") 'deft)
(global-set-key (kbd "C-c d o") 'deft)
(global-set-key (kbd "C-c d n") 'tychoish-deft-create)
(global-set-key (kbd "C-c d d") (lambda ()
                                   (interactive)
                                   (find-file "~/wiki/bwo/")))
