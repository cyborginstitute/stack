(require 'deft)
(provide 'work-deft)

(setq deft-extension "mdwn")
(setq deft-directory "~/wiki/work/bwo/")
(setq deft-text-mode 'markdown-mode)

(defun work-todo ()
  (interactive)
  (find-file-read-only "~/wiki/work/todo.mdwn")
  (visual-line-mode 0)
  (revbufs))
(defun work-flow ()
  (interactive)
  (find-file "~/wiki/work/flow.mdwn")
  (visual-line-mode 0))
(defun work-mongodb-docs-compile-html ()
  (interactive)
  (compile "make SPHINXOPTS='-c ./ -N' -k -C ~/work/mongodb-docs html"))
(defun work-mongodb-docs-compile-aspirational ()
  (interactive)
  (compile "make SPHINXOPTS='-c ./ -N' -k -C ~/work/mongodb-docs draft"))
(defun work-mongodb-docs-compile-publish ()
  (interactive)
  (compile "make SPHINXOPTS='-c ./ -N' -C ~/work/mongodb-docs publish"))
(defun work-mongodb-docs-compile-push ()
  (interactive)
  (compile "make SPHINXOPTS='-c ./ -N' -C ~/work/mongodb-docs publish"))
(defun work-mms-docs-compile ()
  (interactive)
  (compile "make SPHINXOPTS='-c ./ -N' -k -C ~/work/mms-docs html"))

(global-set-key (kbd "C-c l p") 'work-mongodb-docs-compile-publish)
(global-set-key (kbd "C-c l P") 'work-mongodb-docs-compile-push)
(global-set-key (kbd "C-c l h") 'work-mongodb-docs-compile-html)
(global-set-key (kbd "C-c l a") 'work-mongodb-docs-compile-aspirational)
(global-set-key (kbd "C-c l m") 'work-mms-docs-compile)

(setq deft-auto-save-interval 0)

(defun make-deft-frame ()
  "Create a new frame and run deft."
  (interactive)
  (make-frame '((name . "deft")))
  (select-frame-by-name "deft")
  (delete-other-windows)
  (deft))

(defun deft-file-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun tychoish-deft-create (title)
  "Create a new rhizome post."
  (interactive "swork BwO Title: ")
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
                                   (find-file "~/wiki/work/bwo/")))
