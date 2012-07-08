(provide 'tycho-make)
(require 'git-dired)
(require 'flymake)

(require 'make-mode)
(require 'ninja-mode)

(setq makefile-electric-keys t)
(setq compilation-scroll-output t)

(setq auto-mode-alist (cons '("Makefile" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".mk" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("makefile" . makefile-mode) auto-mode-alist))

(global-set-key (kbd "C-c t w") 'tycho-toggle-hooks)

(defconst makefile-nmake-statements
  `("!IF" "!ELSEIF" "!ELSE" "!ENDIF" "!MESSAGE" "!ERROR" "!INCLUDE" ,@makefile-statements)
  "List of keywords understood by nmake.")

(defconst makefile-nmake-font-lock-keywords
  (makefile-make-font-lock-keywords
   makefile-var-use-regex
   makefile-nmake-statements
   t))

(define-derived-mode makefile-nmake-mode makefile-mode "nMakefile"
  "An adapted `makefile-mode' that knows about nmake."
  (setq font-lock-defaults
        `(makefile-nmake-font-lock-keywords ,@(cdr font-lock-defaults))))

(setq *save-hook-off* t)

(defun tycho-toggle-hooks ()
  "Reset the before-save hook to preven cleaning up"
  (interactive)
  (cond
   ((equal *save-hook-off* nil)
    (add-hook 'before-save-hook 'whitespace-cleanup)
    (setq show-trailing-whitespace t)
    (message "whitespace-cleanup on")
    (setq *save-hook-off* t))
   ((equal *save-hook-off* t)
    (setq before-save-hook nil)
    (setq show-trailing-whitespace nil)
    (message "whitespace-cleanup off")
    (setq *save-hook-off* nil))))
