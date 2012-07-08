(provide 'tycho-lisp)

(require 'slime)
; (require 'highlight-sexps)
; (require 'slime-helper)
; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(slime-setup '(slime-fancy))

;; (autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
;; (add-hook 'slime-repl-mode-hook       (lambda () (paredit-mode +1)))

;(add-hook 'lisp-mode-hook 'highlight-sexps-mode)
;(add-hook 'emacs-lisp-mode-hook 'highlight-sexps-mode)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
      (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; Using local-set-key in a mode-hook is a better idea.
(global-set-key (kbd "RET") 'electrify-return-if-match)

(require 'redshank-loader "~/emacs/redshank/redshank-loader")
   (eval-after-load "redshank-loader"
      `(redshank-setup '(lisp-mode-hook
                           slime-repl-mode-hook) t))
