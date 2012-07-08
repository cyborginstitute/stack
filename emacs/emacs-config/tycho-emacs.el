(setq server-use-tcp t)
(provide 'tycho-emacs)

;;
;; my packages and custom configurations
;;

(require 'custom)
(require 'tycho-desktop)
(require 'tycho-menu)
(require 'tycho-latex)
(require 'tycho-ikiwiki)
(require 'tycho-macro)
(require 'tycho-keybindings)
(require 'tycho-writing)
(require 'tycho-instapaper)
(require 'tycho-make)
(require 'tycho-functions)

;; Packages I don't use much/anymore: 

;(require 'tycho-org)
;(require 'tycho-muse)

;; Packages I should use, but that only work on some systems.

;(require 'tycho-python)
;(require 'tycho-magit)
;(require 'tycho-w3m)

;; (load "tycho-python.el" nil t t)

;;
;; applications and system level packages
;;

; (require 'ledger)
; (require 'ledgerutils)
; (require 'ljupdate)

(require 'yasnippet)
(require 'identica-mode)
(require 'netrc)

;; Major modes

(require 'yaml-mode)
(require 'stumpwm-mode)

;; Minor modes

(require 'tea-time)
(require 'rainbow-mode)
(require 'browse-kill-ring)
;(require 'assoc)
(require 'uniquify)
; (require 'autopair)
(require 're-builder)
(require 'hide-region)
(require 'comint)
(require 'recentf)

;(require 'hl-line)
;(require 'saveplace)
;(require 'undo-tree)


;; autoloaded modes

(autoload 'htaccess-mode "htaccess.el" "mode for editing apache's htaccess files")
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files" )
(autoload 'xml-mode "psgml" "Major mode to edit XML files" )
; (autoload 'autopair-global-mode "autopair" nil t)

(electric-pair-mode 1)

(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

; (setq identica-username "tychoish")
; (setq identica-auth-mode "oauth")
(setq identica-auth-mode "auth")
(let ((identica (netrc-machine (netrc-parse "~/.authinfo") "identi.ca" t)))
    (setq identica-password (netrc-get identica "password")
          identica-username (netrc-get identica "login")))
(global-set-key (kbd "C-c t i") 'identica-mode)

;;
;; confguration
;;

(add-hook 'yaml-mode-hook 'flyspell-mode)
(add-hook 'html-mode-hook 'flyspell-mode)

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yaml$" . yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yml$" . yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xml" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.htaccess" . htacess-mode) auto-mode-alist))

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-all-abbrevs
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-dabbrev
        try-expand-line
        try-expand-line-all-buffers
        try-expand-list
        try-expandlist-all-buffers
        try-expand-whole-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(yas/initialize)
(yas/load-directory "~/emacs/snippets")
(setq yas/prompt-functions '(yas/x-prompt yas/ido-prompt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; settings
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-revert-interval 5)
(setq markdown-command "/usr/bin/mmd2XHTML.pl")
(setq autopair-global-mode nil)
(setq-default indicate-empty-lines t)
(winner-mode t)
(column-number-mode t)
(icomplete-mode t)
(recentf-mode 1)
(windmove-default-keybindings)
(browse-kill-ring-default-keybindings)
; (global-undo-tree-mode)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq autopair-autowrap t)

(setq default-major-mode 'markdown-mode)
(setq ispell-program-name "aspell")
; (set-face-background 'hl-line "gray80")

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq query-replace-highlight t)
(setq search-highlight t)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(set-default 'truncate-lines t)

(setq ls-lisp-dirs-first t)
(setq vc-handled-backends nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time nil)
(setq jit-lock-stealth-load 200)
(setq jit-lock-defer-time nil)
(setq next-line-add-newlines nil)
(setq reb-re-syntax 'string)

(setq flyspell-sort-corrections nil)
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-list-command "list")
(setq tea-time-sound "~/media/music/waterson-carthy/Holy Heathens And The Old Green Man/17 Gloryland.m4a")
(setq display-time-mode t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-veprsions 2
      version-control t
      backup-directory-alist  nil
      auto-save-file-name-transforms nil
      make-backup-files nil
      backup-by-copying t)

(setq frame-title-format
      '(:eval
        (if buffer-file-name
            (replace-regexp-in-string
             (getenv "HOME") "~"
             (concat (file-name-directory buffer-file-name) "%b") )
          (buffer-name)
          )))

(setq warnings-to-ignore '())
(add-to-list 'warnings-to-ignore '(free-vars))
(add-to-list 'warnings-to-ignore '(nresolved))
(add-to-list 'warnings-to-ignore '(callargs))
(add-to-list 'warnings-to-ignore '(redefine))
(add-to-list 'warnings-to-ignore '(obsolete))
(add-to-list 'warnings-to-ignore '(noruntine))
(add-to-list 'warnings-to-ignore '(cl-functions))
(add-to-list 'warnings-to-ignore '(interactive-only))
(setq byte-compile-warnings warnings-to-ignore)

(setq save-abbrevs t)
(setq default-abbrev-mode t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))


