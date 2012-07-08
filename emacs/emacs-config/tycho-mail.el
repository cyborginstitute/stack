(provide 'tycho-mail)

(require 'tycho-mail-setup)
;; (require 'notmuch-lib)
;; (require 'notmuch)
;; (require 'notmuch-mua)
;; (require 'notmuch-hello)
;; (require 'notmuch-show)
(require 'lbdb)

;; (setq rmail-movemail-program "~/scripts/mdmovemail")
;; (setq rmail-primary-inbox-list (list (expand-file-name "~/mail/inbox/.rmailfile")))
;; mail and mutt realted stuff

(defun axels-mail-mode-hook ()
  (turn-on-auto-fill) ;;; Auto-Fill is necessary for mails
  (turn-on-font-lock) ;;; Font-Lock is always cool *g*
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
  (not-modified) ;;; We haven't changed the buffer, haven't we? *g*
  (message-goto-body) ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil) ;;; No backups necessary.
  (auto-fill-mode 1)
  (abbrev-mode 1))

(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
(add-to-list 'auto-mode-alist '("/mutt" . message-mode))

(add-hook 'mail-mode-hook 'axels-mail-mode-hook)
(add-hook 'mail-mode-hook 'orgstruct-mode)
(add-hook 'mail-mode-hook 'mc-install-write-mode)
(add-hook 'mail-mode-hook (lambda () (flyspell-mode 1)))

(add-hook 'message-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'message-mode-hook (lambda () (set-face-foreground 'message-cited-text "grey60")))
(add-hook 'message-setup-hook 'orgstruct-mode)
(add-hook 'message-setup-hook 'mc-install-write-mode)
(add-hook 'message-setup-hook 'axels-mail-mode-hook)
(setq mail-header-separator "")t
(add-hook 'message-mode-hook 'turn-on-auto-fill
          (function
           (lambda ()
             (progn
               (local-unset-key "\C-c\C-c")
               (define-key message-mode-map "\C-c\C-c" '(lambda ()
                                                          "save and exit quickly"
                                                          (interactive)
                                                          (save-buffer)))))))

(setq message-default-mail-headers "Cc: \nBcc: \n"
      message-from-style 'angles
      message-send-mail-function 'message-send-mail-with-sendmail
      compose-mail-user-agent-warnings nil
      mail-user-agent 'message-user-agent
      mail-specify-envelope-from t
      mail-header-separator ""
      mail-signature t)

(load-library "mailcrypt") ; provides "mc-setversion"
(mc-setversion "gpg")    ; for PGP 2.6 (default); also "5.0" and "gpg"

(autoload 'mc-install-write-mode "mailcrypt" nil t)
(autoload 'mc-install-read-mode "mailcrypt" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; big brother database stuffs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'bbdb)
;; (bbdb-initialize)
