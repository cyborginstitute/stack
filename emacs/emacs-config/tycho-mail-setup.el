(provide 'tycho-mail-setup)

(message "configure tycho-mail-setup.el or remove all refrences.")

;; (setq rmail-movemail-program ";;")
;; (setq rmail-primary-inbox-list (list (expand-file-name ";;")))

;; (setq sendmail-program "/usr/bin/msmtp"
;;       notmuch-fcc-dirs "sent/"
;;       message-auto-save-directory "~/mail/drafts/"
;;       message-directory "~/mail/"
;;       message-sendmail-extra-arguments '("-a" "tychoish-default")
;;       user-mail-address "garen@tychoish.com"
;;       user-full-name "tycho garen"
;;       mail-host-address "tychoish.com"
;;       mail-signature-file "~/mail/tools/signatures/default")

;; (defun tychoish-mail-personal ()
;;   (interactive)
;;   (setq sendmail-program "/usr/bin/msmtp"
;;         message-sendmail-extra-arguments '("-a" "msmtp-magic")
;;         user-full-name "name"
;;         mail-host-address "domain"
;;         user-mail-address "address"
;;         mail-signature t
;;         mail-signature-file "~/mail/signatures/default")
;;   (beginning-of-buffer)
;;   (while (re-search-forward "^From:.*$" nil t 1)
;;     (replace-match "From: name <address>" nil nil)))

;; (defun tychoish-mail-work ()
;;   (interactive)
;;   (setq sendmail-program "/usr/bin/msmtp"
;;         message-sendmail-extra-arguments '("-a" "msmtp-magic")
;;         user-full-name "name"
;;         mail-host-address "domain"
;;         user-mail-address "address"
;;         mail-signature t
;;         mail-signature-file "~/mail/signatures/default")
;;   (beginning-of-buffer)
;;   (while (re-search-forward "^From:.*$" nil t 1)
;;     (replace-match "From: name <address>" nil nil)))

;; (setq mc-gpg-user-id "")
