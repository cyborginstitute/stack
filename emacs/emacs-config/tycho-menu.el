(provide 'tycho-menu)
(require 'helm-config)
(require 'projectile)
(require 'helm-projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monkeying with defaults. Because I like ido mode a bunch.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ido-mode t)
(setq ido-create-new-buffer 'always)

(global-set-key (kbd "C-c C-f") 'set-fill-column)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

(defun ido-find-file-recentf ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x C-r") 'ido-find-file-recentf)
(global-set-key (kbd "C-x f") 'ido-find-file-read-only)

(require 'kill-ring-search)

(setq ido-use-virtual-buffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings to make things seem peppier
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq helm-idle-delay 0.2)
(setq helm-input-idle-delay 0)
(setq helm-candidate-number-limit 100)
(setq helm-c-adaptive-sorting t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Anything commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-=") 'helm-mini)
(global-set-key (kbd "C-x a") 'helm-mini)
(global-set-key (kbd "C-x x") 'helm-mini)
(global-set-key (kbd "C-x C-a") 'helm-mini)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-set-key (kbd "C-c h a") 'helm-mini)
(global-set-key (kbd "C-c h f") 'helm-for-files)
(global-set-key (kbd "C-c h b") 'helm-buffers-list)
(global-set-key (kbd "C-c h w") 'helm-write-file)
(global-set-key (kbd "C-c h r") 'helm-recentf)

;; (global-set-key (kbd "C-x f") 'helm-find-files)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x w") 'helm-write-file)
;; (global-set-key (kbd "C-x i") 'helm-insert-file)

(global-set-key (kbd "C-x C-r") 'helm-recentf)

(global-set-key (kbd "C-x w") 'helm-write-file)

(global-set-key (kbd "C-c h w") 'helm-write-file)
(global-set-key (kbd "C-x i") 'helm-insert-file)

;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x C-f") 'helm-for-files)

(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x w") 'ido-write-file)
(global-set-key (kbd "C-x i") 'ido-insert-file)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "C-c h i") 'helm-imenu)

;; (global-set-key (kbd "M-y") 'browse-kill-ring)
;; (global-set-key (kbd "M-i") 'imenu)
;; (global-set-key (kbd "C-c h i") 'imenu)f

(global-set-key (kbd "C-c w a") 'helm-w3m-bookmarks)
