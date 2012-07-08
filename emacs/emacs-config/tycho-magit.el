(provide 'tycho-magit)

(require 'magit)

(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g w") 'magit-wazzup)
(global-set-key (kbd "C-x g b c") 'mo-git-blame-current)
(global-set-key (kbd "C-x g b f") 'mo-git-blame-file)

(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g w") 'magit-wazzup)
(global-set-key (kbd "C-c g b c") 'mo-git-blame-current)
(global-set-key (kbd "C-c g b f") 'mo-git-blame-file)

(autoload 'magit-status "magit" nil t)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(setq magit-save-some-buffers nil)
