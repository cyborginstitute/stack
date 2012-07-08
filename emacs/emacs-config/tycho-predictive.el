(add-to-list 'load-path "~/emacs/predictive")

(require 'predictive)

;; preditive mode stuff 

(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'dict-english
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

(add-hook 'markdown-mode-hook 'predictive-mode)
(add-hook 'org-mode-hook 'predictive-mode)
(add-hook 'html-mode-hook 'predictive-mode)
