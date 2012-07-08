(provide 'tycho-python)

(require 'python-mode)
(require 'pydoc-info)
(pydoc-info-add-help '("python" "sphinx"))

(require 'virtualenv)
(setq ipython-command nil)
;(setq ipython-command "/usr/bin/ipython2")
;(require 'ipython)
;(setq py-python-command-args '("-i"))

(setq py-python-command-args nil)
(add-hook 'python-mode-hook
     (lambda ()
      (define-key python-mode-map "\"" 'tychoish-electric-pair)
      (define-key python-mode-map "\'" 'tychoish-electric-pair)
      (define-key python-mode-map "(" 'tychoish-electric-pair)
      (define-key python-mode-map "[" 'tychoish-electric-pair)
      (define-key python-mode-map "{" 'tychoish-electric-pair)
      (define-key python-mode-map "\C-m" 'newline-and-indent)))

(autoload 'python-mode "python-mode.el" "mode for working in python")

(global-set-key (kbd "C-c h p") 'pylookup-lookup)

(setq pylookup-dir "~/emacs/pylookup")
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
