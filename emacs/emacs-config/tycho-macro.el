(provide 'tycho-macro)

(fset 'markdown-indent-code
   "     \C-a\C-n")
(global-set-key (kbd "C-c i") 'markdown-indent-code)