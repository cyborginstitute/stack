(provide 'tycho-instapaper) 

(require 'instapaper)

(setq instapaper-username "")
(setq instapaper-password "")

(global-set-key (kbd "C-c w i") 'instapaper-add-at-point)

