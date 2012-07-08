(require 'jabber)
(require 'hexrgb)

 (setq jabber-account-list
    '(("account"
       (:network-server . "server")
       (:connection-type . network))
     ))

(setq jabber-chat-buffer-show-avatar nil)
(setq jabber-vcard-avatars-retrieve nil)
(setq jabber-avatar-cache-directory "/dev/null")
(setq jabber-roster-line-format " %c %-25n %u %-8s  %S")
(setq jabber-cat-fill-long-lines t)

(add-hook 'jabber-chat-mode-hook 'flyspell-mode)

(require 'znc)

(setq znc-servers '(("remote.cyborginstitute" 4400 t 
                        ((oftc "tycho-oftc" "test") 
                         (freenode "tycho-freenode" "test") 
                         ))))


