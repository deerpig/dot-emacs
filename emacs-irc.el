;; RCIRC =================================================
;; Register Nick on IRC Servers

(rcirc-track-minor-mode 1)
(setq rcirc-default-nick "deerpig")
(setq rcirc-default-user-name "deerpig")
(setq rcirc-default-full-name "Brad Collins")

(setq rcirc-server-alist
   ;; the backtick is important
      `(("irc.freenode.net" :password ,(cadr (auth-source-user-and-password "irc.freenode.net")) 
                                    :channels ("#emacs" "#chenla"))
  	("irc.chenla.org"             :channels ("#chenla"))
      ("keyelementsg.irc.slack.com" :port 6697 :encryption tls
   	       :password ,(cadr (auth-source-user-and-password "keyelementsg.irc.slack.com"))
   	       :channels ("#general" "random"))
               ))
