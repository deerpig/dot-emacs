;; -*- mode: EMACS-LISP; fill-column: 75; comment-column: 50; -*-
;; Emacs Mu4e Configuration

;; mu4e ===========================================

;; the exact path may differ -- check it
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'org-mu4e)

(setq mu4e-get-mail-command "mbsync -a"
      mu4e-mu-binary (executable-find "mu"))

(setq mu4e-attachment-dir  "~/Downloads")

(setq mu4e-user-mail-address-list
      '("brad@chenla.la"
	    "deerpig@gmail.com"
	    "brad.collins@pathmazing.com")
      ;; emacs email defaults
      user-full-name    "Brad Collins"
      user-mail-address "brad@chenla.la"
      mail-host-address "chenla.la")

(setq ;;mu4e-sent-messages-behavior 'delete
      ;; don't show info about indexing new messages
      mu4e-hide-index-messages t
      ;; attempt to show images
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      ;; start in non-queuing mode)

(setq smtpmail-queue-mail nil
      smtpmail-queue-dir "~/Maildir/queue/"
      message-kill-buffer-on-exit t ;; kill sent msg buffers
      ;; use ssmtp
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program (executable-find "/usr/sbin/ssmtp")
      ;; Look at the from header to determine the account from which
      ;; to send. Might not be needed b/c of mlh-msmtp
      ;;mail-specify-envelope-from t
      ;;mail-envelope-from 'header
      ;;message-sendmail-envelope-from 'header)

(setq mu4e-update-interval nil
      org-mu4e-link-query-in-header-mode nil)

;; <RET> Opens URLs in external browsers
(define-key mu4e-view-mode-map (kbd "RET") 'mu4e~view-browse-url-from-binding)

;; The bookmarks for the main screen
(setq mu4e-bookmarks
          `(;;(,(mlh-mu4e-unread-mail-query) "New messages"         ?b)
            ;;("maildir:/elastic/build"      "Build failures"       ?B)
            ("date:today..now NOT T"       "Today's messages"     ?t)
            ("date:7d..now NOT T"          "Last 7 days"          ?w)
            ("maildir:/chenla/INBOX"       "Chenla"               ?c)
            ("maildir:/gmail/INBOX"        "Deerpig"              ?d)
            ("maildir:/cas/github"         "Casnak"               ?n)
            ("maildir:/path/INBOX"         "Pathmazing"           ?p)
            ("maildir:/chenla/INBOX OR maildir:/gmail/INBOX OR maildir:/path/INBOX"
             "All Mail" ?a)))

;; if you set this to nil so signature is not included by default
;; you can include in message with C-c C-w
(setq mu4e-compose-signature-auto-include t)
(setq mu4e-compose-signature (with-temp-buffer
			       (insert-file-contents "~/.dotfiles/signature")
			       (buffer-string)))
;; message-signature-file NOT used by mu4e
(setq message-signature-file "~/.dotfiles/signature")