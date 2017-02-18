;; Insert Stuff ============================================

(defun insert-stuff (name)
  (interactive "cInsert (D)ate,(E)mail,(I)SO,(N)ame,(F)ilename,(L)oc,(B)XID,(U)UID,(W)iki-note,(T)asks, Fix U(R)L")
  (insert (cond ((equal name ?d)(format-time-string "%3b-%m-%Y"))
		((equal name ?e)(insert-email-address))
		((equal name ?i)(format-time-string "%Y-%02m-%02dT%02H:%02M"))
		((equal name ?n)(concat user-full-name " <" 
                  user-mail-address ">"))
		((equal name ?f)(format "%s" (buffer-name)))
		((equal name ?l)(concat (capitalize user-work-location-name)
                                   " (" user-work-latitude " " 
                                   user-work-longitude ")"))
                 
                 ((equal name ?b)(insert-bxid))
                 ((equal name ?u)(uuid)) 
                 ((equal name ?w)(format-time-string "notes-%Y-%m-%d"))
                 ((equal name ?t) (planner-authority-task-template))
                 ((equal name ?r) (burr-replace-url))
		(t ((equal name ?i)(format-time-string "%Y-%02m-%02dT%02H:%02M"))))))
(global-set-key [f1] 'insert-stuff)

;; Inserts the date in the format 
(defun insert-email-address ()
  "Insert `user-email-address' at point."
  (interactive)
  (insert user-mail-address))

;; Inserts the date in the format 
(defun insert-iso ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%02m-%02dT%02H:%02M")))

;; Inserts the date in the format 
(defun insert-date ()
  "Insert date at point."
  (interactive)
       (insert (format-time-string "%d.%m.%Y %H:%M")))

;; Inserts Unix Time

(defun insert-unix ()
  "Insert unix time at point."
  (interactive)
  (insert (format "%s" (float-time)))
  )

;; Insert Sig file of choice

(defun signature (name)
  (interactive "cSig: (M)ailing List Studio(J)ungle, (C)henla, (D)eerpig, (S)tandard")
  (goto-char (point-max))
  (insert "\n")
  (insert-file (cond ((equal name ?e) "~/brad_stuff/emacs.sig")
		     ((equal name ?j) "~/brad_stuff/studiojungle.sig")
		     ((equal name ?c) "~/brad_stuff/chenla.sig")
		     ((equal name ?d) "~/brad_stuff/deerpig.sig")
		     ((equal name ?d) "~/brad_stuff/mail-list.sig")
		     (t "~/brad_stuff/standard.sig"))))
;;(global-set-key [f2] 'signature)


;; Function to insert a commented seperator line: 
;;

(defun insert-comment-heading (comment)
  "Insert COMMENT, followed by \" ---...\".  The line will be
  commented based on which mode you are in." 
  (interactive "sComment: ")
  (insert  comment " " (make-string (- (window-width)
                                       (+ (length comment) 5)
                                       10)
                                    ?-))
  (comment-region (point-at-bol) (point-at-eol))
  (newline))
