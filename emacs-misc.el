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

;; Go to Beginning or End of Special Buffers =========================

(defmacro my-special-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-min'.  This way repeated invocations
toggle between real beginning and logical beginning of the
buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-beginning-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap beginning-of-buffer] ',fname))))))



(defmacro my-special-end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-max' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-max'.  This way repeated invocations
toggle between real end and logical end of the buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-end-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap end-of-buffer] ',fname))))))
;; Dired (M-x dired) -----------------------------

(my-special-beginning-of-buffer dired
  (while (not (ignore-errors (dired-get-filename)))
    (dired-next-line 1)))
(my-special-end-of-buffer dired
  (dired-previous-line 1))

;; Occur (M-x occur) -----------------------------

(my-special-beginning-of-buffer occur
  (occur-next 1))
(my-special-end-of-buffer occur
  (occur-prev 1))

;; Ibuffer (M-x ibuffer) -------------------------

(my-special-beginning-of-buffer ibuffer
  (ibuffer-forward-line 1))
(my-special-end-of-buffer ibuffer
  (ibuffer-backward-line 1))

;; vc directory view (M-x vc-dir or C-x v d) -----

(my-special-beginning-of-buffer vc-dir
  (vc-dir-next-line 1))
(my-special-end-of-buffer vc-dir
  (vc-dir-previous-line 1))

;; bs (M-x bs-show) ------------------------------

(my-special-beginning-of-buffer bs
  (bs-down 2))
(my-special-end-of-buffer bs
  (bs-up 1)
  (bs-down 1))

;; Recentf (M-x recentf-open-files) --------------

(my-special-beginning-of-buffer recentf-dialog
  (when (re-search-forward "^  \\[" nil t)
    (goto-char (match-beginning 0))))
(my-special-end-of-buffer recentf-dialog
  (re-search-backward "^  \\[" nil t))

;; Org Agenda (M-x org-agenda) ------------------

(my-special-beginning-of-buffer org-agenda
  (org-agenda-next-item 1))
(my-special-end-of-buffer org-agenda
  (org-agenda-previous-item 1))

;; ag (from ag.el package, M-x ag) ---------------

(my-special-beginning-of-buffer ag
  (compilation-next-error 1))
(my-special-end-of-buffer ag
  (compilation-previous-error 1))
