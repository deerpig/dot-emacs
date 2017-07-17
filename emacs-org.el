(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(use-package org-plus-contrib 
  :ensure t)

;; -*- mode: EMACS-LISP; fill-column: 75; comment-column: 50; -*-
;; Emacs Org Mode Configuration

(add-to-list 'load-path "~/emacs-lisp/org-mode/lisp")
(add-to-list 'load-path "~/emacs-lisp/org-mode/contrib/lisp" t)

(require 'org)

(setq org-modules 
       (quote (org-bbdb org-bibtex org-crypt org-gnus org-id org-info org-bullets org-habit org-inlinetask org-irc org-mew org-mhe org-protocol org-rmail org-vm org-wl org-w3m)))

;; Enable language evulation in Org Babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ditaa .t)
   (dot . t)
   (R . t)
   (shell . t)
   (python . t)
   (ruby . t)
   (lisp  . t)
   (scheme . t)
   (calc . t)
   (plantuml . t)
   ))

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;; Org Bullets ---------------------------------------------

(require 'org-bullets)
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

    ;; Change bullet list
    ;; https://www.miskatonic.org/2017/01/09/recent-emacs-changes/
    ;; (setq org-bullets-bullet-list '("◉" "○ ""►" "•" "•"))
    (setq org-bullets-bullet-list '("⊢" "⋮" "⦚" "⦙" "⦀")) ;; ⦚ ⦀ ⦙ ⋱ 

    ;; Change the ellipsis that indicates hidden content
    ;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
    (set-face-attribute 'org-ellipsis nil :underline nil)
    (setq org-ellipsis " ⤵") ;; ⤵ ↴ ⬎ ⤷

(define-key global-map "\M-\C-r" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; Open Agenda in Other Window -----------------------------
;; Options:
;;    reorganize-frame   :: show two windows in current frame
;;    current-window     :: use existing window
;;    only-window        :: delete other windows
;;    other-frame        :: open in other frame
(setq org-agenda-window-setup 'other-window)

;; Org Speed Commands --------------------------------------
(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("S" . widen))))

;; Helm Org ------------------------------------------------
(require 'helm-org)

;;(define-key global-map "" 'helm-org-agenda-files-headings)
;;(define-key helm-map (kbd "C-c C-t") 'helm-org-agenda-files-headings)
;;(define-key map (kbd "t")  'helm-org-agenda-files-headings)

;; Store a link to window or heading at point
;; Can then be inserted using C-c M-l
(global-set-key "\C-c\M-s" 'org-store-link)

;; Toggle active/inactive timestamps
(global-set-key "\C-c\C-x=" 'org-toggle-timestamp-type)

(setq human-readable-stamp (concat "@" user-work-location-name
	      " (" user-work-latitude
	      "-" user-work-longitude ")"))

(setq org-directory "~/org"
      org-default-notes-file "~/org/refile.org"
      org-agenda-diary-file  "~/org/diary.org")

(setq org-agenda-include-diary t
      org-enforce-todo-dependencies t
      org-cycle-separator-lines 0
      org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-startup-indented t
      ;; Show all future entries for repeating tasks
      org-agenda-repeating-timestamp-show-all nil
      ;; Show all agenda dates - even if they are empty
      org-agenda-show-all-dates nil
      ;; Start the weekly agenda on Monday
      org-agenda-start-on-weekday 1
      ;; Show one week in weekly agenda view
      org-agenda-span 8
      org-deadline-warning-days 30
      org-src-fontify-natively 't
      ;; Display tags farther right
      org-agenda-tags-column -102)

;; Enable display of the time grid so we can see the 
;; marker for the current time

(setq org-agenda-time-grid 
     (quote ((daily today remove-match)
     #("----------------" 0 16 (org-heading t))
     (0900 1100 1300 1500 1700))))

;; org-weather ===============================================
;; not on MELPA so we install the old fashioned way...
;; :home: https://github.com/kautsig/org-weather
;; :data: http://openweathermap.org/

(add-to-list 'load-path "~/emacs-lisp/org-weather")
(require 'org-weather)
;; Set your location and refresh the data
(setq org-weather-location "Phnom Penh, Cambodia"
      org-weather-api-key  "50bd25ab290db6c323c58439a8e6f41f"
      org-weather-format "天氣: %desc, %tmin-%tmax%tu, %h%hu")
(org-weather-refresh)

;; Refile Targets ------------------------------------------
;;; Use agenda files as primary refile targets

(defun deerpig/org-buffer-files ()
  "Return list of opened orgmode buffer files"
  (mapcar (function buffer-file-name)
	  (org-buffer-list 'files)))

(setq org-refile-targets
      '(;;(nil :maxlevel . 3)
	;;(deerpig/org-buffer-files :maxlevel . 1)
	("~/org/notes.org" :maxlevel . 1)
	("~/org/quotes.org" :maxlevel . 1)
	(org-agenda-files . (:maxlevel . 1))
	))

;; Enable habit tracking (and a bunch of other modules)

(require 'org-habit)
(setq org-habit-show-habits-only-for-today t)
(setq org-habit-show-all-today t)
; global STYLE property values for completion
(setq org-global-properties (quote (("STYLE_ALL" . "habit"))))
; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

;; Display images in org mode ------------------------------

(iimage-mode)
;;add the org file link format to the iimage mode regex
(add-to-list 'iimage-mode-image-regex-alist
 (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]")  1))
;; add a hook so we can display images on load
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))

;;function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))

;;function to toggle images in a org bugger
(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
      (set-face-underline-p 'org-link t))
  (call-interactively 'iimage-mode))

(setq org-tag-alist (quote (
			    ("@errand" . ?e)
			    ("@blog"   . ?b)
			    ("@work"   . ?w)
			    ("@home"   . ?h)
                        ("@farm"   . ?f)
			    ("@road"   . ?r)
			    ("@call"   . ?c)
			    ("@email"  . ?m)
			      )))

;; Easy Templates ==========================================

(add-to-list 'org-structure-template-alist '("n" "#+BEGIN_NOTES\n?\n#+END_NOTES"))
(add-to-list 'org-structure-template-alist '("C" "#+begin_comment\n?\n#+end_comment"))

;; use the tag :ignore: on a heading and org will export the content
;; of a subheading but not export the title!  Very cool!

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(x)" "|" "WORK(w)" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" "DELEGATED")
	      (sequence "NOTES(n)" "QUOTE(q)" "EMAIL" "LINK")
	      )))

(setq org-todo-keyword-faces
      '(("TODO"      :foreground "red"          :weight bold)
        ("NEXT"      :foreground "blue"         :weight bold) 
        ("DONE"      :foreground "green"        :weight bold)
	("NOTES"     . shadow)
	("DRAFT"     :foreground "purple"       :weight bold)
	("CANCELLED" :foreground "OrangeRed"    :weight bold)
	("MEETING"   :foreground "spring green" :weight bold)
	("PHONE"     :foreground "spring green" :weight bold)
	("DELEGATED" :foreground "spring green" :weight bold)
        ("QUOTE"     :foreground "yellow"       :weight bold)
        ("WORK"      :foreground "gold"         :weight bold)
	("LINK"      :foreground "tan2"         :weight bold)
       ;; ("EMAIL"     :foreground "maroon"       :weight bold)
	))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("ARCHIVE" . t))
              ;;("DONE"      ("ARCHIVE" . t))
             	     )))

;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELLED" ("CANCELLED" . t))
;;               ("WAITING" ("WAITING" . t))
;;               ("HOLD" ("WAITING") ("HOLD" . t))
;;               (done ("WAITING") ("HOLD") ("DELEGATED"))
;;               ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Org Capture =============================================

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
;; http://orgmode.org/manual/Template-expansion.html#Template-expansion
(setq org-capture-templates
      (quote (("t" "TODO" entry (file "~/org/refile.org")
               "* TODO %?\n:PROPERTIES:\n :CREATED: %T\n :ID:      %(org-id-new)\n:REF:      %a\n:END:\n")
              ("n" "Note" entry (file "~/org/refile.org")
               "* NOTES %T %?\n:PROPERTIES:\n:CREATED: %T\n:ID:      %(org-id-new)\n:REF:     %a\n:END:\n")
	          ("q" "Quote" entry (file "~/org/refile.org")
               "* QUOTE \n:PROPERTIES:\n :CREATED: %T\n :ID:      %(org-id-new)\n:END:\n\n#+begin_quote\n%?\n#+end_quote")
              ("j" "Journal" entry (file+datetree "~/org/diary.org")
               "* %?\n:PROPERTIES:\n :CREATED: %T\n :ID:      %(org-id-new)\n:END:\n")
              ("m" "Meeting" entry (file "~/org/refile.org")
               "* MEETING with %?\n:PROPERTIES:\n :CREATED: %T\n :ID:      %(org-id-new)\n:END:\n")
              ("p" "Phone call" entry (file "~/org/refile.org")
               "* PHONE %?\n:PROPERTIES:\n :CREATED: %T\n :ID:      %(org-id-new)\n:END:\n")
              ("e" "Email" entry (file "~/org/refile.org")
               "* EMAIL %?\n:PROPERTIES:\n :CREATED: %T\n :ID:      %(org-id-new)\n:END:\n") 
	          ("H" "Habit" entry (file "~/org/habits.org")
	           "** NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
              ("h" "Hoard" entry (file "~/proj/chenla/hoard/refile.org")
               "* BURR %?\n")
              ("h" "Hoard" entry (file "~/proj/chenla/hoard/refile.org")
               "* BURR %?\n")
              ("l" "Link" entry (file "~/org/refile.org") ;; insert org link
	           "* LINK %? %^C %^g \n:PROPERTIES:\n :CREATED: %T\n :ID:      %(org-id-new)\n:END:\n")
	          ("L" "Link" entry (file "~/org/refile.org") ;; create link
	           "* LINK %? %^L %^g \n:PROPERTIES:\n:CREATED:   %T\n:ID:       %(org-id-new)\n:END:\n")
	          ("b" "BibTex" plain (file "~/org/ref.bib")
               "\n\n\n\n%?")
	          ("c" "Contacts" entry (file "~/org/contacts.org")
	           "* %?%:fromname\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED:   %T\n:NAME:      %:fromname\n:EMAIL:     %:fromaddress\n:PHONE:    \n:ALIAS:    \n:NICKNAME: \n:IGNORE:   \n:ICON:     \n:NOTE:     \n:ADDRESS:  \n:BIRTHDAY: \n:END:\n"))))
;;	      ("c" "Contacts" entry (file "~/org/contacts.org")
;;	       "* %(org-contacts-template-name)\n:PROPERTIES:\n:ID:        %(org-id-new)\n:CREATED:   %T\n:NAME:    %\1 %\1\n:EMAIL: %(org-contacts-template-email)\n:PHONE:    \n:ALIAS:    \n:NICKNAME: \n:IGNORE:   \n:ICON:     \n:NOTE:     \n:ADDRESS:  \n:BIRTHDAY: \n:END:\n")

;; Open Capture In Other Frame -----------------------------
;; In Openbox rc.xml bind:
;;    emacsclient -ne "(make-capture-frame)"
;; to W-r
;; 
;; See: http://cestlaz.github.io/posts/using-emacs-24-capture-2/

(defadvice org-capture-finalize 
    (after delete-capture-frame activate)  
  "Advise capture-finalize to close the frame"  
  (if (equal "capture" (frame-parameter nil 'name))  
    (delete-frame)))

(defadvice org-capture-destroy 
    (after delete-capture-frame activate)  
  "Advise capture-destroy to close the frame"  
  (if (equal "capture" (frame-parameter nil 'name))  
    (delete-frame)))

(use-package noflet
  :ensure t)

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

;; Org Contacts ============================================

(require 'org-contacts)

;; open agenda in other window
;; Options:
;;    reorganize-frame   :: show two windows in current frame
;;    current-window     :: use existing window
;;    only-window        :: delete other windows
;;    other-frame        :: open in other frame
(setq org-agenda-window-setup 'other-window)

;; BibTex Mode Stuff ---------------------------------------

(setq bibtex-autokey-year-length 4)

;; Org Abbreviations ---------------------------------------

  (setq org-link-abbrev-alist
       '(("bug"  . "http://bugs.chenla.org/support/issue")
         ("gh"   . "https://github.com/")
         ;;("wiki" . "http://en.wikipedia.org/wiki/")
         ("rfc"  . "http://tools.ietf.org/html/")))

(defun insert-epoch ()
  "Insert time in seconds from the J2000.0 epoch in
   sec.microseconds, by subtracting unix-time"
  (interactive)
  (insert (epoch-J2000)))


(defun epoch-J2000 ()
  "Get time in seconds from the J2000.0 epoch in
   sec.microseconds, by subtracting seconds unix-time
   until year 2000"
  (replace-regexp-in-string "\n\\'" "" 
			    (concat 
			     (number-to-string 
			      (round (- (string-to-number
					 (shell-command-to-string "date +%s")) 946727935)))
			     (shell-command-to-string "date +.%N"))))

;; Jekyll Projects =========================================

(setq deerpig-publish-blog-dir "")
(setq chenla-publish-blog-dir  "~/proj/chenla/build/chenla-html/")


(setq org-html-htmlize-output-type 'css)


;; unset for localization
;;(setq org-jekyll-lang-subdirs '(("en" . "en/")))
;;(setq org-jekyll-lang-subdirs '(("kh" . "kh/")))

(require 'ox-publish)
(setq org-publish-project-alist
      (quote (("chenla-html"
	       :base-directory "~/proj/chenla/chenla-org/"
	       :base-extension "org"
	       :publishing-directory  ,chenla-publish-blog-dir
	       :recursive t
	       :publishing-function org-html-publish-to-html
	       :headline-levels 4
           :table-of-contents nil
	       :html-extension "html")
       
	      ("chenla-static"
	       :base-directory "~/proj/chenla/chenla-org/"
	       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
	       :publishing-directory ,chenla-publish-blog-dir
	       :recursive t
	       :publishing-function org-publish-attachment)
       
	      ("chenla" :components ("chenla-html" "chenla-static"))


	      ("core-docs"
	       :base-directory "~/proj/chenla/core-docs/"
	       :base-extension "org"
	       :publishing-directory "~/proj/chenla/core-html/"
	       :recursive t
	       :publishing-function org-html-publish-to-html
	       :headline-levels 4
	       :html-extension "html")

	      
	      ("core-static"
	       :base-directory "~/proj/chenla/core-docs/"
	       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
	       :publishing-directory "~/proj/chenla/core-html/"
	       :recursive t
	       :publishing-function org-publish-attachment)
       
	      ("core" :components ("core-docs" "core-static"))
	      )))

;; Deploy projects -----------------------------------------

(defun deploy (name)
  "Build and deploy local websites using shell script to call
   orgmode and git.  Scripts are kept in ~/.dotfiles/ and
   linked to ~/bin/"
  (interactive "cdeploy: (c)ore (l)a (d)eerpig")
  (shell-command     (cond ((equal name ?c) "cored")
			   ((equal name ?l) "betad")
			   ((equal name ?d) "deerd"))))

;; delete 2 blank spaces at top of files
;; when publishing

;; (add-hook 'org-publish-after-export-hook
;; 	  '(lambda nil
;; 	     (goto-char (point-min))
;; 	     (while (re-search-forward "^\n\n---" nil t)
;; 	       (replace-match "---"))
;;   ))

(use-package org-octopress
  :ensure t)

;; Export for Jekyll subtrees ------------------------------


(load-file "~/emacs-lisp/ox-jekyll-subtree.el")
;;(load-file "~/emacs-lisp/ox-jekyll.el")
(autoload 'endless/export-to-blog "jekyll-once")
(setq org-jekyll-use-src-plugin t)

(defun deerpig/set-blog-var ()
  "pull values for #+BLOGDIR and #+BASEURL and replace settings for 
   endless/blog-dir and endless/blog-base-url so that we can use 
   ox-jekyll-subtree for multiple blogs...."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "#+BLOGDIR:")
    (forward-char 1)
    (setq endless/blog-dir
	  (buffer-substring-no-properties (point) (line-end-position)))
    (goto-char (point-min)) 
    (search-forward "#+BASEURL:")
    (forward-char 1)
    (setq endless/blog-base-url
	  (buffer-substring-no-properties (point) (line-end-position)))

    ))

;; Obviously, these two need to be changed for your blog.
(setq endless/blog-base-url "http://chenla.org/")
(setq endless/blog-dir (expand-file-name "~/proj/chenla/chenla-org/"))

(defun whack-whitespace (arg)
      "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

;; add man page url -----------------------------------------
(provide 'org-man)

;; Use org-ids in org-links --------------------------------

(setq org-id-track-globally t)
(setq org-id-link-to-org-use-id 'use-existing)
(org-id-update-id-locations) ;; updates id-index

;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
;;
;; Snarfed From: http://stackoverflow.com/a/11384907

(defun deerpig/find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
	 (case-fold-search t)	      ; filesystems are case sensitive
	 (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
	 (filext (or filext "org$\\\|org_archive"))
	 (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
	 (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
	(if (string-match fileregex file-or-dir) ; org files
	    (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
	(dolist (org-file (deerpig/find-org-file-recursively file-or-dir filext)
			  org-file-list) ; add files found to result
	  (add-to-list 'org-file-list org-file)))))))

;; (setq org-agenda-text-search-extra-files
;;       '(agenda-archives
;;         "~/org/pinboard.org"
;;         "~/org/subdir/textfile1.txt"))

(setq org-agenda-text-search-extra-files
      (append (deerpig/find-org-file-recursively "~/org/" "org")
              (deerpig/find-org-file-recursively "~/proj/" "org")))

;; Add Org-ID to Headings in File --------------------------

(defun deerpig/org-add-id-to-headings ()
  "Insert and org-id to all headings in file at point that do not
have an id"
  (interactive)
   (require 'org-id)
   (save-excursion
      (goto-char (point-max))
      (while (outline-previous-heading)
        (org-id-get-create)))
  )

;; Org Reveal ==============================================
;;

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t)
  (setq org-reveal-note-key-char t)
 )
;; note that htmlize has already been loaded elsewhere...

;; ================================
;; Insert Fuctions
;; ================================


(defun insert-org ()
  "Insert template header in a org document"
  (interactive)
        (goto-char (point-min))
        (insert "#   -*- mode: org; fill-column: 60 -*-\n")
	(insert "#+TITLE: \n")
	(insert "#+AUTHOR: Brad Collins \\\<brad@chenla.la>\n")
	(insert "#+DATE: \n")
        (insert "#+STARTUP: showall\n")
      ;;(insert "#+FILETAGS: \n")
        (insert "#+INFOJS_OPT: view:info toc:t ltoc:t\n")
        (insert "#+HTML_HEAD_EXTRA: <style>body {margin-left:50px; width:60%;}</style>\n")
      ;;(insert "\n [ [[~/org/index.org][index]] | ]\n\n")
	(insert "  :PROPERTIES:")
        (insert "\n  :Name: ")
        (insert (format "%s" (buffer-file-name)))
        (insert "\n  :Created: ")
        (insert-iso)
        (insert "@")
        (insert user-work-location-name)
        (insert " (")
	(insert user-work-latitude)
	(insert "-")
	(insert user-work-longitude)
	(insert ")")
	(insert "\n  :ID: ")
	(insert (org-id-new))
	(insert "\n  :URL:")
	(insert "\n  :END:\n\n\n"))

(defun insert-latex ()
  "Insert latex header template in a org document at point."
  (interactive)
        (insert "#+OPTIONS: toc:nil H:5 todo:nil tasks:nil timestamp:nil\n")
        (insert "#+OPTIONS: \\n:nil email:nil d:nil skip:nil\n")
        (insert "#+LaTeX_CLASS_OPTIONS: [12pt,A4]\n")
        (insert "#+LaTeX_HEADER: \\usepackage[T1]{fontenc}\n")
        (insert "#+LaTeX_HEADER: \\usepackage{mathpazo}\n")
        (insert "#+LaTeX_HEADER: \\linespread{1.05}\n")
        (insert "#+LaTeX_HEADER: \\usepackage[scaled]{helvet}\n")
        (insert "#+LaTeX_HEADER: \\usepackage{courier}\n")
        (insert "#+LaTeX_Header: \\usepackage{parskip}\n")
        (insert "#+LaTeX_HEADER: \\usepackage{paralist}\n")
        (insert "#+LaTeX_HEADER: \\let\\itemize\\compactitem\n")
        (insert "#+LaTeX_HEADER: \\let\\description\\compactdesc\n")
        (insert "#+LaTeX_HEADER: \\let\\enumerate\\compactenum\n")
        (insert "#+LaTeX_HEADER: \\usepackage{lastpage}\n")
        (insert "#+LaTeX_HEADER: \\usepackage{fancyhdr}\n")
        (insert "#+LaTeX_HEADER: \\pagestyle{fancy}\n")
        (insert "#+LaTeX_HEADER: \\renewcommand{\\headrulewidth}{0pt}\n")
        (insert "#+LaTeX_HEADER: \\lhead{} \\chead{} \\rhead{}\n")
        (insert "#+LaTeX_HEADER: \\lfoot{ver:1.0 date}\n")
        (insert "#+LaTeX_HEADER: \\cfoot{title}\n")
        (insert "#+LaTeX_HEADER: \\rfoot{\\thepage\\ of \\pageref{LastPage}}\n")
        (insert "#+LaTeX: \\thispagestyle{fancy}\n")
        (insert "#+LATEX_HEADER: \\usepackage{hyperref}\n")
        (insert "#+LATEX_HEADER: \\hypersetup{\n")
        (insert "#+LATEX_HEADER:     colorlinks,%\n")
        (insert "#+LATEX_HEADER:     citecolor=black,%\n")
        (insert "#+LATEX_HEADER:     filecolor=black,%\n")
        (insert "#+LATEX_HEADER:     linkcolor=blue,%\n")
        (insert "#+LATEX_HEADER:     urlcolor=black\n")
        (insert "#+LATEX_HEADER: }\n"))

;; (defun insert-burr-org ()
;;   "Insert template header in a org Burr"
;;   (interactive)
;; 	(insert " :PROPERTIES:")
;;         (insert "\n :Name: ")
;; 	(insert "\n :Ver: ")
;; 	(insert-epoch)
;;         (insert " (J2000.0)")
;; 	(insert "\n :Type: ")
;;         (insert "\n :Created: ")
;;         (insert-iso)
;;         (insert "@")
;;         (insert user-work-location-name)
;;         (insert " (")
;; 	(insert user-work-latitude)
;; 	(insert "-")
;; 	(insert user-work-longitude)
;; 	(insert ")")
;; 	(insert "\n :ID: ")
;; 	(insert (org-id-new))
;; 	;;(insert "\n :REL:")
;; 	(insert "\n :END:\n\n\n")
;; 	)

;; (defun insert-tspace-org ()
;;   "Insert template topicspace header in a org document"
;;   (interactive)
;;         (goto-char (point-min))
;;         (insert " Topicspace  -*- mode: org; fill-column: 60 -*-\n")
;;         (insert "#+STARTUP: showall\n")
;; 	(insert "#+FILETAGS: BMF")
;;         (insert "\n [ [[~/org/index.org][index]] | ]\n\n")
;; 	(insert "  :PROPERTIES:")
;;         (insert "\n  :Name:  _(topicspace)")
;; 	(insert "\n  :Entity: topicspace")
;; 	(insert "\n  :Ver: ")
;; 	(insert-epoch)
;; 	(insert " (J2000.0)")
;;         (insert "\n  :Created: ")
;;         (insert-iso)
;;         (insert "@")
;;         (insert user-work-location-name)
;;         (insert " (")
;; 	(insert user-work-latitude)
;; 	(insert "-")
;; 	(insert user-work-longitude)
;; 	(insert ")")
;; 	(insert "\n  :ID: ")
;; 	(insert (org-id-new))
;; 	(insert "\n  :Path:")
;;         (insert (format "%s" (buffer-file-name)))
;; 	(insert "\n  :END:\n\n\n")
;;         (insert "* \n") 
;; 	)


(defun insert-moleskine ()
  "Insert template org-mode properties box for notes transcribed from my moleskine into an org note"
  (interactive)
       ;; (goto-char (point-min))
	(insert "  :PROPERTIES:")
	(insert "\n  :Comment: Transcribed from small moleskine")
        (insert "\n  :Created: ")
	(insert "\n  :ID: ")
	(insert (org-id-new))
	(insert "\n  :END:\n\n\n")
	)

(defun insert-uuid ()
  "Insert a UUID at point"
  ;; FIXME: problem is that you have to be in org
  ;;  mode to use this
  (interactive)
  (insert (org-id-new)))

(defun insert-filename-org ()
  "Insert a file-name comment at point.  Used to update older muse files"
  (interactive)
  (insert "; Name:  ")
  (insert (format "%s" (buffer-file-name)))
  )

(defun insert-blog ()
  "Insert template header in a blog post"
  (interactive)
        (goto-char (point-min))
        (insert "#+STARTUP: showall\n")
	(insert "#+STARTUP: hidestars\n")
        (insert "#+INFOJS_OPT: view:info toc:t ltoc:nil\n")
	(insert "#+OPTIONS: H:2 num:nil tags:nil toc:nil timestamps:nil\n")
	(insert "#+BEGIN_HTML\n")
	(insert "---\n")
	(insert "layout: post\n")
	(insert "title : \"\"\n")
	(insert "topics: \n")
	(insert "style : blog\n")
	(insert "---\n")
	(insert "#+END_HTML\n\n\n"))

(defun insert-hex-bxid ()
  "Insert 4 slot hex BXID"
  (interactive)
  (insert "!")
  (insert-hex)
  (insert "!")
  (insert-hex)
  (insert "!")
  (insert-hex)
  (insert "!")
  (insert-hex))


(defun insert-burr ()
  "Insert template header for a new Burr"
  (interactive)

  (insert "* DRAFT \n")      
  (org-insert-time-stamp nil t t nil nil nil)
  (insert "\n\n:HEAD:\n")
  (insert ":bmf: bmfver:0.0,100_Canary\n")
  (insert ":id: [[bmf: /base/")
  (insert-epoch)
  (insert "\n")
  (insert ":entity-type: [[bmf:doc_(entity_type)]]\n")
  (insert ":bxid: bxid:")
  (insert-hex-bxid)
  (insert "\n")
  (insert ":layer: [[bmf:base_(layer_type)]]\n")
  (insert ":owner: [[agent:@deerpig]]\n")
  (insert ":created: stamp:")
  (insert-iso)
  (insert "@")
  (insert (princ user-work-latitude))
  (insert ";-")
  (insert (princ user-work-longitude))
  (insert "\n")
  (insert ":creator: [[agent:@deerpig]]\n")
  (insert ":version: ver:")
  (insert-epoch)
  (insert "\n")
  (insert ":END:\n")
  (insert ":TREE:\n")
  (insert ":tt: [[bmf:BMF_(topicspace)]]\n")
  (insert ":bt: [[\n")
  (insert ":pt: [[bmf:\n")
  (insert ":END:\n\n")
  (insert "** Scope Note\n")
  (insert "** References\n"))

  ;;(insert "** Scope Note\n")
  ;;(insert "=x= is a BMF element used in the =head= section.  The
  ;;         only allowed value is =x=.\n")
  ;;(insert "** Meta\n")
  ;;(insert ":module:       [[bmf:BMF_core]]\n")
  ;;(insert ":parent:       [[bmf:head_(section_type)]]\n")
  ;;(insert ":children:     not allowed\n")
  ;;(insert ":value:        \n")
  ;;(insert ":attributes:   not allowed\n")
  ;;(insert "** Usage\n")
  ;;(insert "*** XML Syntax\n")
  ;;(insert "#+begin_example\n\n")
  ;;(insert "#+end_example\n")
  ;;(insert "*** Wiki Syntax\n")
  ;;(insert "#+begin_example\n\n")
  ;;(insert "#+end_example\n")
  ;;(insert "** Schema\n")
  ;;(insert "** References\n"))

(global-set-key (kbd "<f9> b") 'insert-burr)


(defun insert-ideo ()
  "Insert ideograph character template at point"
  (interactive)
        ;;(goto-char (point-min))
        (insert ":Reading:   \n")
	(insert ":Cantonese: \n")
	(insert ":Pinyin:    \n")
	(insert ":UTF-8:     U+\n")
	(insert ":Grade:     \n")
	(insert ":Radical:   \n")
	(insert ":Stroke:    \n")
	(insert ":Frequency: \n\n\n")
	(insert ":Animated: \n")
	(insert ":Unihan:   \n")
	(insert ":Zongwhen: \n\n"))

;; Org Width ===============================================
;; set image width in org buffers

(setq org-image-actual-width nil)

;; Pomodomo Timer ==========================================
(use-package pomidor
  :ensure t)
