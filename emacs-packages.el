;; -*- mode: EMACS-LISP; fill-column: 75; comment-column: 50; -*-
;; Use-Package

;; Unicode Fonts ===========================================

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

;; Gruvbox Theme ===========================================
;; https://github.com/greduan/emacs-theme-gruvbox

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;; remove the strange white line between two fringes.
(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))

;; Winmove =================================================

(use-package windmove
  ;; :defer 4
  :ensure t
  :config
  ;; use command key on Mac
  ;;(windmove-default-keybindings 'hyper)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

;; Buffer Move =============================================

(use-package buffer-move
  :ensure t
  :config
  )

;; Avy Jump Mode ===========================================

(use-package avy
  :ensure t
  :config
  (setq avy-all-windows 'all-frames)
  (global-set-key (kbd "C-,") 'avy-goto-char-2)
  (global-set-key (kbd "C-.") 'avy-goto-word-1))

;; UUID Generators =========================================
;; See [[rfc:4122][RFC4122]]
;; M-x uuidgen inserts a uuid -- but I haven't worked out
;; how to call this programically -- for now just doing it
;; using (org-new-id)

(use-package uuidgen
  :ensure t
  )

;; Powerline ===============================================
(use-package powerline
  :ensure t
  :init
  (powerline-default-theme)
  (setq powerline-color1 "#C05800")
  (setq powerline-color2 "#839496")
  (setq powerline-arrow-shape 'arrow))

;; eshell ===================================================
(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

;; Define a keybinding to get to your eshell quickly.
(global-set-key (kbd "C-c e") 'eshell)

;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

;; emms ====================================================

;;(use-package emms
;;  :ensure t
;;  :config
;;  (progn
;;    (emms-standard)
;;    (emms-default-players)
;;    (setq emms-playlist-buffer-name "Music-EMMS")
;;    (setq emms-source-file-default-directory "/media/deerpig/green/music")))

;; WC-Org ==================================================
;; (add-hook 'org-mode-hook 'wc-mode)
;; Displays word count in modeline of org buffers.
;; Can be customized using `defcustom wc-linemode-format'
;; See http://ireal.blog/?p=6722

(use-package wc-mode
  :ensure t
  )

;; Org-Ref =================================================

;; Org-ref is for interactively adding references to org documents
;; as they are being composed and exported.

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/org/biblio.bib"))
  (setq org-ref-ref-library 'org-ref-helm-cite)

  (setq org-ref-bibliography-notes    "~/org/biblio.bib"
	org-ref-default-bibliography  "~/org/bibtex-pdfs"
	org-ref-pdf-directory         "~/htdocs/lib")

  (setq bibtex-completion-bibliography "~/org/biblio.bib"
	bibtex-completion-library-path "~/htdocs/lib"
	bibtex-completion-notes-path   "~/org")

 ;; open pdf with system pdf viewer (works on mac)
  ;; (setq bibtex-completion-pdf-open-function
  ;; 	(lambda (fpath)
  ;; 	  (start-process "open" "*open*" "open" fpath)))
 )

;; Interleave Mode ==========================================

;; Interleave org notes in pdf files

  (use-package interleave
    :ensure t
    :config
)

;; SSH =====================================================

;; may or may not help emacs not prompt for ssh key passphrases

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package which-key
  :ensure t
  :config 
  (which-key-setup-side-window-right)
  (which-key-mode)
)

;; GIT Packages ============================================

(use-package git
  :ensure t)

;; Magit ---------------------------------------------------

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

;; Git-Gutter ----------------------------------------------
;; :home: https://github.com/syohex/emacs-git-gutter
(use-package git-gutter
  :ensure t
  :config
  ;; use globally
  (global-git-gutter-mode +1)
  ;; add hook if you want to only use for specific modes
  (add-hook 'ruby-mode-hook 'git-gutter-mode)
  (add-hook 'python-mode-hook 'git-gutter-mode))

;; Magithub ------------------------------------------------
;; SEE: http://jr0cket.co.uk/2017/02/spacemacs-using-magit-with-github.html
;;
;; requires installation of hub, see: https://hub.github.com/
;; which I'm not quite ready to do.

;; (use-package magithub
;;   :ensure t
;;   :after magit
;;   :config (magithub-feature-autoinject t))

;; nov.el --------------------------------------------------
;; epub reader
;; requires the esxml library with css-select checked out.

(add-to-list 'load-path "~/.emacs.d/esxml")
(require 'esxml)
(load "~/.emacs.d/nov/nov.el")
(setq nov-unzip-program "/usr/bin/unzip")
(push '("\\.epub\\'" . nov-mode) auto-mode-alist)

;; Git Timemachine -----------------------------------------

(use-package  git-timemachine
  :ensure t
)

;; Dired K =================================================

(use-package dired-k
  :ensure t
  :config 
  (setq dired-k-human-readable t)
  (define-key dired-mode-map (kbd "K") 'dired-k)
  ;; You can use dired-k alternative to revert-buffer
  (define-key dired-mode-map (kbd "g") 'dired-k)

  ;; always execute dired-k when dired buffer is opened
  (add-hook 'dired-initial-position-hook 'dired-k)

  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

;; YASNIPPET ================================================

(use-package yasnippet
  :ensure t
  :config
    (setq yas-snippet-dirs
      '("~/.emacs.d/yasmate/"
        "~/.emacs.d/snippets/"))
  (setq warning-suppress-types '(yasnippet backquote-change))
  ;;(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  ;;(define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
  ;;(define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)
  )

  (yas-global-mode 1)
  (yas-reload-all)

;; Programing Languages ====================================
;; Except Lisp, which has it's own file.

;; PHP =====================================================

(use-package php-mode
  :ensure t
  )

;; Ruby ====================================================

(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :interpreter "ruby")

;; Dictionaries and Word Definitions =======================

;; Define Word 
;; looks up definition online in word-nik
;; (use-package define-word
;;   :ensure t
;;   )
 (use-package dictionary
   :ensure t
   )

;; Boxquote =================================================

(use-package boxquote
  :ensure t )

;; Lorem ipsum ==============================================

(use-package lorem-ipsum
  :ensure t)

;; Twittering ===============================================

  (use-package twittering-mode
  :ensure t
  :config
  (setq twittering-use-master-password t)
  (setq twittering-icon-mode t)         ; Show icons
  (setq twittering-timer-interval 300)  ; Update timeline each 300 seconds
  (setq twittering-url-show-status nil) ; Keeps the echo area from
 				        ; showing all the http processes
  )

;; elfeed =================================================

(setq elfeed-db-directory "~/.elfeed")

(use-package elfeed
  :ensure t
  :init
  (setq-default elfeed-search-filter "@1-month-ago +unread")
  :bind (:map elfeed-search-mode-map
	      ("q" . bjm/elfeed-save-db-and-bury)
	      ("Q" . bjm/elfeed-save-db-and-bury)
	      ("j" . hydra-elfeed/body)
	      ("J" . hydra-elfeed/body))
   :config
     (elfeed-org)

     (defun elfeed-link-title (entry)
       "Copy the entry title and URL as org link to the clipboard."
       (interactive)
       (let* ((link (elfeed-entry-link entry))
              (title (elfeed-entry-title entry))
              (titlelink (concat "[[" link "][" title "]]")))
         (when titlelink
           (kill-new titlelink)
           (x-set-selection 'PRIMARY titlelink)
           (message "Yanked: %s" titlelink))))

     ;; show mode

     (defun elfeed-show-link-title ()
       "Copy the current entry title and URL as org link to the clipboard."
       (interactive)
       (elfeed-link-title elfeed-show-entry))

     (defun elfeed-show-quick-url-note ()
       "Fastest way to capture entry link to org agenda from elfeed show mode"
       (interactive)
       (elfeed-link-title elfeed-show-entry)
       (org-capture nil "n")
       (yank)
       (org-capture-finalize))

     (bind-keys :map elfeed-show-mode-map
                ("l" . elfeed-show-link-title)
                ("v" . elfeed-show-quick-url-note))

     ;; search mode

     (defun elfeed-search-link-title ()
       "Copy the current entry title and URL as org link to the clipboard."
       (interactive)
       (let ((entries (elfeed-search-selected)))
         (cl-loop for entry in entries
                  when (elfeed-entry-link entry)
                  do (elfeed-link-title entry))))

     (defun elfeed-search-quick-url-note ()
       "In search mode, capture the title and link for the selected
   entry or entries in org aganda."
       (interactive)
       (let ((entries (elfeed-search-selected)))
         (cl-loop for entry in entries
                  do (elfeed-untag entry 'unread)
                  when (elfeed-entry-link entry)
                  do (elfeed-link-title entry)
                  do (org-capture nil "n")
                  do (yank)
                  do (org-capture-finalize)
                  (mapc #'elfeed-search-update-entry entries))
         (unless (use-region-p) (forward-line))))

     (bind-keys :map elfeed-search-mode-map
                ("l" . elfeed-search-link-title)
                ("v" . elfeed-search-quick-url-note)))

	      ;;("m" . elfeed-toggle-star)
	      ;;("M" . elfeed-toggle-star)


(defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;; (defalias 'elfeed-toggle-star
;;   (elfeed-expose #'elfeed-search-toggle-all 'star))


;; elfeed goodies ======================================

(use-package elfeed-goodies
  :ensure t
  :init
  (setq elfeed-goodies/entry-pane-position (quote bottom))
  :config
  (elfeed-goodies/setup))

;; elfeed-org ==========================================

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

;; Color Themes ============================================

;; to choose a theme interactively -- M-x color-theme-select
(use-package color-theme
  :ensure t
  )

;; WebDev ==================================================

;; HTML Tidy -----------------------------------------------

(use-package tidy
  :ensure t
  )

;; RelaxNG Mode --------------------------------------------

(use-package rnc-mode
  :ensure t
  )

;; Htmlize -------------------------------------------------

(use-package htmlize
  :ensure t
  )

;; Web-Mode ------------------------------------------------
;;
;; Replacement for html mode.
;;
;; See:
;; :url: http://web-mode.org/ ;; home page
;; :url: http://cestlaz.github.io/posts/using-emacs-21-web-mode/#.WC0t1LMxVhF
(use-package web-mode
    :ensure t
    :config
	 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	 (setq web-mode-engines-alist
	       '(("django"    . "\\.html\\'"))) ;; use for liquid (jekyll)
	 (setq web-mode-ac-sources-alist
	       '(("css" . (ac-source-css-property))
		 ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting t))

;; Rainbow mode --------------------------------------------

(use-package rainbow-mode
  :ensure t
  :config
   (autoload 'rainbow-mode "rainbow" nil t nil)
   (add-hook 'css-mode-hook
	     (lambda ()
	       (rainbow-mode 1)))
  )

;; CSS Mode ------------------------------------------------

(use-package css-mode
  :ensure t
  :init
  (defalias 'apropos-macrop 'macrop)
  (autoload 'css-mode "css-mode")
  (setq auto-mode-alist       
    (cons '("\\.css\\'" . css-mode) auto-mode-alist))
  )

;; CSV Mode ================================================

(use-package csv-mode
  :ensure t
  )

;; findr ===================================================

(use-package findr
  :ensure t
  )

;; wwtime ==================================================
(use-package wwtime
  :ensure t
  )

;; JSON ====================================================

(use-package json
  :ensure t
  )

;; Chef & Cucumber =========================================

;; Chef ----------------------------------------------------

(use-package chef-mode
  :ensure t
  )

;; Cucumber ------------------------------------------------

;; (edit .feature files)
(use-package feature-mode
  :ensure t)

;; Emacs Lisp Development ==================================

;; Cask ----------------------------------------------------

;; (project package management for elisp)
(use-package cask
  :ensure t
  )

;; Ecukes --------------------------------------------------

;; (cucumber like tests for elisp)
(use-package ecukes
  :ensure t
  )

;; Expand Region ===========================================
;; expand the marked region in semantic increments 
;; (negative prefix to reduce region)
;; Bound to C-= by default

(use-package expand-region
:ensure t
:config 
(global-set-key (kbd "C-=") 'er/expand-region))

;; Hungry Delete ===========================================
;; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
:ensure t
:config
(global-hungry-delete-mode))

;; iEdit ===================================================
;; edit all instances of a marked region in a buffer
;; bound to C-;
(use-package iedit
  :ensure t)

;; Beacon Mode =============================================
;; flashes the cursor's line when you scroll

(use-package beacon
:ensure t
:config
(beacon-mode 1)
; this color looks good for the zenburn theme but not for the one
; I'm using for the videos
(setq beacon-color "#666600")
)

;; Calfw ===================================================

(use-package calfw
  :ensure t 
  :config
   (require 'calfw-org)
   (setq cfw:org-overwrite-default-keybinding t)
   ;;(setq cfw:org-agenda-schedule-args '(:timestamp))

   (define-key global-map "\C-cq" 'cfw:open-org-calendar)
)


   (setq calendar-day-name-array
      ["日" "月" "火" "水" "木" "金" "土"])

   ;; Default setting
   (setq cfw:fchar-junction ?+
      cfw:fchar-vertical-line ?|
      cfw:fchar-horizontal-line ?-
      cfw:fchar-left-junction ?+
      cfw:fchar-right-junction ?+
      cfw:fchar-top-junction ?+
      cfw:fchar-top-left-corner ?+
      cfw:fchar-top-right-corner ?+ )

   ;; Unicode characters
   ;; (setq cfw:fchar-junction ?╋
   ;;       cfw:fchar-vertical-line ?┃
   ;;       cfw:fchar-horizontal-line ?━
   ;;       cfw:fchar-left-junction ?┣
   ;;       cfw:fchar-right-junction ?┫
   ;;       cfw:fchar-top-junction ?┯
   ;;       cfw:fchar-top-left-corner ?┏
   ;;       cfw:fchar-top-right-corner ?┓)

(custom-set-faces
 '(cfw:face-title ((t (:foreground "darkgoldenrod3" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-header ((t (:foreground "maroon2" :weight bold))))
 '(cfw:face-sunday ((t :foreground "red" :weight bold)))
 '(cfw:face-saturday ((t :foreground "blue" :weight bold)))
 '(cfw:face-holiday ((t :background "grey10" :foreground "purple" :weight bold)))
 '(cfw:face-default-content ((t :foreground "green2" )))
 '(cfw:face-regions ((t :foreground "cyan")))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-today-title ((t :background "red4" :weight bold)))
 '(cfw:face-today ((t :foreground: "cyan" :weight bold)))
 '(cfw:face-select ((t :background "blue4")))
 '(cfw:face-toolbar-button-off ((t :foreground "cyan" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold)))
;;  '(cfw:face-grid ((t :foreground "DarkGrey")))
;;  '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
;;  '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 )

;; Chronos =================================================
 (use-package chronos 
   :ensure t
   :init
 (setq chronos-expiry-functions '(chronos-buffer-notify
                                  chronos-dunstify))
)

;; Helm Chronos ============================================
 (use-package helm-chronos
   :ensure t
   :init
 (setq helm-chronos-standard-timers
   '( "   1/You Win!"
      "   2/Drink Beer!"
      "   4/Soak noodles"
      "  25/Pomodoro: Work on helm-chronos + 5/Pomodoro: Rest"))
)

(use-package eyebrowse
  :ensure t
  :config

  ;;(setq eyebrowse-keymap-prefix (kbd "H-w"))
  ;;(global-set-key (kbd "H-w") 'eyebrowse-keymap-prefix)
  ;;(global-unset-key (kbd "C-c C-w"))

  (eyebrowse-mode t))
