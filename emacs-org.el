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

;; Winmove =================================================

(use-package windmove
  ;; :defer 4
  :ensure t
  :config
  ;; use command key on Mac
  ;;(windmove-default-keybindings 'hyper)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

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

;; WC-Org ==================================================
;; (add-hook 'org-mode-hook 'wc-mode)
;; Displays word count in modeline of org buffers.
;; Can be customized using `defcustom wc-linemode-format'
;; See http://ireal.blog/?p=6722

(use-package wc-mode
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

;; HELM ====================================================

;; Helm-unicode --------------------------------------------
;; to use, M-x helm-unicode and type name of unicode character
;; (in caps?).
 (use-package helm-unicode
   :ensure t
   )

;; Helm-bibtex ---------------------------------------------

;;"~/org/ref.bib"

(use-package helm-bibtex
  :ensure t
  :init
  (autoload 'helm-bibtex "helm-bibtex" "" t)
  (setq bibtex-completion-bibliography '("~/org/biblio.bib" "~/org/ref.org"))
  (setq bibtex-completion-pdf-field "file")
  ;;(setq bibtex-completion-additional-search-fields '(keywords))
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "okular" nil 0 nil fpath)))
  (setq bibtex-completion-notes-path "~/org/bibnotes.org")
  (setq bibtex-completion-additional-search-fields '(tags))
  )

;; helm-dictionary -----------------------------------------

;; (use-package helm-dictionary
;;    :ensure t
;;    )

;; helm-wordnet ---------------------------------------------
;; requires local install of wordnet

 (use-package helm-wordnet
   :ensure t
   )

;; helm-recoll ----------------------------------------------
 ;; (use-package helm-recoll
 ;;   :ensure t
 ;;   :config
 ;;   (helm-recoll-create-source "org" "~/.recoll/org")
 ;;   (helm :sources '(helm-source-recoll-org))
 ;;   (helm-recoll-create-source "proj" "~/.recoll/proj")
 ;;   (helm :sources '(helm-source-recoll-proj))
 ;;   (helm-recoll-create-source "doc" "~/.recoll/doc")
 ;;   (helm :sources '(helm-source-recoll-doc)))

;; 
;; https://github.com/emacs-helm/helm-recoll
;; Open file using external program using C-c C-x

(use-package helm-recoll
  :commands helm-recoll
  :init (setq helm-recoll-directories
	      '(("org"  . "~/.recoll/org")
		("proj" . "~/.recoll/proj")
		("doc"  . "~/.recoll/doc"))))

;; Helm Dash -----------------------------------------------
(use-package helm-dash
  :ensure t
  :init
  (setq helm-dash-docsets-path "~/.docsets")
  (setq helm-dash-common-docsets
	  '("Bootstrap 3" "Jekyll" "Font_Awesome"
	    "HTML" "CSS" "LaTeX" "Chef" "Vagrant" "Docker"
	    "R" "Python 2" "Python 3" "SciPy" "Ruby"
	    "Common Lisp" "Emacs Lisp" "Racket"
	    "Bash" "Apache_HTTP_Server"))
)

;; Helm Swoop ----------------------------------------------

(use-package helm-swoop  
  :ensure t
  :config
  ;; keybindings
  (global-set-key (kbd "C-M-,") 'helm-swoop)
  (global-set-key (kbd "C-M-.") 'helm-multi-swoop-all)
  ;; isearch uses to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-horizontally)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t)
  ;; Locate unix command used
  (setq helm-locate-command "locate %s -e -A --regex %s"))

;; Org-Ref =================================================
;; 
;; Org-ref is for interactively adding references to org documents
;; as they are being composed and exported.

;; (use-package org-ref
;;   :ensure t
;;   :config
;;   (setq reftex-default-bibliography '("~/org/biblio.bib"))
;;   (setq org-ref-ref-library 'org-ref-helm-cite)
  
;;   (setq org-ref-bibliography-notes    "~/org/biblio.bib"
;; 	org-ref-default-bibliography  "~/org/bibtex-pdfs"
;; 	org-ref-pdf-directory         "~/htdocs/lib")

;;   (setq bibtex-completion-bibliography "~/org/biblio.bib"
;; 	bibtex-completion-library-path "~/htdocs/lib"
;; 	bibtex-completion-notes-path   "~/org")
  
  ;; open pdf with system pdf viewer (works on mac)
  ;; (setq bibtex-completion-pdf-open-function
  ;; 	(lambda (fpath)
  ;; 	  (start-process "open" "*open*" "open" fpath)))
;;  )

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

;; YASNIPPET ================================================

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs "~/.dotfiles/emacs.d/yasnippets/snippets")
  (add-to-list 'yas-snippet-dirs "~/.dotfiles/emacs.d/yasnippets/yasmate")
  (add-to-list 'yas-snippet-dirs "~/.dotfiles/emacs.d/yasnippets/bootstrap")
  (setq warning-suppress-types '(yasnippet backquote-change))
  ;;(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-reload-all)
  ;;(define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
  ;;(define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)
  )

;; Helm Yasnippet ------------------------------------------

(use-package helm-c-yasnippet
  :ensure t
  :config
  (global-set-key (kbd "C-c y") 'helm-yas-complete))

;; Dired Ranger ============================================

; In a dired buffer, mark multiple files and then hit W to copy them.  Go
; to another directory and mark more files and hit C-u W to add Now go to
; the target directory and hit X to move to or Y to copy the files to the
; target

(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

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

;; elfeed ==================================================

(use-package elfeed
  :ensure t
  :bind
  (("C-x w" . elfeed))
  :init
  (setq elfeed-feeds
	'(("http://planet.emacsen.org/atom.xml" emacs)
	  ("http://feeds.arstechnica.com/arstechnica/index/" tech ars)
	  ("http://kk.org/cooltools/feed" tools)
	  ("http://xkcd.com/rss.xml" comic)
	  ("http://www.boingboing.net/atom.xml" tech boing)
	  ("http://sceper.ws/feed/" torrent)
	  ("http://feeds.feedburner.com/longnow" 10k longnow)
	  ("http://rss.slashdot.org/Slashdot/slashdotMain" tech slash))))

(use-package elfeed-goodies
  :ensure t
  :init
  (setq elfeed-goodies/entry-pane-position (quote bottom))
  :config
  (elfeed-goodies/setup))

;; Color Themes ============================================

;; to choose a theme interactively -- M-x color-theme-select
(use-package color-theme
  :ensure t
  )

;; HTML & XML Packages ================================

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

;; CSS Packages ============================================

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

;; Lisp & Scheme Packages ==================================

;; Quack ---------------------------------------------------

(use-package quack
  :ensure t
  )

;; Slime ---------------------------------------------------

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  )

;; Picolisp -------------------------------------------------

(use-package picolisp-mode
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
