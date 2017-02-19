;; Deerpig's Crufty .emacs
;; This dot emacs is a poor example

;; I don't use XEmacs ======================================  
;; This file does not work with XEmacs.
(when (featurep 'xemacs)
  (error "This .emacs file does not work with XEmacs."))

;; THE BARE MINIMUM ========================================

;; first lets get rid of the gui crutches for newbies

;; no tool bar
(tool-bar-mode 0)

;; no menu bar
(menu-bar-mode 0)

;; no scroll bar
(scroll-bar-mode -1)

;; Disable the silly ring
(setq ring-bell-function '(lambda()))

;; The follwing settings constitute a minimum .emacs file for using
;; emacs on remote servers.

;; no scratch message
(setq initial-scratch-message nil)

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; No, please, no tabs in my programs!
(setq indent-tabs-mode nil)

;; I kinda know my emacs
(setq inhibit-startup-message t)

;; flash matching parenthesis
(require 'flash-paren)
(flash-paren-mode 1)

;; Start the emacs-client ==================================
(server-start)

;; emacsclient stuff to automatically open new frame and close and
;; clean everything up after you've finished

(add-hook 'server-switch-hook
	  (lambda nil
	    (let ((server-buf (current-buffer)))
	      (bury-buffer)
	      (switch-to-buffer-other-frame server-buf))))

(add-hook 'server-done-hook 'delete-frame)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

;; External path search ====================================

(add-to-list 'load-path "~/emacs-lisp")
(add-to-list 'load-path "~/emacs-lisp/test")
(add-to-list 'load-path "~/emacs-lisp/emacs-wiki-deerpig")
(add-to-list 'load-path "~/emacs-lisp/docs")
(add-to-list 'load-path "~/emacs-lisp/uri")
(add-to-list 'load-path "~/emacs-lisp/remember")
(add-to-list 'load-path "~/emacs-lisp/psgml")
(add-to-list 'load-path "~/emacs-lisp/ecb")
(add-to-list 'load-path "~/emacs-lisp/semantic")
(add-to-list 'load-path "~/emacs-lisp/tex")
(add-to-list 'load-path "~/emacs-lisp/ses")
(add-to-list 'load-path "~/emacs-lisp/burr")
(add-to-list 'load-path "~/emacs-lisp/sxml-mode")
(add-to-list 'load-path "~/emacs-lisp/xpath")

(add-to-list 'load-path (expand-file-name "~/emacs-lisp/w3/lisp"))
(add-to-list 'load-path "~/emacs-lisp/elib-1.0")
(add-to-list 'load-path "~/emacs-lisp/regexp-info")
(add-to-list 'load-path "~/emacs-lisp/edb")
(add-to-list 'load-path "~/emacs-lisp/xtla")
(add-to-list 'load-path "~/emacs-lisp/wl/elmo")
(add-to-list 'load-path "~/emacs-lisp/etask")
(add-to-list 'load-path "~/emacs-lisp/burs")
(add-to-list 'load-path "~/emacs-lisp/atom-blogger")

(add-to-list 'load-path "~/emacs-lisp/emacs-atom-api")
(add-to-list 'load-path "~/emacs-lisp/nxhtml")
(add-to-list 'load-path "~/emacs-lisp/hyperbole")
(add-to-list 'load-path "~/emacs-lisp/emacs-jabber")
(add-to-list 'load-path "~/emacs-lisp/jd-el")
(add-to-list 'load-path "~/emacs-lisp/google-contacts")
(add-to-list 'load-path "~/emacs-lisp/multiple-cursors.el")
(add-to-list 'load-path "~/emacs-lisp/emacs-async")

;; Prefer UTF-8 over Latin-1 ===============================

(set-language-environment 'english)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Display path in frame title =============================
 (setq frame-title-format
       '((:eval (if (buffer-file-name)
		    (abbreviate-file-name (buffer-file-name))
		  "%b"))))

;; set the default font ====================================

(set-default-font "Deja Vu Sans Mono-12")
 ;;(set-fontset-font (frame-parameter nil 'font)
 ;;   'han '("cwTeXHeiBold" . "unicode-bmp"))

;; Package Manager & Repositories ==========================
(eval-when-compile
  (require 'package))
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Use-package =============================================

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; Customizations ==========================================
;; keep all emacs customizations in file that is not
;; part of the repo.
(setq custom-file "~/.emacs-custom")
(load custom-file 'noerror)

(setq auth-source-debug t)

(use-package auth-source
  :config
  (setq
   auth-sources '(
                  ;; default
                  ;; "secrets:session"
                  ;; "secrets:Login"
                  "~/.authinfo"
                  )
   epa-file-cache-passphrase-for-symmetric-encryption t
   ;; need the following to avoid dbus compiling error
   auth-source-debug 'trivia
   ))

;; Load literate Org files =================================

(load "~/.emacs-user-info")
(load "~/.emacs-helm")
(load "~/.emacs-dired")
(load "~/.emacs-org")
(load "~/.emacs-mu4e")
(load "~/.emacs-packages")
(load "~/.emacs-hydra")
(load "~/.emacs-irc")
(load "~/.emacs-misc")
(load "~/.emacs-lisp")
(load "~/.emacs-sort")

;; Set the Default Browser =================================
;;
;; use firefox as default browser
;; (setq browse-url-browser-function 'browse-url-firefox)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(defvar browse-url-firefox-program 'firefox)
(defvar browse-url-kde-program 'firefox)
(defvar browse-url-gnome-moz-program 'firefox)
(defvar browse-url-mozilla-program 'firefox)
(defvar browse-url-galeon-program 'firefox)
(defvar browse-url-netscape-program 'firefox)
(defvar browse-url-mosaic-program 'firefox)
(defvar browse-url-xterm-program 'firefox)

;; Browse Apropos URL ======================================

(require 'browse-apropos-url)
(provide 'browse-url)
(require 'thingatpt+)

;; Choose commands that mirror DuckDuckGo !bang commands when
;; possible.  !bang should go to search on a site.  !!bangbang should
;; just be a shortcut to a url.  If it's not a search link, default
;; both single and double bang prefix to home page.

(setq apropos-url-alist
      '( ;; DuckDuckGo is default search engine.
	("^\??:? +\\(.*\\)" .       ;; "?" defaults to DuckDuckGo
	 "http://duckduckgo.com/?q=\\1")
	("^!ddg?:? +\\(.*\\)" .     ;; DuckDuckGo Search
	 "http://duckduckgo.com/?q=\\1")
        ("^!!ddg"                   ;; DuckDuckGo Home Page
	 "http://duckduckgo.com")
	("^!bang$" .                ;; DuckDuckGo !bang Page
	 "http://duckduckgo.com/bang.html")
        ;; Google Sites ;;;;;;;;;;;;;;;;;;;;;;
	("^gw?:? +\\(.*\\)" .
         "http://www.google.com/search?q=\\1")
	("^!g?:? +\\(.*\\)" . 	    ;; Google Web Search
	 "http://www.google.com/search?q=\\1")
        ("^!!g$" . 	            ;; Google Home Page
	 "http://google.com/")
        ("^!reader?:? +\\(.*\\)" .  ;; Search Google Reader
         "http://www.google.com/reader/view/#search/\\1")
        ("^!!reader$" .             ;; Google Reader Home
         "http://reader.google.com/")
        ("^!!voice$" .              ;; Google Voice
         "http://voice.google.com/")
	("^!!gmail$" .              ;; GMail
	 "http://mail.google.com/")
	("^!gi:? +\\(.*\\)" .       ;; Google Images
	 "http://images.google.com/images?sa=N&tab=wi&q=\\1")
	("^!!gi$" .                 ;; Google Images
	 "http://images.google.com/")
	("^!gg:? +\\(.*\\)" .       ;; Google Groups
	 "http://groups.google.com/groups?q=\\1")
        ("^!gn:? +\\(.*\\)" .       ;; Google News Search
         "http://news.google.com/news?sa=N&tab=dn&q=\\1")
        ("^!!gn$" .                 ;; Google News Home
         "http://news.google.com/")
	;;Blekko ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ("^!blekko?:? +\\(.*\\)" .  ;; Blekko Search
	 "http://blekko.com/ws/+\\1")
	("^!!blekko$" .             ;; Blekko Home
	 "http://blekko.com/")
        ;; Tech News ;;;;;;;;;;;;;;;;;;;;;;;;;;
	("^!/\.$" . ;; Slashdot Home
         "http://www.slashdot.org")
	("^!!/\.$" . ;; Slashdot Home
         "http://www.slashdot.org")
        ("^!bb$" . ;; Boing Boing Home
         "http://boingboing.net")
        ("^!!bb$" . ;; Boing Boing Home
         "http://boingboing.net")
        ;; Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	("^!emacs:? +\\(.*\\)" . ;; Emacs Wiki Search
         "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")
        ("^!!emacs$" . ;; Emacs Wiki Home
         "http://www.emacswiki.org")
        ;;Hacker News ;;;;;;;;;;;;;;;;;;;;;;;;
	("^!hn:? +\\(.*\\)" . ;; Hacker News Search
	"http://www.hnsearch.com/search#request/all&q=\\1")
	("^!!hn$" . ;; Hacker News Home
	"http://news.ycombinator.com")
	;;Torrent Search ;;;;;;;;;;;;;;;;;;;;
	("^!tpb:? +\\(.*\\)" . ;;The Pirate Bay
	"http://thepiratebay.com/search/\\1")
	("^!demon:? +\\(.*\\)" . ;; Demonoid Search
	"https://www.demonoid.me/files/?query=\\1" )
	("^!demonoid:? +\\(.*\\)" . ;; Demonoid Search
	"https://www.demonoid.me/files/?query=\\1" )
	("^!isohunt:? +\\(.*\\)" . ;;ISOHunt
	"https://isohunt.com/torrents/?ihq=\\1" )
	("^!cheggit:? +\\(.*\\)" . ;;Cheggit Search
	"http://cheggit.net/browsetorrents.php?filter=all%3A%5B\\1" )
	("^!!cheggit:? +\\(.*\\)" . ;;Cheggit Home
	"http://cheggit.net/browsetorrents.php" )
	("^!jpop:? +\\(.*\\)" . ;; JPopSuki Artist Search
	"http://jpopsuki.eu/torrents.php?action=advanced&artistname=\\1" )
	;; Content Companies ;;;;;;;;;;;;;;;;;
	("^!amazon:? +\\(.*\\)" . ;; Amazon
	"http://www.amazon.com/s/?&field-keywords=\\1" )
	("^!imdb:? +\\(.*\\)" . ;; IMDB
	"http://www.imdb.com/find?s=all&q=\\1" )
        ("^!tmdb:? +\\(.*\\)" . ;; The Movie Database
	"http://www.themoviedb.org/search?search=\\1")
        ;; Add Later

	))

;; Emacs Desktop ===========================================
;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

(setq desktop-buffers-not-to-save
(concat "\\("
       "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
       "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
       "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'magit-mode)
;; should add twittering, eirc, elfeed?


;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save		
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; Save/Read the desktop
(desktop-load-default)
(desktop-read)

;; Calendar Style
(setq european-calendar-style t)

;; work-around for error message:
;;  void-function fancy-diary-display
(setq diary-display-function 'diary-fancy-display)

;; Regexp Info Manual ======================================
(require 'info)
(setq Info-default-directory-list 
      (cons "~/emacs-lisp/regexp-info" Info-default-directory-list))

(setq Info-default-directory-list 
      (cons "~/emacs-lisp/burs" Info-default-directory-list))


;; Elisp tutorial and manual ===============================
(require 'info)
(setq Info-default-directory-list 
      (cons "/usr/local/info" Info-default-directory-list))

(setq Info-default-directory-list 
     (cons "~/emacs-lisp/docs" Info-default-directory-list))
