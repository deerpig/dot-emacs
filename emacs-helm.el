;; -*- mode: EMACS-LISP; fill-column: 75; comment-column: 50; -*-
;; Emacs Helm Configuration

;; Helm ====================================================

(require 'helm)
(require 'helm-config)

;; set default window for helm buffer to display
;; can be ‘below’, ‘above’, ‘left’ or ‘right’.
(setq helm-split-window-default-side 'left)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.

(global-set-key (kbd "<menu>") 'helm-command-prefix)

;;(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Map common emacs commands to their Helm equiv.
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
 (when (executable-find "curl")
   (setq helm-google-suggest-use-curl-p t))

;; helm-command-map key-bindings ---------------------------
;; h -- org agenda headings
;; r -- regexp
;; f -- open file
;; s -- search
;; m -- man pages
;; l -- locate
;; y -- yasnippet
(define-key helm-command-map (kbd "h") 'helm-org-agenda-files-headings)
(define-key helm-command-map (kbd "b") 'helm-bibtex)
(define-key helm-command-map (kbd "z") 'helm-select-action) ;list actions
(define-key helm-command-map (kbd "j") 'helm-bookmarks)
(define-key helm-command-map (kbd "d") 'helm-dash)
(define-key helm-command-map (kbd "r") 'helm-recoll)
(define-key helm-command-map (kbd ",") 'helm-swoop)
(define-key helm-command-map (kbd ".") 'helm-multi-swoop-all)
(define-key helm-command-map (kbd "y") 'helm-yas-complete)
(define-key helm-command-map (kbd "u") 'helm-tramp)
(define-key helm-command-map (kbd "T") 'helm-world-time)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal

;;(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-M-x-fuzzy-match t 
      helm-completion-in-region-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(setq helm-surfraw-default-browser-function nil
      helm-surfraw-duckduckgo-url "https://duckduckgo.com/?q=%s&kae=t&k5=2&kp=-1")

;; Helm open file using external program using C-c C-x

(setq helm-external-programs-associations
      (quote (("pdf"  . "evince")
	      ("ps"   . "evince")d
	      ("djvu" . "djview")
	      ("mobi" . "fbreader")
	      ("epub" . "fbreader")
	      ("gif"  . "viewnior")
	      ("jpg"  . "viewnior")
	      ("jpeg" . "viewnior")
	      ("png"  . "viewnior")
	      ("tif"  . "viewnior")
	      ("tiff" . "viewnior")
	      ("mp4"  . "vlc")
	      ("wav"  . "vlc")
	      ("mp3"  . "vlc")
	      ("mkv"  . "vlc"))))

;;(helm-mode 1)

(use-package helm-tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  (defalias 'exit-tramp 'tramp-cleanup-all-buffers))

;; Helm World Time =========================================
;; see http://en.wikipedia.org/wiki/List_of_tz_database_time_zones
;; for list of time zones

(setq display-time-world-list
      '(("America/Los_Angeles" "San Diego")
        ("America/New_York" "Boston")
        ("Europe/London" "London")
        ("Asia/Hong_Kong" "Hong Kong")
        ("Asia/Bangkok" "Bangkok")
        ("Asia/Tokyo" "Osaka")))

  (global-set-key (kbd "C-c T") 'display-time-world)

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

  (use-package helm-dictionary
     :ensure t
     )

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

;; Helm Yasnippet ------------------------------------------

(use-package helm-c-yasnippet
  :ensure t
  :config
  (global-set-key (kbd "C-c y") 'helm-yas-complete))
