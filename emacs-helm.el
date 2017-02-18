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
