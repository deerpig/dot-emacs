;;  DIRED ==================================================

;; Dired+ ==================================================
(require 'dired+)

;; Dired-x =================================================
(require 'dired-x)

(add-hook 'dired-load-hook 
          (function (lambda ()
                      (load "dired-x")
                      ;; Set dired-x variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      (setq dired-omit-files-p t)
                      ;; (setq dired-x-hands-off-my-keys nil)
                      )))

;; Guess Shell Command by file extension ===================

;;guess shell command by file extension.
(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'"    "evince")
  	    ("\\.ps\\'"     "evince")
	    ("\\.djvu\\'"   "djview")
	    ("\\.mobi\\'"   "fbreader")
	    ("\\.epub\\'"   "fbreader")
	    ("\\.gif\\'"    "evince")
	    ("\\.jpg\\'"    "viewnior")
	    ("\\.jpeg\\'"   "viewnior")
	    ("\\.png\\'"    "viewnior")
	    ("\\.tif\\'"    "viewnior")
	    ("\\.tiff\\'"   "viewnior")))

;; Dired Single ============================================

(require 'dired-single)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
  loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
       (lambda nil (interactive) (joc-dired-single-buffer "..")))))

(defun start-singledired()
  (progn
    (require 'dired-single)
    (if (boundp 'dired-mode-map)
        (my-dired-init)
      ;; it's not loaded yet, so add our bindings to the load-hook
      (add-hook 'dired-load-hook 'my-dired-init))))

;; Enabled single dired when browsing directory
(setq enable-singledired t)

;; Show Details by Default =================================

(setq diredp-hide-details-initially-flag nil)

;; ls Switches =============================================

;; Default for `ls switches' in Dired C-u s
;; Must contain `l'.  Hide group, owner, and make file sizes
;; human readable.  Adding an `a' will show hidden dot-files.

(setq dired-listing-switches "-la")

;; Dired Details ===========================================

;; (require 'dired-details)
;; (dired-details-install)

;; Uninteresting Files =====================================

;; Omit uninteresting files in dired
;; use M-o (toggle-omit-files) to show dot, and other files

;;(setq-default dired-omit-files-p nil) ; this is buffer-local variable

;;(setq dired-omit-files
;;       (concat dired-omit-files "\\|^\\..+$"))

;; Recursive Deletes ======================================

(setq dired-recursive-deletes 'top)

;; Dired Sort Menu =========================================

(add-hook 'dired-load-hook
           (lambda () (require 'dired-sort-menu)))

;; set scroll so that it scrolls the page one line at a time
(setq scroll-step 1)

;; Open file at point in Browser ===========================

(global-set-key "\C-c\C-b" 'browse-url-of-dired-file)

;; Make Parent Directory ===================================

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

;; List Directories First ==================================

(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
   (let (buffer-read-only)
     (forward-line 2) ;; beyond dir. header  
     (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

 (add-hook 'dired-after-readin-hook 'sof/dired-sort)

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

;; Dired Quick Sort ========================================

(use-package  dired-quick-sort
  :ensure t)
  (dired-quick-sort-setup)
