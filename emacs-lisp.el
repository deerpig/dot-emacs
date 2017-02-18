;; Lisp & Scheme Packages ==================================

;; Pretty Lambdas ==========================================

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
				    ,(make-char 'greek-iso8859-7 107))
		    nil))))))

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

;; Emacs Lisp ==============================================

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (interactive)
	     (require 'eldoc)
	     (turn-on-eldoc-mode)
	     (pretty-lambdas)
	     (define-key emacs-lisp-mode-map [(control c) (x)] 'copy-eval-dwim-lisp)
	     ;; Default to auto-indent on Enter
	     (define-key emacs-lisp-mode-map [(control j)] 'newline)
	     (define-key emacs-lisp-mode-map [(control m)] 'newline-and-indent)))
