;; Hydra ===============================================
;; See https://github.com/abo-abo/hydra#awesome-docstring

(use-package hydra
  :ensure t  
  :config
(require 'hydra-examples) ;; needed for hydra-move-splitter
)

;; Key Chords =============================================

(use-package key-chord
  :ensure t
  :init
  (progn
   ;; (fset 'key-chord-define 'my/key-chord-define)
    (setq key-chord-one-key-delay 0.16)
    (setq key-chord-two-key-delay 0.16)
    (key-chord-mode 1)
    ;72(key-chord-define-global "1q"     ')
    (key-chord-define-global "2w"     'hydra-window/body)
    (key-chord-define-global "3e"     'hydra-join-lines/body)
    (key-chord-define-global "4r"     'hydra-rectangle/body)
    (key-chord-define-global "5t"     'hydra-twittering/body)
    (key-chord-define-global "6y"     'hydra-transpose/body)
    (key-chord-define-global "7u"     'hydra-unicode/body)
    (key-chord-define-global "8i"     'hydra-logic/body)
    (key-chord-define-global "9o"     'hydra-greek/body)
    (key-chord-define-global "0p"     'hydra-calendar/body)
    ))

;; hydra-window --------------------------------------------
;;
;; hydra for managing windows

(defhydra hydra-window (:color pink :hint nil :timeout 20)
  "
         Move                    Resize            Split
╭─────────────────────────────────────────────────────────────┐
         ^_<up>_^                    ^_C-<up>_^            [_v_]ertical
          ^^▲^^                         ^^▲^^              [_h_]orizontal
 _<left>_ ◀   ▶ _<right>_    _C-<left>_ ◀   ▶ _C-<right>_
          ^^▼^^                         ^^▼^^              ╭──────────┐
        ^_<down>_^                  ^_C-<down>_^           quit : [_SPC_]
"
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("h" split-window-below)
  ("v" split-window-right)
  ("C-<up>" hydra-move-splitter-up) 
  ("C-<down>" hydra-move-splitter-down)
  ("C-<left>" hydra-move-splitter-left)
  ("C-<right>" hydra-move-splitter-right)
  ("SPC" nil))

;; hydra unicode -------------------------------------------

(defhydra hydra-unicode (:color blue :hint nil)
  "
                                                                         ╭─────────┐
                                                                         │ Unicode │
 ╭───────────────────────────────────────────────────────────────────────┴─────────╯
  Punctuation:  • [_b_] ★ [_s_] · [_._] — [_-_] ° [_d_]
  Typography :  § [_S_] ¶ [_p_] ※ [_r_] † [_+_] ‡ [_=_] № [_n_] ⁂ [_*_] ╭───────────────────────┐
  Germanic   :  Ð [_E_] ð [_e_] þ [_T_] Þ [_t_]                    Japan: 〇 [_0_] ₸ [_$_]
  Money      :  ¢ [_1_] € [_2_] ฿ [_3_] £ [_4_] ￥ [_5_]              Marks: ® [_6_] ™ [_7_] © [_8_]
 ╭──────────────────────────────────────────────────────┐
  Arrows    :  ← [_<left>_] → [_<right>_] ↑ [_<up>_] ↓ [_<down>_]
"
  ;; Japan
  ("0" (lambda () (interactive) (insert "〇"))) ;; zero
  ("$" (lambda () (interactive) (insert "₸")))  ;; postal mark
  ;; Typoggrphy
  ("b" (lambda () (interactive) (insert "•"))) ;; bullet
  ("+" (lambda () (interactive) (insert "†"))) ;; dagger
  ("=" (lambda () (interactive) (insert "‡"))) ;; double dagger
  ("d" (lambda () (interactive) (insert "°"))) ;; degree
  ("." (lambda () (interactive) (insert "·"))) ;; mid dot
  ("-" (lambda () (interactive) (insert "—"))) ;; em dash
  ("S" (lambda () (interactive) (insert "§"))) ;; Section
  ("s" (lambda () (interactive) (insert "★"))) ;; star
  ("n" (lambda () (interactive) (insert "№"))) ;; numero
  ("*" (lambda () (interactive) (insert "⁂"))) ;; asterism (centered text break)  
  ("p" (lambda () (interactive) (insert "¶"))) ;; pilcrow (paragraph)
  ("r" (lambda () (interactive) (insert "※"))) ;; note
  ;; Germanic 
  ("E" (lambda () (interactive) (insert "Ð"))) ;; eth Uppercase
  ("e" (lambda () (interactive) (insert "ð"))) ;; eth
  ("t" (lambda () (interactive) (insert "Þ"))) ;; thorn
  ("T" (lambda () (interactive) (insert "þ"))) ;; thorn uppercase
  ;; Arrows
  ("<left>"  (lambda () (interactive) (insert "←"))) ;; left arrow
  ("<right>" (lambda () (interactive) (insert "→"))) ;; right arrow
  ("<up>"    (lambda () (interactive) (insert "↑"))) ;; up arrow
  ("<down>"  (lambda () (interactive) (insert "↓"))) ;; down arrow
  ;; Money
  ("1" (lambda () (interactive) (insert "¢"))) ;; cent
  ("2" (lambda () (interactive) (insert "€"))) ;; euro
  ("3" (lambda () (interactive) (insert "฿"))) ;; baht
  ("4" (lambda () (interactive) (insert "£"))) ;; pound
  ("5" (lambda () (interactive) (insert "￥"))) ;; yen
  ;; Marks
  ("6" (lambda () (interactive) (insert "®"))) ;; registered trademark
  ("7" (lambda () (interactive) (insert "™"))) ;; trademark
  ("8" (lambda () (interactive) (insert "©"))) ;; copyright
  ("<SPC>" nil nil))

;; calendar & weekday hydra --------------------------------

;; This hydra is designed for entering a Hanzi/Kanji
;; date string within the hydra.  To make this work we
;; explicitly set numbers in the hydra so that numbers
;; are not treated as prefix arguments -- this might be
;; something that helm is doing not hydra....

(defhydra hydra-calendar (:color pink :hint nil)
  "
                                                                ╭──────────┐
                                                                │ Calendar │
 ╭──────────────────────────────────────────────────────────────┴──────────╯
  Calendar  :  年 [_Y_] 月 [_M_] 日 [_D_]
  Weekdays  :  (日) [_s_] (月) [_m_] (火) [_f_] (水) [_w_] (木) [_t_] (金) [_g_] (土) [_e_]
"
  ;; Calendar
  ("Y" (lambda () (interactive) (insert "年"))) ;; year
  ("M" (lambda () (interactive) (insert "月"))) ;; moon/month
  ("D" (lambda () (interactive) (insert "日"))) ;; day/sun
  ;; Weekdays
  ("s" (lambda () (interactive) (insert " (日)"))) ;; sunday sun
  ("m" (lambda () (interactive) (insert " (月)"))) ;; monday moon
  ("f" (lambda () (interactive) (insert " (火)"))) ;; tuesday fire
  ("w" (lambda () (interactive) (insert " (水)"))) ;; wednesday water
  ("t" (lambda () (interactive) (insert " (木)"))) ;; thursday wood/tree
  ("g" (lambda () (interactive) (insert " (金)"))) ;; friday gold (payday!)
  ("e" (lambda () (interactive) (insert " (土)"))) ;; saturday earth
  ("0" (lambda () (interactive) (insert "0")))    ;;
  ("1" (lambda () (interactive) (insert "1")))    ;;
  ("2" (lambda () (interactive) (insert "2")))    ;;
  ("3" (lambda () (interactive) (insert "3")))    ;;  set numbers explicitly
  ("4" (lambda () (interactive) (insert "4")))    ;;  so that we can write 
  ("5" (lambda () (interactive) (insert "5")))    ;;  whole date inside hydra
  ("6" (lambda () (interactive) (insert "6")))    ;;
  ("7" (lambda () (interactive) (insert "7")))    ;;
  ("8" (lambda () (interactive) (insert "8")))    ;;
  ("9" (lambda () (interactive) (insert "9")))    ;;
  ("<SPC>" nil nil))

;; hydra greek ---------------------------------------------

(defhydra hydra-greek (:color pink :hint nil)
  "
                                                                 ╭───────┐
                                                                 │ Greek │
 ╭───────────────────────────────────────────────────────────────┴───────╯
  α [_a_] β [_b_] Y [_g_] δ [_d_] ε [_e_] ζ [_z_] η [_h_] θ [_q_] ι [_i_] κ [_k_] λ [_l_] μ [_m_]
  ν [_n_] ξ [_x_] ο [_o_] π [_p_] ρ [_r_] σ [_s_] τ [_t_] υ [_u_] φ [_j_] χ [_c_] ψ [_y_] ω [_w_]

  Α [_A_] Β [_B_] Γ [_G_] Δ [_D_] Ε [_E_] Ζ [_Z_] Η [_H_] Θ [_Q_] Ι [_I_] Κ [_K_] Λ [_l_] Μ [_M_]
  Ν [_N_] Ξ [_X_] Ο [_O_] Π [_P_] Ρ [_R_] Σ [_S_] Τ [_T_] Υ [_U_] Φ [_J_] Χ [_C_] Ψ [_Y_] Ω [_W_]
 ╭────────────┐
  Quit  [_<ESC>_]
"
  ("a" (lambda () (interactive) (insert "α")))
  ("b" (lambda () (interactive) (insert "β")))
  ("g" (lambda () (interactive) (insert "γ")))
  ("d" (lambda () (interactive) (insert "δ")))
  ("e" (lambda () (interactive) (insert "ε")))
  ("z" (lambda () (interactive) (insert "ζ")))
  ("h" (lambda () (interactive) (insert "η")))
  ("q" (lambda () (interactive) (insert "θ")))
  ("i" (lambda () (interactive) (insert "ι")))
  ("k" (lambda () (interactive) (insert "κ")))
  ("l" (lambda () (interactive) (insert "λ")))
  ("m" (lambda () (interactive) (insert "μ")))
  ("n" (lambda () (interactive) (insert "ν")))
  ("x" (lambda () (interactive) (insert "ξ")))
  ("o" (lambda () (interactive) (insert "ο")))
  ("p" (lambda () (interactive) (insert "π")))
  ("r" (lambda () (interactive) (insert "ρ")))
  ("s" (lambda () (interactive) (insert "σ")))
  ("t" (lambda () (interactive) (insert "τ")))
  ("u" (lambda () (interactive) (insert "υ")))
  ("f" (lambda () (interactive) (insert "ϕ")))
  ("j" (lambda () (interactive) (insert "φ")))
  ("c" (lambda () (interactive) (insert "χ")))
  ("y" (lambda () (interactive) (insert "ψ")))
  ("w" (lambda () (interactive) (insert "ω")))
  ("A" (lambda () (interactive) (insert "Α")))
  ("B" (lambda () (interactive) (insert "Β")))
  ("G" (lambda () (interactive) (insert "Γ")))
  ("D" (lambda () (interactive) (insert "Δ")))
  ("E" (lambda () (interactive) (insert "Ε")))
  ("Z" (lambda () (interactive) (insert "Ζ")))
  ("H" (lambda () (interactive) (insert "Η")))
  ("Q" (lambda () (interactive) (insert "Θ")))
  ("I" (lambda () (interactive) (insert "Ι")))
  ("K" (lambda () (interactive) (insert "Κ")))
  ("L" (lambda () (interactive) (insert "Λ")))
  ("M" (lambda () (interactive) (insert "Μ")))
  ("N" (lambda () (interactive) (insert "Ν")))
  ("X" (lambda () (interactive) (insert "Ξ")))
  ("O" (lambda () (interactive) (insert "Ο")))
  ("P" (lambda () (interactive) (insert "Π")))
  ("R" (lambda () (interactive) (insert "Ρ")))
  ("S" (lambda () (interactive) (insert "Σ")))
  ("T" (lambda () (interactive) (insert "Τ")))
  ("U" (lambda () (interactive) (insert "Υ")))
  ("F" (lambda () (interactive) (insert "Φ")))
  ("J" (lambda () (interactive) (insert "Φ")))
  ("C" (lambda () (interactive) (insert "Χ")))
  ("Y" (lambda () (interactive) (insert "Ψ")))
  ("W" (lambda () (interactive) (insert "Ω")))
  ("<SPC>" (lambda () (interactive) (insert " ")))
  ("<ESC>" nil nil))

;; hydra math & logic --------------------------------------

(defhydra hydra-logic (:color blue :hint nil :timeout 30)
  "
                                                                 ╭───────┐
                                                                 │ Math  │
  ╭──────────────────────────────────────────────────────────────┴───────╯
   ≈ [_1_] ≡ [_2_] ≠ [_3_] ∞ [_4_] × [_5_] ± [_6_] ∓ [_7_] ÷ [_8_] √ [_9_]
                                                                 ╭───────┐
                                                                 │ Logic │
  ╭──────────────────────────────────────────────────────────────┴───────╯
   ∀ [_a_] ∁ [_b_] ∃ [_c_] ∄ [_d_] ∅ [_e_] ¬ [_f_] ∧ [_g_] ∨ [_h_] ∩ [_i_] ∪ [_j_] ∈ [_k_] ∉ [_l_]
   ∋ [_m_] ∌ [_n_] ⊂ [_o_] ⊃ [_p_] ⊄ [_q_] ⊅ [_r_] ⊆ [_s_] ⊇ [_t_] ⊈ [_u_] ⊉ [_v_] ⋄ [_w_]
"
  ;; Math
  ("1" (lambda () (interactive) (insert "≈")))
  ("2" (lambda () (interactive) (insert "≡")))
  ("3" (lambda () (interactive) (insert "≠")))
  ("4" (lambda () (interactive) (insert "∞")))
  ("5" (lambda () (interactive) (insert "×")))
  ("6" (lambda () (interactive) (insert "±")))
  ("7" (lambda () (interactive) (insert "∓")))
  ("8" (lambda () (interactive) (insert "÷")))
  ("9" (lambda () (interactive) (insert "√")))
  ;; Logic
  ("a" (lambda () (interactive) (insert "∀")))
  ("b" (lambda () (interactive) (insert "∁")))
  ("c" (lambda () (interactive) (insert "∃")))
  ("d" (lambda () (interactive) (insert "∄")))
  ("e" (lambda () (interactive) (insert "∅")))
  ("f" (lambda () (interactive) (insert "¬")))
  ("g" (lambda () (interactive) (insert "∧")))
  ("h" (lambda () (interactive) (insert "∨")))
  ("i" (lambda () (interactive) (insert "∩")))
  ("j" (lambda () (interactive) (insert "∪")))
  ("k" (lambda () (interactive) (insert "∈")))
  ("l" (lambda () (interactive) (insert "∉")))
  ("m" (lambda () (interactive) (insert "∋")))
  ("n" (lambda () (interactive) (insert "∌")))
  ("o" (lambda () (interactive) (insert "⊂")))
  ("p" (lambda () (interactive) (insert "⊃")))
  ("q" (lambda () (interactive) (insert "⊄")))
  ("r" (lambda () (interactive) (insert "⊅")))
  ("s" (lambda () (interactive) (insert "⊆")))
  ("t" (lambda () (interactive) (insert "⊇")))
  ("u" (lambda () (interactive) (insert "⊈")))
  ("v" (lambda () (interactive) (insert "⊉")))
  ("w" (lambda () (interactive) (insert "⋄")))
  ("<SPC>" nil nil))

;; hydra twittering ----------------------------------------

(defhydra hydra-twittering (:color blue :hint nil  :timeout 10)
        "
                                                                    ╭────────────┐
     Tweets                User                        Timeline     │ Twittering │
  ╭─────────────────────────────────────────────────────────────────┴────────────╯
    _k_  [_t_] post tweet      _p_  [_f_] follow                  ^_g_^      [_u_] update
    ^↑^  [_X_] delete tweet    ^↑^  [_F_] unfollow              ^_S-SPC_^    [_._] new
    ^ ^  [_r_] retweet         ^ ^  [_d_] direct message          ^^↑^^      [^@^] current user
    ^↓^  [_R_] retweet & edit  ^↓^  [_i_] profile (browser)   _h_ ←   → _l_  [_a_] toggle
    _j_  [_b_] favorite        _n_   ^ ^                          ^^↓^^
    ^ ^  [_B_] unfavorite      ^ ^   ^ ^                         ^_SPC_^
    ^ ^  [_RET_] reply         ^ ^   ^ ^                          ^_G_^
    ^ ^  [_T_] show Thread
    ^ ^  [_y_] yank url          Items                     Do
    ^ ^  [_Y_] yank tweet     ╭───────────────────────────────────────────────────────
    ^ ^  [_e_] edit mode        _<backtab>_ ← _o_pen → _<tab>_    [_q_] exit
    ^ ^   ^ ^                   ^         ^   ^ ^      ^     ^    [_/_] search
  --------------------------------------------------------------------------------
       "
       ("\\" hydra-master/body "back")
       ("<ESC>" nil "quit")
       ("q"          twittering-kill-buffer)
       ("e"          twittering-edit-mode)
       ("j"          twittering-goto-next-status :color red)
       ("k"          twittering-goto-previous-status :color red)
       ("h"          twittering-switch-to-next-timeline :color red)
       ("l"          twittering-switch-to-previous-timeline :color red)
       ("g"          beginning-of-buffer)
       ("G"          end-of-buffer)
       ("t"          twittering-update-status-interactive)
       ("X"          twittering-delete-status)
       ("RET"        twittering-reply-to-user)
       ("r"          twittering-native-retweet)
       ("R"          twittering-organic-retweet)
       ("d"          twittering-direct-message)
       ("u"          twittering-current-timeline)
       ("b"          twittering-favorite)
       ("B"          twittering-unfavorite)
       ("f"          twittering-follow)
       ("F"          twittering-unfollow)
       ("i"          twittering-view-user-page)
       ("/"          twittering-search)
       ("."          twittering-visit-timeline)
       ("@"          twittering-other-user-timeline)
       ("T"          twittering-toggle-or-retrieve-replied-statuses)
       ("o"          twittering-click)
       ("<tab>"        twittering-goto-next-thing :color red)
       ("<backtab>"  twittering-goto-previous-thing :color red)
       ("n"          twittering-goto-next-status-of-user :color red)
       ("p"          twittering-goto-previous-status-of-user :color red)
       ("SPC"        twittering-scroll-up :color red)
       ("S-SPC"      twittering-scroll-down :color red)
       ("y"          twittering-push-uri-onto-kill-ring)
       ("Y"          twittering-push-tweet-onto-kill-ring)
       ("a"          twittering-toggle-activate-buffer))

;; hydra rectangle -----------------------------------------
;;
;; rectangle editing is very cool, but it's one of those
;; things you use seldom enough that you can't remember
;; the commands.  The hydra makes all of that go away....
;
;; See: http://oremacs.com/2015/02/25/rectangle-hydra/
;;      http://cestlaz.github.io/posts/using-emacs-27-rectangles/#.WJ542LMxVpi

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark)
			   :timeout 30)
  "
  ^_k_^     _d_elete    _s_tring     |\\     ‗,,,--,,‗
_h_   _l_   _o_k        _y_ank       /,`.-'`'   .‗  \-;;,‗
  ^_j_^     _n_ew-copy  _r_eset     |,4-  ) )‗   .;.(  `'-'
^^^^        _e_xchange  _u_ndo     '---''(‗/.‗)-'(‗\‗)
^^^^        ^ ^         _p_aste
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("e" ora-ex-point-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

;; hydra join lines ----------------------------------------

(defhydra hydra-join-lines (:timeout 5)
  ("<up>" join-line)
  ("<down>" (join-line 1))
  ("t" join-line)
  ("n" (join-line 1))
  ("<SPC>" nil nil))

;; hydra transpose ------------------------------------------

(defhydra hydra-transpose (:color red  :timeout 5)
    "Transpose"
    ("c" transpose-chars "characters")
    ("w" transpose-words "words")
    ("o" org-transpose-words "Org mode words")
    ("l" transpose-lines "lines")
    ("s" transpose-sentences "sentences")
    ("e" org-transpose-elements "Org mode elements")
    ("p" transpose-paragraphs "paragraphs")
    ("t" org-table-transpose-table-at-point "Org mode table")
    ("q" nil "cancel" :color blue))


(defun ora-ex-point-mark ()
  (interactive)
  (if rectangle-mark-mode
      (exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))
