;;; Robert Jarzmik
;;; 2008-03-28
;;; Global keys bindings

;; Keyboard redefinitions
(global-set-key [backspace] 'backward-delete-char)
(global-set-key [delete] 'delete-char)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key "\C-c\C-c" 'compile)
(global-set-key [\C-\S-iso-lefttab] 'other-window)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [\M-g] 'goto-line)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [home]     'beginning-of-line)
(global-set-key [end]      'end-of-line)
(global-set-key [\M-left]  'backward-sentence)
(global-set-key [\M-right] 'forward-sentence)
(global-set-key [\M-up]    'beginning-of-buffer)
(global-set-key [\M-down]  'end-of-buffer)
; Touches de fonctions
; F1 : lance l'apropos sur le mot ou se trouve le curseur
(global-set-key [f1]     'vectra-apropos-on-word)
;; Ctrl+F1 : lance man sur le mot ou se trouve le curseur
(global-set-key [\C-f1]  'vectra-man-on-word)
;; F2 : coupe le buffer en 2 verticalement
(global-set-key [f2]     'split-window-vertically)
;; Ctrl+F2 : coupe le buffer en 2 horizontalement
(global-set-key [\C-f2]  'split-window-horizontally)
;; F3 : non affecté
;; F4 : commente la ligne courante  suivant le mode choisi (C++,C,...)
(global-set-key [f4]     'vectra-comment-region)
;; Ctrl + F4 : decommente la ligne courante  suivant le mode choisi (C++,C,...)
;; (global-set-key [\C-f4]  'vectra-uncomment-region)
;; F5 : la complétion
(global-set-key [f5]     'dabbrev-expand)
;; F6 : lance ispell
(global-set-key [f6]     'ispell-buffer)
(global-set-key [\C-f6]  'ispell-word)
(global-set-key [\M-f6]  'ispell-region)
;; F7 : indente correctement tout le buffer courant
(global-set-key [f7]     'vectra-indent-hilit-buffer)
;; F9 : lance la compilation
(global-set-key [f9]     'compile)
;; F10 : va à l erreur suivante
(global-set-key [f10]    'next-error)
;; F11 : lance la speedbar
(global-set-key [f11]    'speedbar)
;; Ctrl+F4 : tue le buffer courant
(global-set-key [\C-f4]    'kill-buffer)
