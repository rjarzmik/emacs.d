;;; my-keyboard.el --- Configuration Emacs -*- lexical-binding: t; -*-
;;; Robert Jarzmik
;;; 2008-03-28
;;; Global keys bindings

;; Keyboard redefinitions

; Function keys
; F1 : lance l'apropos sur le mot ou se trouve le curseur
; Should be mode specific
;; F2 : not used
;; F3 : not used
;; F4 : not used
;; F5 : la complétion
(global-set-key [f5]     'dabbrev-expand)
;; F6 : lance ispell
(global-set-key [f6]     'ispell-buffer)
;; F7 : not used
;; F8 : not used
;; F9 : lance la compilation
(global-set-key [f9]     'my-compile)
;; F10 : va à l erreur suivante
(global-set-key [f10]    'next-error)
;; F11 : lance la speedbar
(global-set-key [f11]    'speedbar)
;; Ctrl+F4 : tue le buffer courant
(global-set-key [\C-f4]    'kill-buffer)

;; C-x b and C-f4 for killing buffers
(substitute-key-definition 'kill-buffer 'kill-current-buffer global-map)

;; Window moves
;; Don't use windmove anymore: (windmove-default-keybindings 'shift)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "C-c *" 'window-toggle-stickiness)

;; Buffer moves
(use-package iswitchb
  :bind ("C-<tab>" . iswitchb-buffer))

;; Activity bindings
(when (featurep 'activity)
  (keymap-global-set "C-` `" 'activity-pop)
  (keymap-global-set "C-` d" (lambda () (interactive) (toggle-activity "Default")))
  (keymap-global-set "C-` g" (lambda () (interactive) (toggle-activity "Gnus")))
  (keymap-global-set "C-` l" (lambda () (interactive) (toggle-activity "Linux"))))

;; Matches braces/parenthesis movement (vi like)
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))
(keymap-global-set "C-%" 'goto-match-paren)

;; Frame text font increase/descrease
(use-package zoom-frm
  :bind (:map ctl-x-map
	      ("C-+" . zoom-in/out)
	      ("C--" . zoom-in/out)))

(provide 'my-keyboard)
