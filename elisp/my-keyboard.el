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


; Function keys
; F1 : lance l'apropos sur le mot ou se trouve le curseur
; Should be mode specific
;; F2 : coupe le buffer en 2 verticalement
(global-set-key [f2]     'split-window-vertically)
;; Ctrl+F2 : coupe le buffer en 2 horizontalement
(global-set-key [\C-f2]  'split-window-horizontally)
;; F3 : not used
;; F4 : not used
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

;; C-x b and C-f4 for killing buffers
(substitute-key-definition 'kill-buffer 'kill-this-buffer global-map)

;; Window moves
(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-l") 'windmove-left)
(global-set-key (kbd "C-M-m") 'windmove-right)

;; W3M enabled emacs : Google search and activity
(when (featurep 'w3m)
  (global-set-key (kbd "C-M-g") 'w3m-search-new-session))

;; Activity bindings

(when (featurep 'activity)
  (global-set-key (kbd "C-` `") 'activity-pop)
  (global-set-key (kbd "C-` d") (lambda () (interactive)
				  (toggle-activity "default")))
  (when (featurep 'w3m)
    (global-set-key (kbd "C-` w") (lambda () (interactive) (toggle-activity "Web"))))
  (when (featurep 'emms)
    (global-set-key (kbd "C-` e") (lambda () (interactive) (toggle-activity "emms")))))

(provide 'my-keyboard)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
