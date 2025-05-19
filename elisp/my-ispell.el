;;; Robert Jarzmik
;;; 2025-05-14
;;; Dictionary checks

(use-package ispell
  :ensure nil  ; `ispell` is built in in Emacs
  :config
  (setq ispell-program-name "hunspell")
  ;; Default dictionary
  (setq ispell-dictionary "en_US")
  ;; Add personnal dictionnary
  (setq ispell-personal-dictionary (locate-user-emacs-file "data/hunspell_personal"))
  ;; S'assurer que le fichier personnel existe
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

(provide 'my-ispell)
