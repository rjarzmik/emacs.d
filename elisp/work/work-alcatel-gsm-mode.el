;; Mode bouchons
(define-generic-mode 'bouchon-robert
   (list ?#)
   nil
   nil
   (list "\\.inj\\'")
   nil
   "Generic mode for Robert's bouchon.")

;; Mode fichiers de gms (.def)
(define-generic-mode 'gms-mode
   nil
   (list "VAR_INT" "VAR_EXT" "FUNC_EXT" "FUNC_INT" "HISTORIQUE"  "RETOUR" "PARAM"
	 "SOURCE" "INCLUDE" "PRIVATE" "LANGAGE" "MODULE" "NIVEAU" "TEST" "PROJET" 
	 "SYSTEME" "constant" "modifiable" "aucun")
   '(
     ("^\\(\\..*$\\)" (1 font-lock-comment-face))
     ("^%\\(FUNC_\\(INT\\|EXT\\)\\)\\s-*%\\s-*\\([^%]+\\)\\s-*%\\(\\(.*\\\\[\n\r]\\)*[^\n\r]*\\)" 
      (1 font-lock-keyword-face)
      (3 font-lock-function-name-face)
      (4 font-lock-comment-face))
     ("^%\\(HISTORIQUE\\)\\s-*%\\(\\(.*\\\\[\n\r]\\)*[^\n\r]*\\)" 
      (1 font-lock-keyword-face)
      (2 font-lock-comment-face))
     ("^%\\(VAR_\\(INT\\|EXT\\)\\)\\s-*%\\s-*\\([^%]+\\)\\s-*\\**\\s-*%\\s-*\\([^%]+\\)\\s-*%\\(\\(.*\\\\[\n\r]\\)*[^\n\r]*\\)" 
      (1 font-lock-keyword-face)
      (3 font-lock-type-face)
      (4 font-lock-variable-name-face)
      (5 font-lock-comment-face))
     ("^%\\(MODULE\\)\\s-*%\\s-*\\([^%]+\\)\\s-*%\\(\\(.*\\\\[\n\r]\\)*[^\n\r]*\\)" 
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face)
      (3 font-lock-comment-face))
     ("^%\\(PARAM\\)\\s-*%\\s-*\\(constant\\|modifiable\\)\\s-*%\\s-*\\([^%]+\\)\\s-*\\**\\s-*%\\s-*\\([^%]+\\)\\s-*%\\(\\(.*\\\\[\n\r]\\)*[^\n\r]*\\)" 
      (1 font-lock-keyword-face)
      (2 font-lock-keyword-face)
      (3 font-lock-type-face)
      (4 font-lock-variable-name-face)
      (5 font-lock-comment-face))
     ("^%\\(RETOUR\\)\\s-*%\\s-*\\([^%]+\\)\\s-*\\**\\s-*%\\s-*\\([^%]+\\)\\s-*%\\(\\(.*\\\\[\n\r]\\)*[^\n\r]*\\)" 
      (1 font-lock-keyword-face)
      (2 font-lock-type-face)
      (3 font-lock-variable-name-face)
      (4 font-lock-comment-face))
     )
   (list "\\.def\\'")
   nil
   "Mode generique pour les fichiers de definition gms")

