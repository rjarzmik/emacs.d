;; Create a mode to colorize Icom logs
;(setq Icom-seeked-fpa "Digitatron_EVP_1c47 STPV")
(define-generic-mode 'icom-mode
   nil
   (list "ic_demander_cnx" "ic_accepter_cnx" "ic_attendre_evt" "ic_lire_evt" 
	 "ic_envoyer_sur_cnx" "ic_recevoir_sur_cnx")
   '(
     ("^\\(#.*$\\)" (1 font-lock-comment-face))
     ; Keywords
     ("^\\(is_identifier_agent\\|ic_\\(demander_cnx\\|accepter_cnx\\|attendre_evt\\|lire_evt\\|envoyer_sur_cnx\\|recevoir_sur_cnx\\)$"
      (1 font-lock-keyword-face))
     ; Keyword ends
;     ("^\\(is_identifier_agent\\|ic_\\(demander_cnx\\|accepter_cnx\\|attendre_evt\\|lire_evt\\|envoyer_sur_cnx\\|recevoir_sur_cnx\\) \\(IC_.*\\)$"
;      (1 font-lock-keyword-face)
;      (2 font-lock-variable-name-face))

;     ("^%\\(is_identifier_agent\\)()\\(.*\\)\\(is_identifier_agent\\)() \\(IC_[^\\s]*\\)" 
;      (1 font-lock-keyword-face)
;      (3 font-lock-keyword-face)
;      (4 font-lock-variable-name-face))
     )
   (list "icom\\.prim\\'")
   nil
   "Generic mode to understand icom logs")
