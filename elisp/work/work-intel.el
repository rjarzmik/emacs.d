;; Intel related stuff

(add-to-list 'load-path (concat user-emacs-directory "site-elisp/psi_pupdr_scripts-intel-lisp"))

(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var)))) 

;(require 'intel-debug-activity)
(require 'intel-chat "intel/lync-chat.el")

(provide 'work-intel)
