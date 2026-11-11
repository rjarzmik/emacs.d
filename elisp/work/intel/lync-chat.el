;;; lync-chat.el --- Configuration Emacs -*- lexical-binding: t; elisp-autofmt-load-packages-local: ("use-package" "use-package-core"); -*-
(require 'pidgin)

;;set name of existing directory for store history
(setq pidgin-messenger-directory "~/.pigin_log_directory")

(pidgin-connect)
(provide 'intel-chat)
