;;; lync-chat.el --- Configuration Emacs -*- lexical-binding: t; -*-
(require 'pidgin)

;;set name of existing directory for store history
(setq pidgin-messenger-directory "~/.pigin_log_directory")

(pidgin-connect)
(provide 'intel-chat)
