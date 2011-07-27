;; Copyright (C) 2010 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: Emacs jabber configuration

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; My jabber configuration
;;

(when (require 'jabber nil t)
  (setq jabber-account-list
	'(("robert.jarzmik@gmail.com" 
	   (:network-server . "talk.google.com")
	   (:connection-type . ssl))))
  (setq jabber-auto-reconnect nil)
  (jabber-roster-toggle-offline-display)
  (jabber-roster-toggle-binding-display)
  (setq jabber-chat-buffer-show-avatar nil)

  (defun my-jabber-chat-hook ()
    (ispell-change-dictionary "francais")
    (flyspell-mode))

  (add-hook 'jabber-chat-mode-hook 'my-jabber-chat-hook))

;; Jabber activity
(when (featurep 'activity)
  (defun my-jabber-activity ()
    (delete-other-windows)
    (jabber-connect-all)
    (switch-to-buffer "*-jabber-roster-*"))
  (defun my-jabber-buf-filter-p (buffer)
    (with-current-buffer buffer
      (string= (substring (symbol-name major-mode) 0 (length "jabber")) "jabber")))

  (add-to-list 'available-activities (make-activity :name "Jabber"
						    :open-hook 'my-jabber-activity
						    :buffer-filter-p 'my-jabber-buf-filter-p)))

(provide 'my-jabber)
