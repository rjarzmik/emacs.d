;; Copyright (C) 2010 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs EMMS configuration

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
;; My emacs EMMS configuration

(when (and (require 'emms-setup nil t)
	   (require 'emms-info-libtag nil t))
  (setq emms-source-file-default-directory "~/Music/"
	emms-info-asynchronously t
	later-do-interval 0.0001
	emms-info-functions '(emms-info-libtag)
	emms-mode-line-format " %s "
	emms-show-format "NP: %s"
	emms-history-file (concat user-emacs-directory "data/emms-history")
	emms-cache-file (concat user-emacs-directory "data/emms-cache"))
  (emms-devel)
  (emms-default-players))

(when (featurep 'activity)
  (defun emms-filter-activity (buf)
    (let ((bufmode (symbol-name major-mode)))
      (string-match "^emms-" bufmode)))

  (add-to-list 'available-activities
	       (make-activity :name "emms"
			      :open-hook (lambda ()
					   (delete-other-windows)
					   (emms-browser))
			      :buffer-filter-p 'emms-filter-activity)))

(provide 'my-emms)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
