;;; go-impl.el --- impl method stub generator for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-go-impl
;; Version: 0.01
;; Package-Requires: ((emacs "24") (go-mode "1.3.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Insert method stubs of interface.

;;; Code:

(defvar go-impl--receiver-history nil)
(defvar go-impl--interface-history nil)

(defun go-impl--execute (receiver interface)
  (with-temp-buffer
    (unless (zerop (process-file "impl" nil t nil receiver interface))
      (error "Failed: impl '%s' %s" receiver interface))
    (buffer-string)))

;;;###autoload
(defun go-impl (receiver interface)
  (interactive
   (list
    (read-string "Receiver: " nil 'go-impl--receiver-history)
    (read-string "Interface: " nil 'go-impl--interface-history)))
   (let ((stubs (go-impl--execute receiver interface)))
     (insert stubs)))

(provide 'go-impl)

;;; go-impl.el ends here
