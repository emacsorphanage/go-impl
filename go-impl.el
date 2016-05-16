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

(require 'go-mode)
(require 'cl-lib)

(defvar go-impl--interface-cache (make-hash-table :test #'equal))
(defvar go-impl--receiver-history nil)
(defvar go-impl--interface-history nil)

(defun go-impl--collect-interface (package)
  (with-temp-buffer
    (unless (zerop (process-file "godoc" nil t nil "-src" package))
      (error "Failed: 'godoc -src %s'" package))
    (goto-char (point-min))
    (cl-loop with re = "^type\\s-+\\(\\S-+\\)\\s-+interface"
             with real-package = (if (string-match "[/-]\\(.+\\)\\'" package)
                                     (match-string-no-properties 1 package)
                                   package)
             while (re-search-forward re nil t)
             collect (concat real-package "." (match-string-no-properties 1)) into interfaces
             finally return (progn
                              (puthash package (cl-copy-list interfaces) go-impl--interface-cache)
                              interfaces))))

(defun go-impl--collect-interfaces (packages)
  (cl-loop for package in packages
           if (gethash package go-impl--interface-cache)
           append it
           else
           append (go-impl--collect-interface package)))

(defun go-impl--matched-packages (packages pattern)
  (let ((package (if (string-match "\\." pattern)
                     (substring pattern 0 (match-beginning 0))
                   pattern)))
   (cl-loop with regexp = (concat package "\\'")
            for p in packages
            when (string-match-p regexp p)
            collect p)))

(defun go-impl--completing-function (packages string predicate code)
  (let* ((matched (go-impl--matched-packages packages string))
         (candidates (go-impl--collect-interfaces matched)))
    (if (not code)
        (try-completion string candidates predicate)
      (all-completions string candidates predicate))))

(defun go-impl--execute (receiver interface)
  (with-temp-buffer
    (unless (zerop (process-file "impl" nil t nil receiver interface))
      (error "Failed: impl '%s' %s" receiver interface))
    (buffer-string)))

;;;###autoload
(defun go-impl (receiver interface)
  (interactive
   (let* ((packages (go-packages))
          (comp-fn (lambda (input predicate code)
                     (go-impl--completing-function packages input predicate code))))
     (list
      (read-string "Receiver: " nil 'go-impl--receiver-history)
      (completing-read "Interface: " comp-fn nil nil nil 'go-impl--interface-history))))
  (let ((stubs (go-impl--execute receiver interface)))
    (insert stubs)))

(provide 'go-impl)

;;; go-impl.el ends here
