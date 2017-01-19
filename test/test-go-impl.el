;;; test-go-impl.el --- Test for go-impl

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'go-impl)

(ert-deftest completing-function ()
  "go-impl--completing-function"
  (let ((got (go-impl--completing-function (go-packages) "i" nil t)))
    (should (member "io" got))
    (should (member "ioutil" got))))

(ert-deftest matched-packages ()
  "go-impl--matched-packages"
  (let ((got (go-impl--matched-packages (go-packages) "json")))
    (should got)
    (should (member "encoding/json" got))))

(ert-deftest collect-interface ()
  "go-impl--collect-interface"
  (let ((got (go-impl--collect-interface "encoding/json")))
    (should (member "json.Unmarshaler" got))
    (should (member "json.Marshaler" got))))

(ert-deftest real-package-name ()
  "go-impl--real-package-name"
  (should (string= (go-impl--real-package-name "cmd/internal/goobj") "goobj"))
  (should (string= (go-impl--real-package-name "go-colortext") "colortext")))

;;; test-go-impl.el ends here
