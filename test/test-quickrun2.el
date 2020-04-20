;;; test-quickrun2.el --- test for quickrun2

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'quickrun2)

(ert-deftest find-language-from-name ()
  "Find language information from file name"
  (let ((lang-source (quickrun2--find-language-source "foo.go")))
    (should (string= (plist-get lang-source :pattern) "\\.go\\'"))))

(ert-deftest find-language-from-major-mode ()
  "Find language information from major-mode"
  (with-temp-buffer
    (let* ((major-mode 'perl-mode)
           (lang-source (quickrun2--find-language-source "_no_match_file_name_")))
      (should (equal (plist-get lang-source :major-mode) '(perl-mode cperl-mode))))))

(ert-deftest fill-param ()
  "Fill language source parameter"
  (with-temp-buffer
    (let* ((lang-source (list :exec '(("foo" bar "baz" source)) :bar "apple"))
           (filled (quickrun2--fill-param :exec lang-source "foo.cpp")))
      (should (equal filled '(("foo" "apple" "baz" "foo.cpp")))))))

(ert-deftest fill-param-with-function ()
  "Fill param list with function parameter"
  (with-temp-buffer
    (let* ((lang-source (list :exec '(("foo" output "baz" source))
                              :output (lambda (src) (concat "hello/" src))))
           (filled (quickrun2--fill-param :exec lang-source "foo.cpp")))
      (should (equal filled '(("foo" "hello/foo.cpp" "baz" "foo.cpp")))))))

;; TODO
;; - add quickrun test
;; - add command test
;; - add overwrite command test

;;; test-quickrun2.el ends here
