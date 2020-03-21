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

(ert-deftest quickrun2:exec-quickrun2 ()
  "Exec `quickrun2'"
  (let ((buf (find-file-noselect "sample/sample.py")))
    (with-current-buffer buf
      (quickrun2))
    ;; quickrun2 is async function
    (sleep-for 1)
    (with-current-buffer "*quickrun2*"
      (let ((str (buffer-substring-no-properties (point-min) (point-max))))
        (should (string= "Hello Python quickrun2.el\n" str))))))

(ert-deftest quickrun2:add-command ()
  "Add new command"
  (quickrun2-add-command "-test"
                        '((:command . "test foo")
                          (:description . "test description")))
  (let ((params (assoc-default "-test" quickrun2--language-alist)))
    (should params)
    (let ((command (assoc-default :command params))
          (desc (assoc-default :description params)))
      (should (string= command "test foo"))
      (should (string= desc "test description")))))

(ert-deftest quickrun2:override-configuration ()
  "Override registerd command"
  (quickrun2-add-command "c/gcc"
                        '((:command . "clang")
                          (:description . "Compile clang"))
                        :override t)
  (let* ((params (assoc-default "c/gcc" quickrun2--language-alist))
         (command (assoc-default :command params)))
    (should (string= command "clang"))))

(ert-deftest quickrun2:use-tempfile-p ()
  "Whether use temporary file or not."
  (quickrun2-add-command "tempfile0" '((:command . "tempfile0") (:tempfile . t)))
  (let ((use-tempfile (quickrun2--use-tempfile-p "tempfile0")))
    (should use-tempfile))

  (quickrun2-add-command "tempfile1" '((:command . "tempfile1") (:tempfile . nil)))
  (let ((use-tempfile (quickrun2--use-tempfile-p "tempfile1")))
    (should-not use-tempfile))

  ;; use temporary file if :tempfile paramter is not specified
  (quickrun2-add-command "tempfile2" '((:command . "tempfile2")))
  (let ((use-tempfile (quickrun2--use-tempfile-p "tempfile2")))
    (should use-tempfile))

  (let* ((quickrun2--compile-only-flag t)
         (use-tempfile (quickrun2--use-tempfile-p "hoge")))
    (should-not use-tempfile)))

;;; test-quickrun2.el ends here
