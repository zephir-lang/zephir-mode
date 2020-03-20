;;; test-zephir-mode-font-lock.el --- Zephir Mode: Font locking tests -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018, 2019, 2020 Serghei Iakovlev

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.4.0
;; URL: https://github.com/zephir-lang/zephir-mode

;; This file is NOT part of GNU Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Define test-suites to test `zephir-mode' font locking using `buttercup'.

;;; Code:

(require 'buttercup)

(when (require 'undercover nil t)
  ;; Track coverage, but don't send to coverage serivice.  Save in parent
  ;; directory as undercover saves paths relative to the repository root.
  (undercover "*.el" "test/utils.el"
              (:report-file "coverage-final.json")
              (:send-report nil)))

(let* ((current-dir (file-name-directory (or load-file-name (buffer-file-name)
                                             default-directory))))
  (load (concat current-dir "utils.el") nil 'nomessage 'nosuffix))


;;;; Tests

(describe "Classlike fontification"
  (it "fontify class"
    (with-zephir-buffer
     "use \\Foo\\Bar as Baz;
      class A extends B implements C {}"
     ;; “use”
     (should (eq (zephir-get-face-at 1) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 3) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 4))

     ;; “\Foo\Bar”
     (should (eq (zephir-get-face-at 5) 'font-lock-type-face))
     (should (eq (zephir-get-face-at 12) 'font-lock-type-face))
     (should-not (zephir-get-face-at 13))

     ;; “as”
     (should (eq (zephir-get-face-at 14) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 15) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 16))

     ;; “Baz”
     (should (eq (zephir-get-face-at 17) 'font-lock-type-face))
     (should (eq (zephir-get-face-at 19) 'font-lock-type-face))
     (should-not (zephir-get-face-at 20))

     ;; “class”
     (should (eq (zephir-get-face-at 28) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 32) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 33))

     ;; “A”
     (should (eq (zephir-get-face-at 34) 'font-lock-type-face))
     (should-not (zephir-get-face-at 35))

     ;; “extends”
     (should (eq (zephir-get-face-at 36) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 42) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 43))

     ;; “B”
     (should (eq (zephir-get-face-at 44) 'font-lock-type-face))
     (should-not (zephir-get-face-at 45))

     ;; “implements”
     (should (eq (zephir-get-face-at 46) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 55) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 56))

     ;; “C”
     (should (eq (zephir-get-face-at 57) 'font-lock-type-face))
     (should-not (zephir-get-face-at 58))))

  (it "fontify namespace"
    (with-zephir-buffer
     "namespace Foo;
      class Bar {}"
     ;; “namespace”
     (should (eq (zephir-get-face-at 1) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 9) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 10))

     ;; “class”
     (should (eq (zephir-get-face-at 22) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 26) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 27))

     ;; “Bar”
     (should (eq (zephir-get-face-at 28) 'font-lock-type-face))
     (should (eq (zephir-get-face-at 30) 'font-lock-type-face))
     (should-not (zephir-get-face-at 31))))

  (it "fontify interface"
    (with-zephir-buffer
     "namespace Phalcon\\Url;
      interface UrlInterface {}"
     ;; “namespace”
     (should (eq (zephir-get-face-at 1) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 9) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 10))

     ;; “Phalcon\Url”
     (should (eq (zephir-get-face-at 11) 'font-lock-type-face))
     (should (eq (zephir-get-face-at 21) 'font-lock-type-face))
     (should-not (zephir-get-face-at 22))

     ;; “interface”
     (should (eq (zephir-get-face-at 31) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 38) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 39))

     ;; “UrlInterface”
     (should (eq (zephir-get-face-at 40) 'font-lock-type-face))
     (should (eq (zephir-get-face-at 51) 'font-lock-type-face))
     (should-not (zephir-get-face-at 52))))

  (it "fontify class modifiers"
    (with-zephir-buffer
     "abstract final class FooBar {}"
     ;; “abstract”
     (should (eq (zephir-get-face-at 1) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 8) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 9))

     ;; “final”
     (should (eq (zephir-get-face-at 10) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 14) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 15))

     ;; “class”
     (should (eq (zephir-get-face-at 16) 'font-lock-keyword-face))
     (should (eq (zephir-get-face-at 20) 'font-lock-keyword-face))
     (should-not (zephir-get-face-at 21))

     ;; “FooBar”
     (should (eq (zephir-get-face-at 22) 'font-lock-type-face))
     (should (eq (zephir-get-face-at 27) 'font-lock-type-face))
     (should-not (zephir-get-face-at 28)))))

;;; test-zephir-mode-font-lock.el ends here
