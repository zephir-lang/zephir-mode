;;; test-zephir-mode-utils.el --- Zephir Mode: Utils tests -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.6.0
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

;; Define test-suites to test `zephir-mode' utils using `buttercup'.

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

;;; Code:

(describe "Positioning"
  (describe "create regexp for function"
    (it "finds functions w/o ‘visibility’"
      (with-zephir-buffer
       '("/** Doc comment */"
         "public function __construct(string! name, array! definition)"
         "{<*>}")
       (re-search-backward (zephir-create-regexp-for-function))
       (expect (point) :to-be 20)))

    (it "finds functions ‘public static’ methods"
      (with-zephir-buffer
       '("/** Doc comment */"
         "public static fn $fetch()"
         "{<*>}")
       (re-search-backward (zephir-create-regexp-for-function "public"))
       (expect (point) :to-be 20)))

    (it "finds functions ‘public’ methods"
      (with-zephir-buffer
       '("/** Doc comment */"
         "public function $fetch()"
         "{<*>}")
       (re-search-backward (zephir-create-regexp-for-function "public"))
       (expect (point) :to-be 20)))

    (it "finds functions ‘protected’ methods"
      (with-zephir-buffer
       '("/** Doc comment */"
         "protected fn test()"
         "{<*>}")
       (re-search-backward (zephir-create-regexp-for-function "protected"))
       (expect (point) :to-be 20)))

    (it "finds functions ‘private’ methods"
      (with-zephir-buffer
       '("/** Doc comment */"
         "private function __toString()"
         "{<*>}")
       (re-search-backward (zephir-create-regexp-for-function "private"))
       (expect (point) :to-be 20)))

    (it "finds functions ‘deprecated private’ methods"
      (with-zephir-buffer
       '("/** Doc comment */"
         "deprecated private function __toString()"
         "{<*>}")
       (re-search-backward (zephir-create-regexp-for-function "private"))
       (expect (point) :to-be 20)))

    (it "does not find abnormally formed functions"
      (with-zephir-buffer
       '("/** Doc comment */"
         "private function toString"
         "{}")
       (re-search-forward (zephir-create-regexp-for-function) nil t)
       (expect (point) :to-be 1))))

  (describe "create regexp for ‘classlike’"
    (it "finds ‘namespace’"
      (with-zephir-buffer
       '("// some comment here"
         "namespace Acme;"
         "class DI {}<*>")
       (re-search-backward (zephir-create-regexp-for-classlike "namespace"))
       (expect (point) :to-be 22)))

    (it "finds ‘class’ usind regexp w/o type"
      (with-zephir-buffer
       '("// some comment here"
         "class Service {<*>}")
       (re-search-backward (zephir-create-regexp-for-classlike))
       (expect (point) :to-be 22)))

    (it "finds ‘interface’"
      (with-zephir-buffer
       '("interface CommonInterface extends BaseInterface"
         "{"
         "}")
       (re-search-forward (zephir-create-regexp-for-classlike "interface"))
       (expect (point) :to-be 26)))

    (it "finds import aliases"
      (with-zephir-buffer
       "use Foo as Bar;"
       (re-search-forward (zephir-create-regexp-for-classlike "as"))
       (expect (point) :to-be 15)))

    (it "finds ‘implements’"
      (with-zephir-buffer
       "class A implements B {}"
       (re-search-forward (zephir-create-regexp-for-classlike "implements"))
       (expect (point) :to-be 21)))

    (it "finds ‘extends’"
      (with-zephir-buffer
       '("namespace Acme;"
         "class Service extends \\Acme\\Services\\Base"
         "{"
         "    public function __construct() {}"
         "}")
       (re-search-forward (zephir-create-regexp-for-classlike "extends"))
       (expect (point) :to-be 58))))

  (describe "zephir-in-listlike"
    (it "determines the position of the openning ‘[’"
      (with-zephir-buffer
       '("let myArray = [" "<*>" "]")
       (expect (zephir-in-listlike "\\[") :to-be 15)))

    (it "operates with multi-dimensional arrays"
      (with-zephir-buffer
       '("let config = ["
         "    \"foo\" : bar,"
         "    \"baz\" : [ 1 ],"
         "    ["
         "       1 : 2"
         "    ]"
         "<*>];")
       (expect (zephir-in-listlike "\\[") :to-be 14)))

    (it "returns nil, if point is not in an array"
      (with-zephir-buffer
       "public function <*>foo() {}"
       (expect (zephir-in-listlike "\\[") :to-be nil)))))

;;; test-zephir-mode-utils.el ends here
