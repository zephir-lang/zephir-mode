;;; test-zephir-mode-font-lock.el --- Zephir Mode: Font locking tests -*- lexical-binding: t; -*-

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

(describe "Fontification of classes"
  (it "fontifies classes"
    (expect "class A extends B implements C {}"
            :to-be-fontified-as
            '(("class" keyword "A" type "extends" keyword "B" type
               "implements" keyword "C" type))))

  (it "fontifies ‘use’ keyword"
    (expect "use Phalcon\\Url;"
            :to-be-fontified-as
            '(("use" keyword "Phalcon\\Url" type))))

  (it "fontifies ‘use .. as’ statement"
    (expect "use Phalcon\\Url as PhUrl;"
            :to-be-fontified-as
            '(("use" keyword "Phalcon\\Url" type "as" keyword "PhUrl" type))))

  (it "fontifies namespaces"
    (expect "namespace Phalcon\\Url;"
            :to-be-fontified-as
            '(("namespace" keyword "Phalcon\\Url" type))))

  (it "fontifies interfaces"
    (expect "interface UrlInterface {}"
            :to-be-fontified-as
            '(("interface" keyword "UrlInterface" type))))

  (it "fontifies class modifiers"
    (expect "final class Kernel {}"
            :to-be-fontified-as
            '(("final" keyword "class" keyword "Kernel" type)))

    (expect "abstract class Kernel {}"
            :to-be-fontified-as
            '(("abstract" keyword "class" keyword "Kernel" type)))))

(describe "Fontification constants"
  (it "fontifies built-in constants"
    (expect "__LINE__ __FILE__ __FUNCTION__"
            :to-be-fontified-as
            '(("__LINE__" builtin "__FILE__" builtin "__FUNCTION__" builtin)))

    (expect "__CLASS__ __METHOD__ __NAMESPACE__"
            :to-be-fontified-as
            '(("__CLASS__" builtin "__METHOD__" builtin "__NAMESPACE__" builtin))))

  (it "fontifies regular form of constants"
    (expect "self::HTML5; Logger::ALERT; const FOO = 5;"
            :to-be-fontified-as
            '(("HTML5" constant "ALERT" constant "FOO" constant)))))

(describe "Fontification keywords"
  (it "fontifies ‘this’ keyword"
    (expect "this->foo = this;"
            :to-be-fontified-as
            '(("this" constant "foo" variable-name "this" constant))))

  (it "fontifies booleans and null"
    (expect "null, false, true"
            :to-be-fontified-as
            '(("null" constant "false" constant "true" constant))))

  (it "fontifies variables"
    (expect "$compilationContext->classDefinition->classEntry"
            :to-be-fontified-as
            '(("classDefinition" variable-name "classEntry" variable-name))))

  (it "fontifies data types"
    (expect "int uint bool boolean float double long ulong
     char uchar string istring resource object var void array callable"
            :to-be-fontified-as
            '(("int" type "uint" type "bool" type "boolean" type "float" type
               "double" type "long" type "ulong" type)
              ("char" type "uchar" type "string" type "istring" type
               "resource" type "object" type "var" type "void" type
               "array" type "callable" type)))))

(describe "Fontification of visibility"
  (it "fontifies property visibility"
    (expect "internal foo;"
            :to-be-fontified-as
            '(("internal" keyword)))

    (expect "scoped bar;"
            :to-be-fontified-as
            '(("scoped" keyword)))

    (expect "inline baz;"
            :to-be-fontified-as
            '(("inline" keyword)))

    (expect "public bar;"
            :to-be-fontified-as
            '(("public" keyword)))

    (expect "protected foo;"
            :to-be-fontified-as
            '(("protected" keyword)))

    (expect "private bar;"
            :to-be-fontified-as
            '(("private" keyword)))

    (expect "private privateProperty;"
            :to-be-fontified-as
            '(("private" keyword)))))

(describe "Fontification of function headers"
  (it "fontifies standard <visibility> function <name> () headers"
    (expect "public function foo () {}"
            :to-be-fontified-as
            '(("public" keyword "function" keyword "foo" function-name)))

    (expect "protected function bar () {}"
            :to-be-fontified-as
            '(("protected" keyword "function" keyword "bar" function-name)))

    (expect "private function baz () {}"
            :to-be-fontified-as
            '(("private" keyword "function" keyword "baz" function-name)))

    (expect "internal function foo () {}"
            :to-be-fontified-as
            '(("internal" keyword "function" keyword "foo" function-name)))

    (expect "scoped function test () {}"
            :to-be-fontified-as
            '(("scoped" keyword "function" keyword "test" function-name)))

    (expect "internal function fuz () {}"
            :to-be-fontified-as
            '(("internal" keyword "function" keyword "fuz" function-name)))))

;;; test-zephir-mode-font-lock.el ends here
