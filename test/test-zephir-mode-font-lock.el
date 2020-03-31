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

(describe "Fontification of comments"
  (it "fontifies C-style comments"
    (expect "// Some text"
            :to-be-fontified-as
            '(("// " comment-delimiter "Some text" comment))))

  (it "fontifies C++-style comments"
    (expect "/* Some text */"
            :to-be-fontified-as
            '(("/* " comment-delimiter "Some text */" comment))))

  (it "fontifies docblocks"
    (expect "/** Some text */"
            :to-be-fontified-as
            '(("/** Some text */" doc)))))

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
  (it "fontifies constant definition"
    (expect "const FOO = 42;"
            :to-be-fontified-as
            '(("const" zephir-keyword "FOO" zephir-constant-assign)))

    (expect "const &a = 13;"
            :to-be-fontified-as
            '(("const" zephir-keyword "a" zephir-constant-assign))))

  (it "fontifies built-in constants"
    (expect "__LINE__ __FILE__ __FUNCTION__"
            :to-be-fontified-as
            '(("__LINE__" builtin "__FILE__" builtin "__FUNCTION__" builtin)))

    (expect "__CLASS__ __METHOD__ __NAMESPACE__"
            :to-be-fontified-as
            '(("__CLASS__" builtin "__METHOD__" builtin "__NAMESPACE__" builtin))))

  (it "fontifies regular form of constants"
    (expect "self::HTML5; Logger::ALERT;"
            :to-be-fontified-as
            '(("HTML5" constant "ALERT" constant)))))

(describe "Fontification variables"
  (it "fontifies variables"
    (expect "$compilationContext->classDefinition->classEntry"
            :to-be-fontified-as
            '(("->" zephir-object-operator "classDefinition" zephir-property-name
               "->" zephir-object-operator "classEntry" zephir-property-name))))

  (it "fontifies methods call"
    (expect "$compilationContext->classDefinition->get()"
            :to-be-fontified-as
            '(("->" zephir-object-operator "classDefinition" zephir-property-name
               "->" zephir-object-operator "get" zephir-method-call)))

    (expect "foo->var->bar()"
            :to-be-fontified-as
            '(("->" zephir-object-operator "var" zephir-property-name
               "->" zephir-object-operator "bar" zephir-method-call)))))

(describe "Fontification keywords"
  (it "fontifies ‘this’ keyword"
    (expect "this->foo = this;"
            :to-be-fontified-as
            '(("this" zephir-this "->" zephir-object-operator
               "foo" zephir-property-name "this" zephir-this)))

    (expect "$this->foo = $this;"
            :to-be-fontified-as
            '(("this" zephir-this-face "->" zephir-object-operator
               "foo" zephir-property-name "this" zephir-this-face))))

  (it "fontifies booleans and null"
    (expect "null, false, true"
            :to-be-fontified-as
            '(("null" constant "false" constant "true" constant))))

  (it "fontifies type hints"
    (expect "int foo; bool &bar, double $baz; istring &$buz"
            :to-be-fontified-as
            '(("int" zephir-type "foo" zephir-variable-name
               "bool" zephir-type "bar" zephir-variable-name
               "double" zephir-type "$baz" zephir-variable-name
               "istring" zephir-type "$buz" zephir-variable-name)))))

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
  (it "fontifies standard ‘function <name> ()’ headers"
    (expect "function foo () {}"
            :to-be-fontified-as
            '(("function" zephir-keyword-face "foo" zephir-function-name)))

    (expect "function $foo() {}"
            :to-be-fontified-as
            '(("function" zephir-keyword-face "$foo" zephir-function-name))))

  (it "fontifies standard ‘fn <name> ()’ headers"
    (expect "fn foo () {}"
            :to-be-fontified-as
            '(("fn" zephir-keyword-face "foo" zephir-function-name)))

    (expect "fn $foo() {}"
            :to-be-fontified-as
            '(("fn" zephir-keyword-face "$foo" zephir-function-name))))

  (it "fontifies standard ‘<visibility> function <name> ()’ headers"
    (expect "public function foo () {}"
            :to-be-fontified-as
            '(("public" keyword "function" zephir-keyword-face
               "foo" zephir-function-name)))

    (expect "protected function bar () {}"
            :to-be-fontified-as
            '(("protected" keyword "function" zephir-keyword-face
               "bar" zephir-function-name)))

    (expect "private function baz () {}"
            :to-be-fontified-as
            '(("private" keyword "function" zephir-keyword-face
               "baz" zephir-function-name)))

    (expect "internal function foo () {}"
            :to-be-fontified-as
            '(("internal" keyword "function" zephir-keyword-face
               "foo" zephir-function-name)))

    (expect "scoped function test () {}"
            :to-be-fontified-as
            '(("scoped" keyword "function" zephir-keyword-face
               "test" zephir-function-name)))

    (expect "internal function fuz () {}"
            :to-be-fontified-as
            '(("internal" keyword "function" zephir-keyword-face
               "fuz" zephir-function-name))))

  (it "fontifies return type hints headers"
    (expect "fn foo() -> array | int | <Foo> | <\\A\\B\\C> | void"
            :to-be-fontified-as
            '(("fn" zephir-keyword-face "foo" zephir-function-name
               "array" zephir-type "int" zephir-type "Foo" zephir-type
               "\\A\\B\\C" zephir-type "void" zephir-type)))

    (expect "fn foo() -> int
             | bool | istring"
            :to-be-fontified-as
            '(("fn" zephir-keyword-face "foo" zephir-function-name
               "int" zephir-type)
              ("bool" zephir-type "istring" zephir-type))))

  (it "fontifies parametrized function headers"
    (expect "function go (a, &b, $c, &$d) {}"
            :to-be-fontified-as
            '(("function" zephir-keyword-face "go" zephir-function-name
               "a" zephir-variable-name "b" zephir-variable-name
               "$c" zephir-variable-name "$d" zephir-variable-name)))

    (expect "fn test (int a, string b, bool c, <A> d, const <\\A\\B> &$c) {}"
            :to-be-fontified-as
            ;; FIXME: ‘"const" zephir-variable-name’ => keyword, not variable
            '(("fn" zephir-keyword-face "test" zephir-function-name
               "int" zephir-type "a" zephir-variable-name
               "string" zephir-type "b" zephir-variable-name
               "bool" zephir-type "c" zephir-variable-name
               "A" zephir-type "d" zephir-variable-name
               "const" zephir-variable-name "\\A\\B" zephir-type
               "$c" zephir-variable-name)))))

;;; test-zephir-mode-font-lock.el ends here
