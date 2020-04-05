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
            ;; TODO(serghei): ‘ */’ -> ‘comment-delimiter’
            '(("/* " comment-delimiter "Some text */" comment))))

  (it "fontifies docblocks"
    (expect "/** Some text */"
            :to-be-fontified-as
            '(("/** Some text */" doc)))))

(describe "Fontification of classes"
  (it "fontifies classes"
    (expect "class A extends B implements C {}"
            :to-be-fontified-as
            '(("class" zephir-class-declaration "A" type
               "extends" zephir-class-declaration-spec "B" type
               "implements" zephir-class-declaration-spec "C" type))))

  (it "fontifies ‘implement’ keyword"
    (expect "implements \\A, b,   C, \\D\\E\\F,
             foo;"
            :to-be-fontified-as
            '(("implements" zephir-class-declaration-spec
               "\\A" type "b" type "C" type "\\D\\E\\F" type)
              ("foo" type))))

  (it "fontifies ‘use’ keyword"
    (expect "use A\\B\\C;"
            :to-be-fontified-as
            '(("use" zephir-import-declaration "A\\B\\C" type)))

    (expect "use A"
            :to-be-fontified-as
            '(("use" zephir-import-declaration "A" type)))

    (expect "use \\Phalcon\\Url;"
            :to-be-fontified-as
            '(("use" zephir-import-declaration "\\Phalcon\\Url" type))))

  (it "fontifies ‘use .. as’ statement"
    (expect "use Phalcon\\Url as PhUrl;"
            :to-be-fontified-as
            '(("use" zephir-import-declaration "Phalcon\\Url" type "as"
               keyword "PhUrl" type))))

  (it "fontifies namespaces"
    (expect "namespace Phalcon\\Url;"
            :to-be-fontified-as
            '(("namespace" zephir-namespace-declaration "Phalcon\\Url" type))))

  (it "fontifies interfaces"
    (expect "interface UrlInterface {}"
            :to-be-fontified-as
            '(("interface" zephir-class-declaration "UrlInterface" type))))

  (it "fontifies class modifiers"
    (expect "final class Kernel {}"
            :to-be-fontified-as
            '(("final" zephir-class-modifier "class" zephir-class-declaration
               "Kernel" type)))

    (expect "abstract class Kernel {}"
            :to-be-fontified-as
            '(("abstract" zephir-class-modifier "class" zephir-class-declaration
               "Kernel" type)))))

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
            '(("__LINE__" zephir-magical-constant
               "__FILE__" zephir-magical-constant
               "__FUNCTION__" zephir-magical-constant)))

    (expect "__CLASS__ __METHOD__ __NAMESPACE__"
            :to-be-fontified-as
            '(("__CLASS__" zephir-magical-constant
               "__METHOD__" zephir-magical-constant
               "__NAMESPACE__" zephir-magical-constant))))

  (it "fontifies regular form of constants"
    (expect "self::HTML5; Logger::ALERT;"
            :to-be-fontified-as
            '(("HTML5" zephir-constant "ALERT" zephir-constant)))))

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
            '(("this" zephir-this "->" zephir-object-operator
               "foo" zephir-property-name "this" zephir-this))))

  (it "fontifies booleans and null"
    (expect "null, false, true"
            :to-be-fontified-as
            '(("null" zephir-constant "false" zephir-constant
               "true" zephir-constant))))

  (it "fontifies type hints"
    (expect "int foo; bool &bar, double $baz; istring &$buz"
            :to-be-fontified-as
            '(("int" zephir-type "foo" zephir-variable-name
               "bool" zephir-type "bar" zephir-variable-name
               "double" zephir-type "$baz" zephir-variable-name
               "istring" zephir-type "$buz" zephir-variable-name))))

  (it "fontifies type casts ‘<type> variable = ...’"
    (expect "<MyInterface> criteria = test();"
            :to-be-fontified-as
            '(("MyInterface" zephir-type "criteria" zephir-variable-name)))

    (expect "<MyInterface> a, <MyInterface> b"
            :to-be-fontified-as
            '(("MyInterface" zephir-type "a" zephir-variable-name
               "MyInterface" zephir-type "b" zephir-variable-name))))

  (it "doesn't fontify functions similar to keywords"
    (expect "get_called_class(this);"
            :to-be-fontified-as
            '(("this" zephir-this)))

    (expect "get_class(this);"
            :to-be-fontified-as
            '(("this" zephir-this)))

    (expect "let myclass = 42;"
            :to-be-fontified-as
            '(("let" zephir-keyword)))

    (expect " array_intersect(name, fieilds); "
            :to-be-fontified-as
            '(()))

    (expect " array_diff (); "
            :to-be-fontified-as
            '(()))))

(describe "Fontification of visibility"
  (it "fontifies property visibility"
    (expect "internal foo;"
            :to-be-fontified-as
            '(("internal" zephir-keyword)))

    (expect "scoped bar = { get, set, toString };"
            :to-be-fontified-as
            '(("scoped" zephir-keyword)))

    (expect "inline baz;"
            :to-be-fontified-as
            '(("inline" zephir-keyword)))

    (expect "public static hello = 42;"
            :to-be-fontified-as
            '(("public" zephir-keyword "static" zephir-keyword
               "hello" zephir-property-name)))

    (expect "public bar = { get };"
            :to-be-fontified-as
            '(("public" zephir-keyword "bar" zephir-property-name
               "get" zephir-keyword)))

    ;; TODO(serghei): ‘public targetLocal = true { toString, get };’
    (expect "public targetLocal = [] { toString, get };"
            :to-be-fontified-as
            '(("public" zephir-keyword "targetLocal" zephir-property-name
               "toString" zephir-keyword "get" zephir-keyword)))

    (expect "protected foo;"
            :to-be-fontified-as
            '(("protected" zephir-keyword "foo" zephir-property-name)))

    (expect "private baz = 1;"
            :to-be-fontified-as
            '(("private" zephir-keyword "baz" zephir-property-name)))

    (expect "private privateProperty;"
            :to-be-fontified-as
            '(("private" zephir-keyword
               "privateProperty" zephir-property-name))))

  (it "does not confuse variables and methods with magic shortcuts"
    (expect "var get;"
            :to-be-fontified-as
            '(("var" zephir-type "get" zephir-variable-name)))

    (expect "this->set(name, def, true)"
            :to-be-fontified-as
            '(("this" zephir-this "->" zephir-object-operator
               "set" zephir-method-call "true" zephir-constant)))

    (expect "/** If the magic method starts with \"get\" we try to get a
              * service with that name */"
            :to-be-fontified-as
            '(("/** If the magic method starts with \"get\" we try to get a" doc)
              ("              * service with that name */" doc)))))

(describe "Fontification of function headers"
  (it "fontifies standard ‘function <name> ()’ headers"
    (expect "function foo () {}"
            :to-be-fontified-as
            '(("function" zephir-keyword "foo" zephir-function-name)))

    (expect "function $foo() {}"
            :to-be-fontified-as
            '(("function" zephir-keyword "$foo" zephir-function-name))))

  (it "fontifies standard ‘fn <name> ()’ headers"
    (expect "fn foo () {}"
            :to-be-fontified-as
            '(("fn" zephir-keyword "foo" zephir-function-name)))

    (expect "fn $foo() {}"
            :to-be-fontified-as
            '(("fn" zephir-keyword "$foo" zephir-function-name))))

  (it "fontifies standard ‘<visibility> function <name> ()’ headers"
    (expect "public function foo () {}"
            :to-be-fontified-as
            '(("public" zephir-keyword"function" zephir-keyword
               "foo" zephir-function-name)))

    (expect "protected function bar () {}"
            :to-be-fontified-as
            '(("protected" zephir-keyword "function" zephir-keyword
               "bar" zephir-function-name)))

    (expect "private function baz () {}"
            :to-be-fontified-as
            '(("private" zephir-keyword "function" zephir-keyword
               "baz" zephir-function-name)))

    (expect "internal function foo () {}"
            :to-be-fontified-as
            '(("internal" zephir-keyword "function" zephir-keyword
               "foo" zephir-function-name)))

    (expect "scoped function test () {}"
            :to-be-fontified-as
            '(("scoped" zephir-keyword "function" zephir-keyword
               "test" zephir-function-name)))

    (expect "internal function fuz () {}"
            :to-be-fontified-as
            '(("internal" zephir-keyword "function" zephir-keyword
               "fuz" zephir-function-name))))

  (it "fontifies ‘<modifier> <visibility> function <name> ()’ headers"
    (expect "final public function __construct() {}"
            :to-be-fontified-as
            '(("final" zephir-method-modifier "public" zephir-keyword
               "function" zephir-keyword "__construct" zephir-function-name)))

    (expect "abstract private static function test() {}"
            :to-be-fontified-as
            '(("abstract" zephir-method-modifier "private" zephir-keyword
               "static" zephir-keyword "function" zephir-keyword
               "test" zephir-function-name))))

  (it "fontifies return type hints headers"
    (expect "fn foo() -> array | int | <Foo> | <\\A\\B\\C> | void"
            :to-be-fontified-as
            '(("fn" zephir-keyword "foo" zephir-function-name
               "array" zephir-type "int" zephir-type "Foo" zephir-type
               "\\A\\B\\C" zephir-type "void" zephir-type)))

    (expect "fn foo() -> int
             | bool | istring"
            :to-be-fontified-as
            '(("fn" zephir-keyword "foo" zephir-function-name
               "int" zephir-type)
              ("bool" zephir-type "istring" zephir-type))))

  (it "fontifies parametrized function headers"
    (expect "function go (a, &b, $c, &$d) {}"
            :to-be-fontified-as
            '(("function" zephir-keyword "go" zephir-function-name
               "a" zephir-variable-name "b" zephir-variable-name
               "$c" zephir-variable-name "$d" zephir-variable-name)))

    (expect "fn test (int a, string b, bool c, <A> d, const <\\A\\B> &$c) {}"
            :to-be-fontified-as
            ;; FIXME: ‘"const" zephir-variable-name’ => keyword, not variable
            '(("fn" zephir-keyword "test" zephir-function-name
               "int" zephir-type "a" zephir-variable-name
               "string" zephir-type "b" zephir-variable-name
               "bool" zephir-type "c" zephir-variable-name
               "A" zephir-type "d" zephir-variable-name
               "const" zephir-variable-name "\\A\\B" zephir-type
               "$c" zephir-variable-name))))

  (it "fontifies default values for function's parameters"
    (expect "function test (a = null, b=false, c=CONST_VAL, d = 42, e=\"str\")"
            :to-be-fontified-as
            '(("function" zephir-keyword "test" zephir-function-name
               "a" zephir-variable-name "null" zephir-constant
               "b" zephir-variable-name "false" zephir-constant
               "c" zephir-variable-name "CONST_VAL" zephir-constant
               "d" zephir-variable-name
               "e" zephir-variable-name "\"str\"" string)))))

;;; test-zephir-mode-font-lock.el ends here
