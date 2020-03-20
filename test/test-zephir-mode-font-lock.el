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

(describe "Fontification of classes"
  (it "fontify class"
    (expect "class A extends B implements C {}"
            :to-be-fontified-as
            '(("class" keyword "A" type "extends" keyword "B" type
               "implements" keyword "C" type))))

  (it "fontify use"
    (expect "use Phalcon\\Url;"
            :to-be-fontified-as
            '(("use" keyword "Phalcon\\Url" type))))

  (it "fontify use .. as"
    (expect "use Phalcon\\Url as PhUrl;"
            :to-be-fontified-as
            '(("use" keyword "Phalcon\\Url" type "as" keyword "PhUrl" type))))

  (it "fontify namespace"
    (expect "namespace Phalcon\\Url;"
            :to-be-fontified-as
            '(("namespace" keyword "Phalcon\\Url" type))))

  (it "fontify interface"
    (expect "interface UrlInterface {}"
            :to-be-fontified-as
            '(("interface" keyword "UrlInterface" type))))

  (it "fontify class modifiers"
    (expect "abstract final class Kernel {}"
            :to-be-fontified-as
            '(("abstract" keyword "final" keyword "class" keyword
               "Kernel" type)))))

(describe "Fontification of builtin constants"
  (it "fontifies constants"
    (expect "[ null, false, true ]"
            :to-be-fontified-as
            '(("null" constant "false" constant "true" constant)))))

(describe "Fontification of special keywords"
  (it "fontifies “this” keyword"
    (expect "this->foo = this;"
            :to-be-fontified-as
            '(("this" constant "this" constant)))))

(describe "Fontification of visibility"
  (it "fontifies internal method"
    (expect "internal function foo"
            :to-be-fontified-as
            '(("internal" keyword))))

  (it "fontifies scoped class"
    (expect "scoped class B {}"
            :to-be-fontified-as
            '(("scoped" keyword "class" keyword "B" type))))

  (it "fontifies inline function"
    (expect "inline function foo () {}"
            :to-be-fontified-as
            '(("inline" keyword))))

  (it "fontifies public property"
    (expect "public bar;"
            :to-be-fontified-as
            '(("public" keyword))))

  (it "fontifies protected property"
    (expect "protected foo;"
            :to-be-fontified-as
            '(("protected" keyword))))

  (it "fontifies private property"
    (expect "private bar;"
            :to-be-fontified-as
            '(("private" keyword)))))

;;; test-zephir-mode-font-lock.el ends here
