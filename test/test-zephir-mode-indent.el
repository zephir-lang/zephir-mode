;;; zephir-mode-indent-test.el --- Zephir Mode: Indentation tests -*- lexical-binding: t; -*-

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

;; Define test-suites to test `zephir-mode' indentation using `buttercup'.

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
(describe "String indentation"
  (it "un-indents miltiline strings"
    (zephir-test-indent
     '("\"" "Hello World" "\""))))

(describe "Block indentation"
  (it "properly indents ‘if’ blocks"
    (zephir-test-indent
     '("if !fetch prefix, options[\"prefix\"] {"
       "    let prefix = \"\";"
       "    unset(options[\"prefix\"]);"
       "}"))

    (zephir-test-indent
     '("if this->has(asset) {"
       "    return false;"
       "} else { "
       "    return true;"
       "}")))

  (it "properly indents ‘classlike’ blocks"
    (zephir-test-indent
     '("class A"
       "{"
       "    public b;"
       "    public function foo() {"
       "        let b = 42;"
       "    }"
       "    public function bar()"
       "    {"
       "        let b = 13;"
       "    }"
       "}")))
  )

(describe "Argguments list indentation"
  (it "indents regular function calls"
    (zephir-test-indent
     '("this->interpolate("
       "    item->getMessage(),"
       "    item->getContext()"
       ");")))

  (it "indents function calls in column like style"
    (zephir-test-indent
     '("new AssetJs( path,"
       "             collectionLocal,"
       "             filter,"
       "             collectionAttributes,"
       "             version,"
       "             autoVersion );")))

    (it "indents function declaration"
    (zephir-test-indent
     '("public function aVeryLongMethodName("
       "    ClassTypeHint $arg1,"
       "    $arg2,"
       "    array $arg3 = []"
       ") {"
       "    // method body"
       "}"))))

(describe "Array indentation"
  (it "indents regular arrays"
    (zephir-test-indent
     '("let logger = ["
       "    Logger::ALERT    : LOG_ALERT,"
       "    Logger::CRITICAL : LOG_CRIT,"
       "    [ 1 ],"
       "    ["
       "        foo,"
       "        bar"
       "    ]"
       "];")))

  (it "indents column like arrays"
    (zephir-test-indent
     '("let attributes = [ type : text,"
       "                   href : url,"
       "                   rel  : stylesheet ];"))))

(describe "Commentary indentation"
  (it "indents Java-like dockblocks"
    (zephir-test-indent
     '("/**"
       " * This is the summary for a DocBlock."
       " *"
       " * This is the description for a DocBlock."
       " * This text may contain multiple lines."
       " */")))

  (it "indents C-style comments (1)"
    (zephir-test-indent
     '("/* C-style comments"
       " * can contain"
       " * multiple lines."
       " */")))

  (it "indents C-style comments (2)"
    (zephir-test-indent
     '("/*"
       " * C-style comments"
       " * can contain"
       " * multiple lines. */")))

  (it "indents C-style comments (3)"

    (zephir-test-indent
     '("/*"
       "  C-style comments"
       "  can contain"
       "  multiple lines."
       " */")))

  (it "carefully indents offsets"
    (expect (zephir-get-indented-code "/* test */   ")
            :to-equal "/* test */   "))

  (it "unindents first line"
    (expect (zephir-get-indented-code "   /* test */")
            :to-equal "/* test */")))

;;; test-zephir-mode-indent.el ends here
