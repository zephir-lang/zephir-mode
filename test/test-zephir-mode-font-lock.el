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

(describe "Zephir builtins fontification"
  (it "fontify class"
    (with-zephir-buffer
     "class A {}"
     ;; class
     (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
     (should (eq (zephir-test-face-at 5) 'font-lock-keyword-face))
     (should-not (zephir-test-face-at 6))))

  (it "fontify namespace"
    (with-zephir-buffer
     "namespace Foo;
      class Bar {}"
     ;; namespace
     (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
     (should (eq (zephir-test-face-at 9) 'font-lock-keyword-face))
     (should-not (zephir-test-face-at 10))))

  (it "fontify interface"
    (with-zephir-buffer
     "namespace Phalcon\\Url;
      interface UrlInterface {}"
     ;; interface
     (should (eq (zephir-test-face-at 31) 'font-lock-keyword-face))
     (should (eq (zephir-test-face-at 38) 'font-lock-keyword-face))
     (should-not (zephir-test-face-at 39))))

  (it "fontify class modifiers"
    (with-zephir-buffer
     "abstract final class A {}"
     ;; abstract
     (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
     (should (eq (zephir-test-face-at 8) 'font-lock-keyword-face))
     (should-not (zephir-test-face-at 9))

     ;; final
     (should (eq (zephir-test-face-at 10) 'font-lock-keyword-face))
     (should (eq (zephir-test-face-at 14) 'font-lock-keyword-face))
     (should-not (zephir-test-face-at 15)))))

;;; test-zephir-mode-font-lock.el ends here
