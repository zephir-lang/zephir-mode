;;; test-zephir-mode-navigatopn.el --- Zephir Mode: Navigation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Serghei Iakovlev

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

;; Define test-suites to test `zephir-mode' navigation using `buttercup'.

;;; Code:

(describe "Navigation"
  (it "moves back to the beginning of a defun"
    (with-zephir-buffer
     "    public function foo () {
      }

     // Some comment"
     (goto-char (point-max))
     (back-to-indentation)
     (expect (looking-at "// Some comment"))
     (beginning-of-defun)
     (expect (looking-at "public function foo"))))

  (it "moves back to the ARGth beginning of a defun"
    (with-zephir-buffer
     "public function bar () {
      }

      public function foo () {
      }

     // Some comment"
     (goto-char (point-max))
     (back-to-indentation)
     (expect (looking-at "// Some comment"))
     (beginning-of-defun 2)
     (expect (looking-at "public function bar"))))

  (it "moves forward to the end of a defun"
    (with-zephir-buffer
     "public function foo () {
      }
     // Some comment"
     (expect (looking-at "public function foo"))
     (end-of-defun)
     (expect (looking-at "\\s-*// Some comment"))))

  (it "moves forward to the ARGth end of a defun"
    (with-zephir-buffer
     "public function foo () {
      }
      public function bar () {
      }
     // Some comment"
     (expect (looking-at "public function foo"))
     (end-of-defun 2)
     (expect (looking-at "\\s-*// Some comment")))))

;;; test-zephir-mode-navigation.el ends here
