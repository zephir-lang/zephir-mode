;;; test-zephir-mode-utils.el --- Zephir Mode: Utils tests -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.5.0
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
  (describe "zephir-in-array"
    (it "determines the position of the openning ‘[’"
      (with-zephir-buffer
       '("let levels = [" "<*>" "]")
       (expect (zephir-in-array) :to-be 14)))

    (it "returns nil, if point is not in an array"
      (with-zephir-buffer
       "public function <*>foo() {}"
       (expect (zephir-in-array) :to-be nil)))))

;;; test-zephir-mode-utils.el ends here
