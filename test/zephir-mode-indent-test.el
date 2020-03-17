;;; zephir-mode-indent-test.el --- Indentation tests for zephir-mode.el -*- lexical-binding: t; -*-

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

;; Define test-suites to test indentation for `zephir-mode' using `buttercup'.

;;; Code:

(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)

;;;; Tests

(describe "Commentary indentation"
  (it "indents Java-like dockblocks"
    (zephir-test-indent "
      /**
       * This is the summary for a DocBlock.
       *
       * This is the description for a DocBlock.
       * This text may contain multiple lines.
       */"))

  (it "indents C-style comments (1)"
    (zephir-test-indent "
      /* C-style comments
       * can contain
       * multiple lines.
       */"))

  (it "indents C-style comments (2)"
    (zephir-test-indent "
      /*
       * C-style comments
       * can contain
       * multiple lines. */"))

  (it "indents C-style comments (3)"
    (zephir-test-indent "
      /*
        C-style comments
        can contain
        multiple lines.
       */")))

;;; zephir-mode-indent-test.el ends here
