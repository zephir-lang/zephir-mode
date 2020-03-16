;;; zephir-mode-indent-test.el --- Zephir Mode: Indentation test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Free Software Foundation, Inc.

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.4.4
;; URL: https://github.com/sergeyklay/bnf-mode

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

;;   Automate tests from the "test" directory using `ert', which comes bundled
;; with Emacs >= 24.1.

;;; Code:

(ert-deftest zephir-mode-indentation/dockblocks ()
  :tags '(indentation)
  (zephir-test-indent "
  /**
   * This is the summary for a DocBlock.
   *
   * This is the description for a DocBlock.
   * This text may contain multiple lines.
   */"))

(ert-deftest zephir-mode-indentation/c-style-comments-1 ()
  :tags '(indentation)
  (zephir-test-indent "
  /* C-style comments
   * can contain
   * multiple lines.
   */"))

(ert-deftest zephir-mode-indentation/c-style-comments-2 ()
  :tags '(indentation)
  (zephir-test-indent "
  /*
   * C-style comments
   * can contain
   * multiple lines. */"))

(ert-deftest zephir-mode-indentation/c-style-comments-3 ()
  :tags '(indentation)
  (zephir-test-indent "
  /*
    C-style comments
    can contain
    multiple lines.
   */"))

(provide 'zephir-mode-indent-test)
;;; zephir-mode-indent-test.el ends here
