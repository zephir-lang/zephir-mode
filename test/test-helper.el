;;; test-helper.el --- Zephir Mode: Non-interactive unit-test setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018, 2019, 2020 Serghei Iakovlev

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.4.0
;; URL: https://github.com/zephir-lang/zephir-mode

;; This file is NOT part of GNU Emacs.

;;; License

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

;; Non-interactive test suite setup for ERT Runner.

;;; Code:

(require 's)      ; `s-contains?'
(require 'cl-lib) ; `cl-defmacro'

;; Make sure the exact Emacs version can be found in the build output
(message "Running tests on Emacs %s" emacs-version)

;; The test fixtures assume an indentation width of 4, so we need to set that
;; up for the tests.
(setq-default default-tab-width 4
              indent-tabs-mode nil)

(when (require 'undercover nil t)
  ;; Track coverage, but don't send to coverage serivice.  Save in parent
  ;; directory as undercover saves paths relative to the repository root.
  (undercover "*.el"
              (:report-file "coverage-final.json")
              (:send-report nil)))

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Don't load old byte-compiled versions
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "zephir-mode" source-directory)))

(defmacro zephir-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (zephir-mode)

     ,(if (fboundp 'font-lock-ensure)
          '(font-lock-ensure)
        '(with-no-warnings (font-lock-fontify-buffer)))

     (goto-char (point-min))
     ,@body))

(defun zephir-test-indent (code)
  "Test indentation of Zephir code.

The CODE argument is a string that should contain correctly
indented Zephir code.  The CODE is indented using `indent-region'
and the test succeeds if the result did not change."
  (zephir-test-with-temp-buffer
   code
   (indent-region (point-min) (point-max))
   (should (string= (buffer-string) code))))

(when (s-contains? "--win" (getenv "ERT_RUNNER_ARGS"))
  (defun ert-runner/run-tests-batch-and-exit (selector)
    (ert-run-tests-interactively selector)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; test-helper.el ends here
