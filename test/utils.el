;;; utils.el --- Zephir Mode: Non-interactive unit-test setup -*- lexical-binding: t; -*-

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

;; Non-interactive test suite setup for `buttercup'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)    ; `cl-defmacro'

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Don't load old byte-compiled versions
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "zephir-mode" source-directory) nil 'nomessage))

(defmacro with-zephir-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (zephir-mode)

     ,(if (fboundp 'font-lock-ensure)
          '(font-lock-ensure)
        '(with-no-warnings (font-lock-fontify-buffer)))

     (pop-to-buffer (current-buffer))
     (goto-char (point-min))
     (unwind-protect
         (progn ,@body))))

(defun zephir-get-indented-code (code)
  "Indent Zephir CODE using `indent-region'.
Return the whole buffer, without the text properties."
  (with-zephir-buffer
   code
   (let ((inhibit-message t))
     (indent-region (point-min) (point-max))
     (buffer-substring-no-properties
      (point-min) (point-max)))))

(defun zephir-test-indent (code)
  "Test indentation of Zephir code.
The CODE argument is a string that should contain correctly
indented Zephir code.  The CODE is indented using
`zephir-get-indented-code' and the test succeeds if the result did not
change."
  (let ((content code))
    ;; The test fixtures assume an indentation width of 4,
    ;; so we need to set that up for the tests.
    (setq-default indent-tabs-mode nil)
    (setq-default default-tab-width 4)
    (expect (zephir-get-indented-code content) :to-equal code)))

(defun zephir-get-face-at (pos &optional content)
  "Get the face at POS in CONTENT.
If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (with-zephir-buffer content
                          (get-text-property pos 'face))
    (get-text-property pos 'face)))

;;; utils.el ends here
