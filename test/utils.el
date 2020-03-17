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

(require 'cl-lib) ; `cl-defmacro'

;; Make sure the exact Emacs version can be found in the build output
(message "Running tests on Emacs %s" emacs-version)

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

(defmacro with-zephir-buffer (&rest body)
  "Evaluate BODY in a temporary buffer."
  (declare (debug (&rest form)))
  `(with-temp-buffer
     (zephir-mode)
     ,(if (fboundp 'font-lock-ensure)
          '(font-lock-ensure)
        '(with-no-warnings (font-lock-fontify-buffer)))
     (pop-to-buffer (current-buffer))
     (goto-char (point-min))
     (unwind-protect
         (progn ,@body))))

(defun zephir-join-strings (strs)
  "Join all strings in STRS in a series with \\n as a delimiter."
  (mapconcat (lambda (x) (concat x "\n")) strs ""))

(defun zephir-get-indented-strs (strs)
  "Indent all strings in STRS using `indent-region'."
  (butlast
   (split-string
    (with-zephir-buffer
     (let ((inhibit-message t))
       (insert (replace-regexp-in-string "^\\s *" "" (zephir-join-strings strs)))
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings (font-lock-fontify-buffer)))
       (indent-region (point-min) (point-max))
       (buffer-substring-no-properties
        (point-min) (point-max))))
    "\n" nil)))

(defun zephir-test-indent (code)
  "Test indentation of Zephir code.

The CODE argument is a string that should contain correctly
indented Zephir code."
  (let ((strs (split-string code "\n"))
        (indent-tabs-mode nil)
        (font-lock-verbose nil))
    (equal strs (zephir-get-indented-strs strs))))

;;; utils.el ends here
