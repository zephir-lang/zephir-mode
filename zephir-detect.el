;;; zephir-detect.el --- Zephir detection -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.6.0
;; URL: https://github.com/zephir-lang/zephir-mode
;; Keywords: languages
;; Package-Requires: ((emacs "25.1"))
;; Revision: $Format:%h (%cD %d)$

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

;; Various Zephir detect functions and variables are defined here.  This feature
;; is used by `zephir-mode'.

;;; Code:

(require 'subr-x) ; `when-let'

(defun zephir-syntax-context (&optional pos)
  "Determine the syntax context at POS, defaulting to point.
Return nil, if there is no special context at POS, or one of
`comment'
     POS is inside a comment
`single-quoted'
     POS is inside a single-quoted string
`double-quoted'
     POS is inside a double-quoted string"
  (let ((state (save-excursion (syntax-ppss pos))))
    (if (nth 4 state)
        'comment
      (pcase (nth 3 state)
        (`?\' 'single-quoted)
        (`?\" 'double-quoted)))))

(defun zephir-in-string-or-comment-p (&optional pos)
  "Determine whether POS is inside a string or a comment."
  (not (null (zephir-syntax-context pos))))

(defun zephir-in-string-p (&optional pos)
  "Determine whether POS is inside a string.
This function determines single-quoted as well as double-quoted strings."
  (let ((ctx (zephir-syntax-context pos)))
    (or (eq ctx 'single-quoted)
        (eq ctx 'double-quoted))))

(defun zephir-in-comment-p (&optional pos)
  "Determine whether POS is inside a comment."
  (and (zephir-in-string-or-comment-p pos)
       (not (zephir-in-string-p pos))))

(defun zephir-comment-start-pos (ctx)
  "Return the position of comment containing current point.
If point is not inside a comment, return nil.  Uses CTX as a syntax context."
  (and ctx (nth 4 ctx) (nth 8 ctx)))

(defun zephir-in-ipg (re-open)
  "Return the position of RE-OPEN when `point' is inside an “IPG”.

This function is intended to use when `point' is inside a
parenthetical group (IPG) eg. in an array, argument list,
etc.  Return nil, if point is not in an IPG."
  (save-excursion
    (let ((opoint (nth 1 (syntax-ppss))))
      (when (and opoint
                 (progn (goto-char opoint) (looking-at-p re-open)))
        opoint))))

(defun zephir-in-param-list-p ()
  "Determine whether `point' is in a function parameter list."
  (ignore-errors
    (save-excursion
      (when-let ((open-paren-pt (zephir-in-ipg "(")))
        open-paren-pt
        (goto-char open-paren-pt)
        (forward-symbol -1)
        (or (looking-at-p "\\_<f\\(?:unctio\\)?n\\_>" )
            (progn (forward-symbol -1)
                   (looking-at-p "\\_<f\\(?:unctio\\)?n\\_>")))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-detect)
;;; zephir-detect.el ends here
