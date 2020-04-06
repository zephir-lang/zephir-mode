;;; zephir.el --- Common functionality to support Zephir language -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.7.0
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

;;   This file provides common variable and functions for Zephir packages.
;;
;;   Zephir -- is a high level language that eases the creation and
;; maintainability of extensions for PHP.  Zephir extensions are exported to C
;; code that can be compiled and optimized by major C compilers such as
;; gcc/clang/vc++.  Functionality is exposed to the PHP language.  For more
;; information see URL `https://zephir-lang.com'.
;;
;;;; Movement:
;;
;;   Move to the beginning or end of the current block with `beginning-of-defun'
;; (“C-M-a”) and `end-of-defun' (“C-M-e”) respectively.
;;
;;;; Imenu:
;;
;;   There is a support to jump to namespaces, classes, functions, properties
;; and constant declarations.

;;; Code:

(require 'rx)     ; `rx'
(require 'subr-x) ; `when-let'

(defconst zephir-language-keywords
  '("array"
    "as"
    "break"
    "try"
    "catch"
    "throw"
    "clone"
    "reverse"
    "empty"
    "fetch"
    "let"
    "if"
    "else"
    "elseif"
    "while"
    "break"
    "continue"
    "typeof"
    "instanceof"
    "loop"
    "for"
    "in"
    "do"
    "switch"
    "case"
    "default"
    "eval"
    "isset"
    "likely"
    "unlikely"
    "static"
    "unset"
    "new"
    "fn"
    "function"
    "use"
    "implements"
    "extends"
    "namespace"
    "return"
    "class"
    "interface"
    "echo"
    "final"
    "abstract")
  "Zephir language keywords.")

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

(defconst zephir-possible-visiblities
  '("public" "protected" "private" "internal" "inline" "scoped")
  "Possible values for visibility declaration in Zephir code.")

(defun zephir-create-regexp-for-classlike (&optional type)
  "Make a regular expression for a ‘classlike’ with the given TYPE.

Optional TYPE must be a string that specifies the type of a
object, such as ‘interface’ or ‘namespace’.

The regular expression this function returns will check for other
keywords that can appear in ‘classlike’ signatures,
e.g. ‘abstract’ or ‘final’.  The regular expression will have two
capture groups which will be TYPE and the name of an object
respectively."
  (let ((type (or type "class"))
        (line-start "")
        (modifier "")
        (root-ns ""))
    (cond
     ((string= type "class")
      (setq line-start "^"
            ;; Class modifier, e.g. if “abstract” or “final”
            modifier "\\(?:\\(?:abstract\\|final\\)?\\s-+\\)?"))
     ((string= type "interface")
      (setq line-start "^"))
     ((string= type "namespace")
      (setq line-start "^"))
     ((string= type "extends")
      (setq root-ns "\\\\?")))

    ;; Concatenate regexp parts
    (concat
     line-start
     "\\s-*"
     modifier

     ;; Object type, group #1
     "\\(" type "\\)"

     "\\s-+"

     ;; Object name, group #2.
     "\\(" root-ns (zephir-rx classlike) "\\)")))

(eval-when-compile
  (defun zephir-create-regexp-for-constant ()
    "Make a regular expression for a constant definition.

The regular expression will have two capture groups which will be
the word ‘const’ and the name of a constant respectively."
    (zephir-rx word-start (group "const")
               (+ (syntax whitespace)) (? "&")
               (group identifier)))


  (defun zephir-create-regexp-for-function (&optional visibility)
    "Make a regular expression for a function with the given VISIBILITY.

Optional VISIBILITY, when passed, must be a string that specifies
the visibility for a Zephir function, e.g. ‘scoped’ or ‘public’.
The parameter VISIBILITY can itself also be a regular expression.

The regular expression this function returns will check for other
keywords that can appear in method signatures, e.g. ‘final’ or
‘deprecated’.  The regular expression will have two capture
groups which will be the word ‘function’ (or ‘fn’) and the name
of a function respectively."
    (let ((visibility (or visibility zephir-possible-visiblities)))
      (when (stringp visibility)
        (setq visibility (list visibility)))
      (rx-to-string `(: line-start
                        (* (syntax whitespace))
                        (? "deprecated" (+ (syntax whitespace)))
                        (* (or "abstract" "final")
                           (+ (syntax whitespace)))
                        (or ,@visibility)
                        (+ (syntax whitespace))
                        (? "static" (+ (syntax whitespace)))
                        (group (or "fn" "function"))
                        (+ (syntax whitespace))
                        (group symbol-start
                               (? ?$)
                               (any "A-Z" "a-z" ?_)
                               (0+ (any "A-Z" "a-z" "0-9" ?_))
                               symbol-end)
                        (* (syntax whitespace))
		        "(")))))



;;;; Specialized rx

(eval-when-compile
  (defconst zephir-rx-constituents
    `(
      ;; Identifier
      (identifier . ,(rx (? ?$)
                         word-start
                         (any "A-Z" "a-z" ?_)
                         (0+ (any "A-Z" "a-z" "0-9" ?_))
                         word-end))

      ;; Magic constants
      (magic-const . ,(rx symbol-start
                          (or "__LINE__" "__FILE__" "__FUNCTION__"
                              "__CLASS__" "__METHOD__" "__NAMESPACE__")
                          symbol-end))

      ;; Predefined language constants
      (builtin-const . ,(rx symbol-start
                            (or "null" "true" "false")
                            symbol-end))

      ;; User-defined constants
      (constant . ,(rx symbol-start
                       (any "A-Z" ?_)
                       (+ (any "A-Z" "0-9" ?_))
                       symbol-end))

      ;; Function declaraion
      (fn-decl . ,(rx symbol-start (or "fn" "function") symbol-end))

      ;; Namespace, class or interface name
      (classlike . ,(rx symbol-start
                        (optional ?$)
                        (any "A-Z" "a-z" ?_)
                        (0+ (any "A-Z" "a-z" "0-9" ?_))
                        (0+
                         (and "\\"
                              (any "A-Z" "a-z" ?_)
                              (+ (any "A-Z" "a-z" "0-9" ?_))))
                        symbol-end))

      ;; Data types
      (data-type . ,(rx (or (and (? "u") "int")
                            (and "bool" (? "ean"))
                            (and (? "u") "long")
                            (and (? "u") "char")
                            (and (? "i") "string")
                            (and (or "dou" "calla") "ble")
                            "float"
                            "resource"
                            "object"
                            "var"
                            "void"
                            "array"))))
    "Additional special sexps for `zephir-rx'.")

  (defmacro zephir-rx (&rest sexps)
    "Zephir-specific replacement for `rx'.

In addition to the standard forms of `rx', the following forms
are available:

`identifier'
     Any valid identifier with optional dollar sign, e.g. function name,
     variable name, etc.

`magic-const'
     Magical keyword that is expanded at compile time.

`builtin-const'
     Predefined language constants.

`constant'
     User-defined constants.
     By convention, constant identifiers are always uppercase.

`fn-decl'
     A function declaraion.

`classlike'
     A valid namespace, class or interface name without leading ‘\\’.

`data-type'
     Any valid data type.

See `rx' documentation for more information about REGEXPS param."
    (let ((rx-constituents (append zephir-rx-constituents rx-constituents)))
      (rx-to-string (cond ((null sexps) (error "No regexp is provided"))
                          ((cdr sexps)  `(and ,@sexps))
                          (t            (car sexps)))
                    t))))


;;;; Movement

(defun zephir-beginning-of-defun (&optional arg)
  "Move the beginning of the ARGth Zephir function from point.
Implements Zephir version of `beginning-of-defun-function'."
  (interactive "p")
  (let ((arg (or arg 1))
        (case-fold-search t))
    (while (> arg 0)
      (re-search-backward (zephir-create-regexp-for-function) nil 'noerror)
      (back-to-indentation)
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (when (eq opoint (point))
          (re-search-forward (zephir-create-regexp-for-function) nil 'noerror)
          (back-to-indentation))
        (setq arg (1+ arg))))))

(defun zephir-end-of-defun (&optional arg)
  "Move the end of the ARG'th Zephir function from point.
Implements Zephir version of `end-of-defun-function'.  For more
see `zephir-beginning-of-defun'."
  (interactive "p")
  (zephir-beginning-of-defun (- (or arg 1))))


;;;; Imenu

(defvar zephir-imenu-generic-expression
  `(("Namespaces"
     ,(zephir-create-regexp-for-classlike "namespace") 2)
    ("Classes"
     ,(zephir-create-regexp-for-classlike) 2)
    ("Interfaces"
     ,(zephir-create-regexp-for-classlike "interface") 2)
    ("All Methods"
     ,(zephir-create-regexp-for-function) 2)
    ("Public Methods"
     ,(zephir-create-regexp-for-function '("public")) 2)
    ("Protected Methods"
     ,(zephir-create-regexp-for-function '("protected")) 2)
    ("Private Methods"
     ,(zephir-create-regexp-for-function '("private")) 2)
    ("Properties"
     ,(zephir-rx line-start
                 (* (syntax whitespace))
                 (or "public" "protected" "private")
                 (+ (syntax whitespace))
                 (group identifier)
                 (* (syntax whitespace))
                 (any "=" ";" "{"))
     1)
    ("Constants"
     ,(zephir-create-regexp-for-constant) 2))
  "Imenu generic expression for `zephir-mode'.
For more see `imenu-generic-expression'.")

(provide 'zephir)
;;; zephir.el ends here
