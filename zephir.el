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


;;;; Requirements

;; Pacify the byte compiler
(eval-when-compile
  (require 'rx))

(require 'subr-x) ; `when-let'


;;;; Customization

;;;###autoload
(defgroup zephir nil
  "Major mode for editing Zephir code."
  :tag "Zephir"
  :prefix "zephir-"
  :group 'languages
  :link '(url-link :tag "GitHub Page" "https://github.com/zephir-lang/zephir-mode")
  :link '(url-link :tag "Zephir Site" "https://zephir-lang.com")
  :link '(emacs-commentary-link :tag "Commentary" "zephir-mode"))


;;;; Zephir Keywords

(defconst zephir-magical-constants
  '("__LINE__"
    "__FILE__"
    "__FUNCTION__"
    "__CLASS__"
    "__METHOD__"
    "__NAMESPACE__")
  "Magical keyword that is expanded at compile time.
These are different from “constants” in strict terms.")

(defconst zephir-builtin-constants
  '("null" "true" "false")
  "Predefined language constants.")

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

(defconst zephir-possible-visiblities
  '("public" "protected" "private" "internal" "inline" "scoped")
  "Possible values for visibility declaration in Zephir code.")

(defconst zephir-name-start-re "[$_[:alpha:]]"
  "Regexp matching the start of a Zephir identifier, whithout groupping.")

(defconst zephir-name-re
  (concat zephir-name-start-re "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a Zephir identifier, whithout groupping.")

(defconst zephir-classlike-re
  (concat zephir-name-re "\\(?:\\s_\\|\\sw\\|\\\\\\)*")
  "Regexp matching a Zephir ‘classlike’, whithout groupping.")

(defconst zephir-data-type-re
  (concat "u?int\\|bool\\(?:ean\\)?\\|u?long\\|u?char\\|i?string"
          "\\|\\(?:calla\\|dou\\)ble\\|float\\|resource\\|object"
          "\\|var\\|void\\|array")
  "Regexp matching possible data types in Zephir, whithout groupping.")

(defconst zephir-constant-re
  (concat "\\<\\(const\\)\\s-+&?\\(" zephir-name-re "\\)")
  "Regular expression for a constant definition.

The regular expression will have two capture groups which will be
the word ‘const’ and the name of a constant respectively.")

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


;;;; Utils


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
     "\\(" root-ns zephir-classlike-re "\\)")))

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
		      "("))))


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
     ,(concat "\\(?:p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\)"
              "\\s-+\\(" zephir-name-re "\\)\\s-*[;={]")
     1)
    ("Constants" ,zephir-constant-re 2))
  "Imenu generic expression for `zephir-mode'.
For more see `imenu-generic-expression'.")

(provide 'zephir)
;;; zephir.el ends here
