;;; zephir-mode.el --- Major mode for editing Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.5.0
;; URL: https://github.com/zephir-lang/zephir-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "24.3"))
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

;;   GNU Emacs major mode for editing Zephir code.  Provides font-locking,
;; indentation and navigation support.
;;
;;   Zephir -- is a high level language that eases the creation and
;; maintainability of extensions for PHP.  Zephir extensions are
;; exported to C code that can be compiled and optimized by major C
;; compilers such as gcc/clang/vc++.  Functionality is exposed to the
;; PHP language.  For more information see URL `https://zephir-lang.com'.

;;;; Subword Mode:

;;   GNU Emacs comes with `subword-mode', a minor mode that allows you to
;; navigate the parts of a “camelCase” as if they were separate words.  For
;; example, Zephir Mode treats the variable “fooBarBaz” as a whole name by
;; default.  But if you enable `subword-mode' then Emacs will treat the variable
;; name as three separate words, and therefore word-related commands
;; (e.g. “M-f”, “M-b”, “M-d”, etc.) will only affect the “camelCase” part of the
;; name under the cursor.
;;
;; If you want to always use `subword-mode' for Zephir files then you can add
;; this to your Emacs configuration:
;;
;;   (add-hook 'zephir-mode-hook (lambda () (subword-mode 1)))

;;;; Movement:

;;   Move to the beginning or end of the current block with `beginning-of-defun'
;; (“C-M-a”) and `end-of-defun' (“C-M-e”) respectively.

;;;; Indentation:

;;   `zephir-indent-tabs-mode' can be changed to insert tabs for indentation in
;; Zephir Mode.  `zephir-indent-level' can be used to contol indentation level
;; of Zephir statements.

;;;; Syntax checking:

;; Presently flymake/flycheck support is not provided.

;;;; Support:

;;   Bug tracking is currently handled using the GitHub issue tracker at
;; `https://github.com/sergeyklay/zephir-mode/issues'.  Feel free to ask
;; question or make suggestions in our issue tracker.

;;;; History:

;;   History is tracked in the Git repository rather than in this file.  See URL
;; `https://github.com/zephir-lang/zephir-mode/blob/master/NEWS'.

;;;; Customize && Help:

;; See “M-x apropos-command ^zephir-” for a list of commands.
;; See “M-x customize-group zephir” for a list of customizable variables.

;;; Code:


;;;; Requirements

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

;; Pacify the byte compiler
(eval-when-compile
  (require 'rx)     ; `rx'
  ;; 24.x, 25.x compat
  (unless (fboundp 'prog-first-column)
    (defun prog-first-column () 0)))

(require 'imenu)    ; `imenu-generic-expression'
(require 'pkg-info) ; `pkg-info-version-info'


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

(defcustom zephir-mode-hook nil
  "List of functions to call when entering Zephir Mode."
  :tag "Hook"
  :type 'hook
  :group 'zephir)

(defcustom zephir-indent-tabs-mode nil
  "Indentation can insert tabs in Zephir Mode if this is non-nil."
  :type 'boolean
  :group 'zephir
  :safe 'booleanp)

(defcustom zephir-indent-level 4
  "Indentation of Zephir statements."
  :type 'integer
  :group 'zephir
  :safe 'integerp)


;;;; Version information

(defun zephir-mode-version (&optional show-version)
  "Display string describing the version of Zephir Mode.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'zephir-mode)))
    (when show-version
      (message "Zephir Mode version: %s" version))
    version))


;;;; Utilities

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

(defun zephir-in-array ()
  "Return the position of the openning ‘[’.
Return nil, if point is not in an array."
  (save-match-data
    (save-excursion
      (let ((point-init (point))
            (array-start (search-backward "[" nil t)))
        (when array-start
          (let ((close-brackets (count-matches "]" array-start point-init))
                (open-brackets 0))
            (while (and array-start (> close-brackets open-brackets))
              (setq array-start (search-backward "[" nil t))
              (when array-start
                (setq close-brackets (count-matches "]" array-start point-init))
                (setq open-brackets (1+ open-brackets)))))
          array-start)))))


;;;; Specialized rx

(eval-when-compile
  (defconst zephir-possible-visiblities
    '("public" "protected" "private" "internal" "inline" "scoped")
    "Possible values for visibility declaration in Zephir code.")

  (defconst zephir-rx-constituents
    `(
      ;; Identifier
      (identifier . ,(rx symbol-start
                         (optional ?$)
                         (any "A-Z" "a-z" ?_)
                         (0+ (any "A-Z" "a-z" "0-9" ?_))
                         symbol-end))
      ;; Builtin declarations and reserved keywords
      (builtin-decl . ,(rx symbol-start
                           (or "class"
                               "interface"
                               "namespace"
                               "abstract"
                               "final"
                               "use"
                               "extends"
                               "implements")
                           symbol-end))
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
      ;; Visibility modifier
      (visibility . ,(rx-to-string `(: symbol-start
                                       (or ,@zephir-possible-visiblities)
                                       symbol-end)))
      ;; Data types
      (data-type . ,(rx symbol-start
                        (or (and (? "u") "int")
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
                            "array")
                        symbol-end)))
    "Additional special sexps for `zephir-rx'.")

  (defmacro zephir-rx (&rest sexps)
    "Zephir-specific replacement for `rx'.

In addition to the standard forms of `rx', the following forms
are available:

`identifier'
     Any valid identifier with optional dollar sign, e.g. function name,
     variable name, etc.

`builtin-dcl'
     Any valid builtin declaraion.

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

`visibility'
     Any valid visibility modifier.

`data-type'
     Any valid data type.

See `rx' documentation for more information about REGEXPS param."
    (let ((rx-constituents (append zephir-rx-constituents rx-constituents)))
      (rx-to-string (cond ((null sexps) (error "No regexp is provided"))
                          ((cdr sexps)  `(and ,@sexps))
                          (t            (car sexps)))
                    t))))

(defun zephir-create-regexp-for-classlike (&optional type)
  "Make a regular expression for a classlike with the given TYPE.

Optional TYPE must be a string that specifies the type of a
object, such as ‘interface’ or ‘namespace’.

The regular expression this function returns will check for other
keywords that can appear in classlike signatures, e.g. ‘abstract’
or ‘final’.  The regular expression will have two capture groups
which will be TYPE and the name of an object respectively."
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
     ((or (string= type "extends")
          (string= type "implements")
          (string= type "use"))
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


;;;; Navigation

(defconst zephir-beginning-of-defun-regexp
  (zephir-rx line-start
             (0+ (syntax whitespace))
             (optional "deprecated" (+ (syntax whitespace)))
             (optional symbol-start
                       (or "abstract" "final")
                       symbol-end
                       (+ (syntax whitespace)))
             (optional visibility (+ (syntax whitespace))
                       (optional "static" (+ (syntax whitespace))))
             (group fn-decl)
             (+ (syntax whitespace))
             (group identifier)
             (0+ (syntax whitespace))
             "(")
  "Regular expression for a Zephir function.")

(defun zephir-beginning-of-defun (&optional arg)
  "Move the beginning of the ARGth Zephir function from point.
Implements Zephir version of `beginning-of-defun-function'."
  (interactive "p")
  (let ((arg (or arg 1))
        (case-fold-search t))
    (while (> arg 0)
      (re-search-backward zephir-beginning-of-defun-regexp nil 'noerror)
      (back-to-indentation)
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (when (eq opoint (point))
          (re-search-forward zephir-beginning-of-defun-regexp nil 'noerror)
          (back-to-indentation))
        (setq arg (1+ arg))))))

(defun zephir-end-of-defun (&optional arg)
  "Move the end of the ARG'th Zephir function from point.
Implements Zephir version of `end-of-defun-function'.  For more
see `zephir-beginning-of-defun'."
  (interactive "p")
  (zephir-beginning-of-defun (- (or arg 1))))


;;;; Indentation code

(defun zephir--proper-indentation (ctx)
  "Return the proper indentation for the current line.
This uses CTX as a current parse state."
  (save-excursion
    ;; Move `point' to the first non-whitespace character in the current line
    (back-to-indentation)

    (cond
     ;; Inside multiline string
     ((zephir-in-string-p) 0)

     ;; Multiline commentary
     ((zephir-in-comment-p)
      ;; Make sure comment line is indentet proper way relative to
      ;; open-comment (“/*”) for all possible use-cases.
      ;;
      ;;   /**
      ;;    * This is the summary for a DocBlock.
      ;;    *
      ;;    * This is the description for a DocBlock.
      ;;    */
      ;;
      ;;   /*
      ;;    * C-style comments
      ;;    * can contain multiple lines.
      ;;    */
      ;;
      ;;   /*
      ;;     C-style comments
      ;;     can contain multiple lines.
      ;;    */
      (let ((asteriks-marker-p (looking-at-p "\\*+")))
        (save-excursion
          (goto-char (zephir-comment-start-pos ctx))
          (+ (current-indentation)
             (if asteriks-marker-p 0 1)
             1))))

     ;; TODO(serghei): Cover use-case for single-line comments (“//”)

     ;; Inside an innermost parenthetical grouping
     ((let ((ipg-pos (nth 1 ctx)))
        (when ipg-pos
          (let ((array-start (zephir-in-array))
                (indent 0))
            (when array-start
              ;; Regular arrays
              ;;
              ;; let logger = [
              ;;     Logger::ALERT    : LOG_ALERT,
              ;;     Logger::CRITICAL : LOG_CRIT,
              ;;     [ 1 ],
              ;;     [
              ;;         foo,
              ;;         bar
              ;;     ]
              ;; ];
              (unless (looking-at-p "]")
                ;; Use normal indentation unless current symbol a closing
                ;; bracket on a line by itself.  Otherwise align it with opening
                ;; bracket.
                (setq indent (+ indent zephir-indent-level)))
              (save-excursion
                (goto-char array-start)
                (+ (current-indentation) indent)))))))

     ;; Otherwise indent to the first column
     (t (prog-first-column)))))

(defun zephir-indent-line ()
  "Indent current line as Zephir code."
  (interactive)
  (if (bobp)
      ;; First line is always non-indented
      (indent-line-to 0)
    (let* (
           ;; Save the current parse state.
           ;; We will need it in `zephir--proper-indentation'.
           (ctx (save-excursion (syntax-ppss (point-at-bol))))

           ;; The first non-whitespace character (l)
           ;; |          +-------------------------- The offset (-)
           ;; |          |
           ;; |          |              +------------- Whitespace characters (.)
           ;; |_________________________|______________
           ;; let foo = bar;...........................#
           ;;                                          |
           ;;                                          |
           ;; The current point position (#) ----------+
	   ;;
	   (offset (- (point) (save-excursion (back-to-indentation) (point)))))
      (indent-line-to (zephir--proper-indentation ctx))
      ;; Move point to the previous offset
      (when (> offset 0) (forward-char offset)))))


;;;; Font Locking

(defvar zephir-font-lock-keywords
  `(
    ;; Builtin declaration
    (,(zephir-rx (group builtin-decl))
     1 font-lock-keyword-face)
    ;; ‘class Foo’
    (,(zephir-create-regexp-for-classlike)
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; ‘namespace Foo’
    (,(zephir-create-regexp-for-classlike "namespace")
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; ‘interface Foo’
    (,(zephir-create-regexp-for-classlike "interface")
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; ‘use Foo’
    (,(zephir-create-regexp-for-classlike "use")
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; ‘implements Foo’
    (,(zephir-create-regexp-for-classlike "implements")
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; Highlight class name after ‘use ... as’
    (,(zephir-rx (optional "\\")
                 classlike
                 (+ (syntax whitespace))
                 (group symbol-start "as" symbol-end)
                 (+ (syntax whitespace))
                 (group identifier))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; Highlight ‘extends’
    (,(zephir-rx classlike
                 (+ (syntax whitespace))
                 (group symbol-start "extends" symbol-end)
                 (+ (syntax whitespace))
                 (group classlike)
                 (optional (+ (syntax whitespace)))
                 (or ?{ "implements"))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; Magic constants
    (,(zephir-rx (group magic-const))
     1 font-lock-builtin-face)
    ;; User-defined constants
    (,(zephir-rx (group (or constant builtin-const)))
     1 font-lock-constant-face)
    ;; Highlight special variables
    (,(zephir-rx (group symbol-start "this" word-end)
                 (0+ "->" identifier))
     1 font-lock-constant-face)
    ;; Visibility
    (,(zephir-rx (group visibility))
     (1 font-lock-keyword-face))
    ;; Function names, i.e. ‘function foo’
    ;; TODO(serghei): deprecated <visibility> function <name>
    ;; TODO(serghei): <visibility> static function <name>
    ;; TODO(serghei): deprecated function <name>
    ;; TODO(serghei): function <name>
    ;; TODO(serghei): let foo = function () {}
    (,zephir-beginning-of-defun-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ;; Data types
    (,(zephir-rx (group data-type))
     1 font-lock-type-face))
  "Font lock keywords for Zephir Mode.")


;;;; Alignment


;;;; Imenu

(defvar zephir-imenu-generic-expression
  `(
    ("Namespaces"
     ,(zephir-create-regexp-for-classlike "namespace") 2)
    ("Classes"
     ,(zephir-create-regexp-for-classlike "class") 2)
    ("Interfaces"
     ,(zephir-create-regexp-for-classlike "interface") 2)
    ("All Methods"
     ,zephir-beginning-of-defun-regexp 2)
    ("Properties"
     ,(zephir-rx line-start
                 (* (syntax whitespace))
                 visibility
                 (+ (syntax whitespace))
                 (group identifier)
                 (* (syntax whitespace))
                 (any "=" ";" "{"))
     1)
    ("Constants" ,(zephir-rx line-start
                             (* (syntax whitespace))
                             "const"
                             (+ (syntax whitespace))
                             (group identifier)
                             (* (syntax whitespace))
                             "=")
     1))
  "Imenu generic expression for lua-mode.
For more see `imenu-generic-expression'.")


;;;; Initialization

(defvar zephir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Symbol constituents
    (modify-syntax-entry ?_   "_"      table)
    (modify-syntax-entry ?$   "_"      table)

    ;; Characters used to delimit string constants
    (modify-syntax-entry ?\"  "\""     table)
    (modify-syntax-entry ?\'  "\""     table)

    ;; Comment enders
    (modify-syntax-entry ?\n  "> b"    table)

    ;; Give CR the same syntax as newline
    (modify-syntax-entry ?\^m "> b"    table)

    ;; Set up block and line oriented comments
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)

    ;; The parenthesis, braces and brackets
    (modify-syntax-entry ?\(  "()"     table)
    (modify-syntax-entry ?\)  ")("     table)
    (modify-syntax-entry ?\{  "(}"     table)
    (modify-syntax-entry ?\}  "){"     table)
    (modify-syntax-entry ?\[  "(]"     table)
    (modify-syntax-entry ?\]  ")["     table)
    table)
  "Syntax table in use in `zephir-mode' buffers.

This includes setting ' and \" as string delimiters, and setting up
the comment syntax tokens handle both line style \"//\" and block style
\"/*\" \"*/\" comments.")

;;;###autoload
(define-derived-mode zephir-mode prog-mode "Zephir" ()
  "A major mode for editing Zephir code.

\\{zephir-mode-map}

Turning on Zephir Mode calls the value of `prog-mode-hook' and then of
`zephir-mode-hook', if they are non-nil."
  :group 'zephir

  ;; Comments setup
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(/[*/]+\\)\\s-*")
  (setq-local comment-end "")

  ;; Font locking
  (setq font-lock-defaults
        '((zephir-font-lock-keywords) ; keywords
          nil                         ; keywords-only
          nil                         ; case-fold
          ))

  ;; TODO(serghei): Paragaphs

  ;; IMenu
  (setq-local imenu-generic-expression zephir-imenu-generic-expression)

  ;; Navigation
  (setq-local beginning-of-defun-function #'zephir-beginning-of-defun)
  (setq-local end-of-defun-function #'zephir-end-of-defun)

  ;; Indentation
  (setq-local indent-line-function #'zephir-indent-line)
  (setq-local indent-tabs-mode zephir-indent-tabs-mode))


;; Invoke zephir-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)

;;; zephir-mode.el ends here
