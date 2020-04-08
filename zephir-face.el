;;; zephir-face.el --- Face definitions for Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/zephir-lang/zephir-mode
;; Keywords: languages
;; Package-Requires: ((emacs "25.1"))

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

;; Face definitions for Zephir code.  This feature is used by `zephir-mode'.

;;; Code:


;;;; Requirements

(require 'zephir)

;; Pacify the byte compiler
(eval-when-compile
  (require 'rx)
  (require 'regexp-opt))


;;;; Customization

;;;###autoload
(defgroup zephir-faces nil
  "Faces used in Zephir Mode"
  :tag "Zephir Faces"
  :group 'zephir
  :group 'faces)

(defface zephir-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Zephir Mode face used to highlight keywords."
  :group 'zephir-faces
  :tag "Zephir Keyword")

(defface zephir-function-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Zephir Mode face used to highlight function names."
  :group 'zephir-faces
  :tag "Zephir Function Name")

(defface zephir-function-call-face
  '((t ()))
  "Zephir Mode face used to highlight function names in calles."
  :group 'zephir-faces
  :tag "Zephir Function Call")

(defface zephir-method-call-face
  '((t (:inherit zephir-function-call-face)))
  "Zephir Mode face used to highlight method names in calles."
  :group 'zephir-faces
  :tag "Zephir Method Call")

(defface zephir-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Zephir Mode face used to highlight variable names."
  :group 'zephir-faces
  :tag "Zephir Variable Name")

(defface zephir-property-name-face
  '((t (:inherit zephir-variable-name-face)))
  "Zephir Mode face used to highlight property names."
  :group 'zephir-faces
  :tag "Zephir Property Name")

(defface zephir-operator-face
  '((t ()))
  "Zephir Mode face used to operators."
  :group 'zephir-faces
  :tag "Zephir Operator")

(defface zephir-comparison-operator-face
  '((t (:inherit zephir-operator-face)))
  "Zephir Mode face used to comparison operators (‘==’, ‘!=’, ‘===’, ...)."
  :group 'zephir-faces
  :tag "Zephir Comparison Operator")

(defface zephir-logical-operator-face
  '((t (:inherit zephir-operator-face)))
  "Zephir Mode face used to logical operators (‘&&’, ‘||’, ‘!’, ...)."
  :group 'zephir-faces
  :tag "Zephir Logical Operator")

(defface zephir-object-operator-face
  '((t (:inherit zephir-operator-face)))
  "Zephir Mode face used to object operators (‘->’)."
  :group 'zephir-faces
  :tag "Zephir Object Operator")

(defface zephir-type-face
  '((t (:inherit font-lock-type-face)))
  "Zephir Mode face used to highlight types."
  :group 'zephir-faces
  :tag "Zephir Type")

(defface zephir-type-<>-face
  '((t ()))
  "Zephir Mode face used to highlight angle brackets around the type (‘<Foo>’)."
  :group 'zephir-faces
  :tag "Zephir Type Brackets")

(defface zephir-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Zephir Mode face used to highlight constants."
  :group 'zephir-faces
  :tag "Zephir Constant")

(defface zephir-magical-constant-face
  '((t (:inherit font-lock-builtin-face)))
  "Zephir Mode face used to highlight magical constants."
  :group 'zephir-faces
  :tag "Zephir Magical Constant")

(defface zephir-constant-assign-face
  '((t (:inherit font-lock-type-face)))
  "Zephir Mode face used to highlight constant assigning (‘const’ statement)."
  :group 'zephir-faces
  :tag "Zephir Constant Assign")

(defface zephir-this-face
  '((t (:inherit zephir-constant-face)))
  "Zephir Mode face used to highlight ‘this’ variables."
  :group 'zephir-faces
  :tag "Zephir this")

(defface zephir-class-declaration-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to class declarations (‘class’, ‘interface’)."
  :group 'zephir-faces
  :tag "Zephir Class Declaration")

(defface zephir-class-declaration-spec-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight class declaration specification keywords
(‘implements’, ‘extends’)."
  :group 'zephir-faces
  :tag "Zephir Class Declaration Specification")

(defface zephir-namespace-declaration-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight namespace declaration keyword."
  :group 'zephir-faces
  :tag "Zephir Namespace Declaration")

(defface zephir-import-declaration-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight import statements (‘use ... as ...’)."
  :group 'zephir-faces
  :tag "Zephir Import Statement")

(defface zephir-class-modifier-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight class modifiers (‘final’, ‘abstract’)."
  :group 'zephir-faces
  :tag "Zephir Class Modifier")

(defface zephir-method-modifier-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight method modifiers (‘final’, ‘abstract’)."
  :group 'zephir-faces
  :tag "Zephir Method Modifier")

(defun zephir-font-lock-syntactic-face (state)
  "Specify font lock faces based on syntax table entries.
Uses STATE as a syntax context."
  (if (nth 3 state)
      font-lock-string-face
    (if (save-excursion
          (goto-char (zephir-comment-start-pos state))
          (looking-at "/\\*\\*"))
        font-lock-doc-face
      font-lock-comment-face)))


;;;; Font locking

(defconst zephir--font-lock-keywords-1
  `(;; Highlight occurrences of class declaration specification
    (,(zephir-create-regexp-for-classlike "extends")
     (1 'zephir-class-declaration-spec-face)
     (2 font-lock-type-face))

    ;; Highlight occurrences of namespace declarations
    (,(zephir-create-regexp-for-classlike "namespace")
     (1 'zephir-namespace-declaration-face)
     (2 font-lock-type-face))

    ;; Highlight occurrences of class declarations
    (,(zephir-create-regexp-for-classlike "class")
     (1 'zephir-class-declaration-face)
     (2 font-lock-type-face))

    ;; Highlight occurrences of interface declarations
    (,(zephir-create-regexp-for-classlike "interface")
     (1 'zephir-class-declaration-face)
     (2 font-lock-type-face))

    ;; Highlight occurrences of class modifiers
    (,(concat (regexp-opt '("abstract" "final") 'symbols)
              "\\s-+"
              "\\_<class\\_>")
     1 'zephir-class-modifier-face)

    ;; Highlight occurrences of import statements
    (,(rx-to-string `(: (* (syntax whitespace))
                        symbol-start (group "use") symbol-end
                        (+ (syntax whitespace))
                        (group (+ (or (syntax word)
                                      (syntax symbol)
                                      ?\\)))))
     (1 'zephir-import-declaration-face)
     (2 font-lock-type-face))

    ;; Highlight occurrences of import aliases
    (,(concat "\\<as\\s-+"
              "\\(" zephir-name-re "\\)")
     1 font-lock-type-face))
  "Level one font lock keywords for `zephir-mode'.")

(defconst zephir--font-lock-keywords-2
  (append
   zephir--font-lock-keywords-1
   `(;; Highlight occurrences of logical operators
     ("\\(!\\|&&\\|||\\)[^=]" 1 'zephir-logical-operator-face)))
  "Level two font lock keywords for `zephir-mode'.")

(defconst zephir-font-lock-keywords
  (append
   zephir--font-lock-keywords-2
   `(
     ;; Highlight occurrences of ‘implements Foo, Bar’.
     ;;
     ;; The first regexp is the anchor of the fontification, meaning the
     ;; "starting point" of the region:
     ;;
     ;;                  ------------ Starting point
     ;;                  |
     ;; class Terminator implements \Robot, \Machine
     ;; {
     ;; }
     ;;
     ("\\_<\\(implements\\)\\_>\\s-+"
      ;; Fontify the `implements' as a `zephir-class-declaration-spec-face'.
      (1 'zephir-class-declaration-spec-face)
      ;; Look for symbols after the space (‘\\s-+’), they are classes.
      ("\\(\\(?:\\sw\\|\\s_\\|\\\\\\)+\\)\\_>"
       ;; Set the limit of search to the current `implements' form only.
       (save-excursion
         (re-search-forward "{\\|;\\|extends" nil 'noerror)
         (forward-char -1)
         (point))
       ;; When we found all the classes in the region (`implements' form)
       ;; go back to the ‘\\s-+’ marker.
       (progn (re-search-backward "\\_<\\(implements\\)\\_>\\s-+")
              (forward-symbol 1))
       ;; Fontify each matched symbol as class.
       (1 font-lock-type-face)))

     ;; Highlight occurrences of method modifiers (‘abstract’, ‘final’)
     (,(rx-to-string `(: symbol-start (group (or "abstract" "final")) symbol-end
                         (+ (syntax whitespace))
                         (+ (or ,@(append
                                   zephir-possible-visiblities
                                   '("static")))
                            (+ (syntax whitespace)))
                         symbol-start
                         (or "fn" "function")
                         symbol-end))
      1 'zephir-method-modifier-face)

     ;; Fontify methods call like ‘object->method()’
     (,(concat "\\(->\\)"
               "\\(" zephir-name-re "\\)\\s-*(")
      (1 'zephir-object-operator-face)
      (2 'zephir-method-call-face))

     ;; Highlight occurrences of magic constants
     (,(regexp-opt zephir-magical-constants 'symbols)
      1 'zephir-magical-constant-face)

     ;; Highlight definition of user defined constants
     (,zephir-constant-re
      (1 'zephir-keyword-face)
      (2 'zephir-constant-assign-face))
     ("\\_<[A-Z_][0-9A-Z_]+\\_>" 0 'zephir-constant-face)

     ;; Highlight occurrences of built-in constants
     (,(regexp-opt zephir-builtin-constants 'symbols)
      1 'zephir-constant-face)

     ;; Highlight occurrences of the word ‘this’
     ("\\<\\(this\\)\\>" 1 'zephir-this-face)

     ;; Highlight properties like ‘object->property’
     (,(concat "\\(->\\)"
               "\\(" zephir-name-re "\\)")
      (1 'zephir-object-operator-face)
      (2 'zephir-property-name-face))

     ;; Highlight function/method name i.e. ‘function foo ()’
     (,(concat (regexp-opt '("fn" "function") 'words)
               "\\s-+\\("
               zephir-name-re
               "\\)\\s-*(")
      (1 'zephir-keyword-face)
      (2 'zephir-function-name-face))

     ;; Type hints i.e. ‘int a’
     (,(concat "\\(?:const\\s-+\\)?"
               "\\<\\b\\(" zephir-data-type-re "\\)!?"
               "\\s-+&?\\(" zephir-name-re "\\)")
      (1 'zephir-type-face)
      (2 'zephir-variable-name-face))

     ;; Type hints i.e. ‘<AdapterFactory> factory’
     (,(concat "\\(?:const\\s-+\\)?"
               "\\(<\\)"
               "\\(\\(?:\\sw\\|\\s_\\|\\\\\\)+\\)"
               "\\(>\\)"
               "\\s-+&?"
               "\\(" zephir-name-re "\\)")
      (1 'zephir-type-<>-face)
      (2 'zephir-type-face)
      (3 'zephir-type-<>-face)
      (4 'zephir-variable-name-face))

     ;; Continued formal parameter list i.e. ‘function foo (a, b, c, d, e)’
     (,(concat "\\s-*&?" zephir-name-re "\\s-*[),=]")
      (,zephir-name-re
       (if (save-excursion (backward-char)
                           (zephir-in-param-list-p))
           (forward-symbol -1)
         (end-of-line))
       (end-of-line)
       (0 'zephir-variable-name-face)))

     ;; Return type hints
     (,(rx (or (:")" (* (syntax whitespace)) "->") "|")
           (* (syntax whitespace))
           (group "<")
           (group (+ (or (syntax word) (syntax symbol) "\\")))
           (group ">")
           (* (syntax whitespace)))
      (1 'zephir-type-<>-face)
      (2 'zephir-type-face)
      (3 'zephir-type-<>-face))
     (,(concat "\\(?:)\\s-*->\\||\\)"
               "\\s-*"
               "\\(" zephir-data-type-re "\\)")
      1 'zephir-type-face)

     ;; Highlight occurrences of comparison operators
     ;; TODO(serghei): Remove ‘[^-]’ from the regexp
     (,"===\\|==\\|!==\\|!=\\|=>\\|<=\\|<\\|[^-]>"
      1 'zephir-comparison-operator-face)

     ;; Builtin declarations and reserverd keywords
     (,(regexp-opt (append zephir-language-keywords
                           zephir-possible-visiblities)
                   'symbols)
      1 'zephir-keyword-face)

     ;; Function names, i.e. ‘function foo’
     ;; TODO(serghei): deprecated <visibility> function <name>
     ;; TODO(serghei): <visibility> static function <name>
     ;; TODO(serghei): deprecated function <name>
     ;; TODO(serghei): let foo = function () {}
     (,(zephir-create-regexp-for-function)
      (1 'zephir-keyword-face)
      (2 'zephir-function-name-face))

     ;; Highlight occurrences of class member variables (‘public foo’).
     ;;
     ;; The first regexp is the anchor of the fontification, meaning the
     ;; "starting point" of the region:
     ;;
     ;; ------------------------- Starting point
     ;; |
     ;; public static myVar = [] { get, set, toString };
     ;;
     ("\\_<\\(p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\)\\_>\\s-+"
      ;; Fontify the property visibility as a `zephir-keyword-face'.
      (1 'zephir-keyword-face)
      ;; When done with the visibility look for the ‘static’ word.
      ;; At this moment we're at point after the ‘\\s-+’ (from previous regexp).
      ("\\<static\\>"
       ;; Set the limit of the seacrh to the current class property only.
       (save-excursion
         (re-search-forward "\\s-\\|;" nil 'noerror)
         (forward-char -1)
         (point))
       ;; Do not move back when we've found ‘static’ keyword to ensure
       ;; forward progress.
       nil
       ;; Fontify the found word as `zephir-keyword-face'.
       (0 'zephir-keyword-face))
      ;; Look for symbols after the space (‘\\s-+’), this is a property name.
      ("\\(\\$?\\<[A-Z_a-z][0-9A-Z_a-z]*\\>\\)"
       ;; Set the limit of search to a property name only.
       (save-excursion
         (re-search-forward "=\\|;\\|{" nil 'noerror)
         (forward-char -1)
         (point))
       ;; Do not move back when we've found property name to ensure
       ;; forward progress.
       nil
       ;; Fontify each matched symbol as property.
       (1 'zephir-property-name-face))
      ;; Finally search for magic shortcuts.  They are in the following form:
      ;;
      ;;   public foo = 42 { get, set, toString };
      ;;                   ^^^^^^^^^^^^^^^^^^^^^^
      ;;
      ("\\<\\(?:get\\|set\\|toString\\)\\>"
       ;; Set the limit of search to the current property form only.
       (save-excursion
         (re-search-forward ";\\|}" nil 'noerror)
         (forward-char -1)
         (point))
       ;; Do not move back when we've found all matches to ensure
       ;; forward progress.  At this point we are done with the form.
       nil
       ;; Fontify the found word as `zephir-keyword-face'.
       (0 'zephir-keyword-face)))))
  "Font lock keywords for Zephir Mode.")

(provide 'zephir-face)
;;; zephir-face.el ends here
