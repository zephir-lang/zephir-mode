;;; zephir-mode.el --- Major mode for editing Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.7.0
;; URL: https://github.com/zephir-lang/zephir-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "25.1"))
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

;;   GNU Emacs major mode for editing Zephir code.  Provides syntax
;; highlighting, indentation, movement, Imenu and navigation support.
;;
;;;; Support:
;;
;;   Bug tracking is currently handled using the GitHub issue tracker at
;; `https://github.com/zephir-lang/zephir-mode/issues'.  Feel free to ask
;; question or make suggestions in our issue tracker.
;;
;;;; History:
;;
;;   History is tracked in the Git repository rather than in this file.  See URL
;; `https://github.com/zephir-lang/zephir-mode/blob/master/NEWS'.
;;
;;;; Customize && Help:
;;
;;   See “M-x apropos-command RET ^zephir- RET” for a list of all commands and
;; “M-x customize-group RET zephir RET” for a list of customizable variables.

;;; Code:


;;;; Requirements

(require 'zephir)
(require 'zephir-face)
(require 'zephir-indent)

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

;; Pacify the byte compiler
(eval-when-compile
  (require 'rx)
  (require 'regexp-opt))

(require 'font-lock)
(require 'imenu)
(require 'pkg-info)


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


;;;; Initialization

(defvar zephir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Symbol constituents
    (modify-syntax-entry ?_   "_"      table)
    (modify-syntax-entry ?$   "_"      table)

    ;; Punctuaction constituents
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)

    ;; Characters used to delimit string constants
    (modify-syntax-entry ?\"  "\""     table)
    (modify-syntax-entry ?\'  "\""     table)

    ;; Comment enders
    (modify-syntax-entry ?\n  "> b"    table)

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
  (setq-local font-lock-syntactic-face-function
              #'zephir-font-lock-syntactic-face)
  (setq-local font-lock-defaults
              '((zephir-font-lock-keywords) ; keywords
                nil                         ; keywords-only
                nil))                       ; case-fold

  ;; TODO(serghei): Paragraphs

  ;; Imenu
  (setq-local imenu-generic-expression zephir-imenu-generic-expression)

  ;; Navigation
  (setq-local beginning-of-defun-function #'zephir-beginning-of-defun)
  (setq-local end-of-defun-function #'zephir-end-of-defun)

  ;; Indentation
  (setq-local indent-line-function #'zephir-indent-line)
  (setq-local indent-tabs-mode zephir-indent-tabs-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)
;;; zephir-mode.el ends here
