;;; zephir-mode.el --- Major mode for editing Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Serghei Iakovlev

;; Author: Serghei Iakovlev (serghei@phalconphp.com)
;; Maintainer: Serghei Iakovlev
;; Version: 0.4.0
;; URL: https://github.com/zephir-lang/zephir-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "24.3"))

;; This file is not part of GNU Emacs.

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
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;   GNU Emacs major mode for editing Zephir code.  Provides font-locking,
;; indentation, alignment and navigation support.
;;
;; Syntax checking: Flymake support is not provided.
;;
;; Movement: Move to the beginning or end of the current block with
;; `beginning-of-defun' (C-M-a) and `end-of-defun' (C-M-e) respectively.
;;
;; Usage:  Put this file in your Emacs Lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'zephir-mode)
;;
;; To use abbrev-mode, add lines like this:
;;   (add-hook 'zephir-mode-hook
;;     '(lambda () (define-abbrev zephir-mode-abbrev-table "ex" "extends")))
;;
;; Many options available under Help:Customize
;; Options specific to zephir-mode are in Programming/Languages/Zephir
;;
;; The following variables are available for customization (see more via
;; `M-x customize-group zephir`):
;;
;; - Var `zephir-indent-level':
;;   indentation offset in spaces
;;
;; - Var `zephir-mode-hook':
;;   list of functions to execute when zephir-mode is initialized
;;
;; Bugs: Bug tracking is currently handled using the GitHub issue tracker at
;; https://github.com/zephir-lang/zephir-mode/issues
;;
;; History: History is tracked in the Git repository rather than in this file.
;; See https://github.com/zephir-lang/zephir-mode/blob/master/CHANGELOG.md

;;; Code:


;;; Compatibility

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
;; while zephir-mode only uses cl-lib (without compatibility aliases)
(eval-and-compile
  (if (and (= emacs-major-version 24) (>= emacs-minor-version 4))
      (require 'cl)))


;;; Requirements

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

(eval-when-compile
  (require 'rx))

(require 'cc-mode)
(require 'cl-lib)
(require 'pkg-info)


;;; Customization

;;;###autoload
(defgroup zephir nil
  "Major mode for editing Zephir code."
  :tag "Zephir"
  :prefix "zephir-"
  :group 'languages
  :link '(url-link :tag "GitHub Page" "https://github.com/zephir-lang/zephir-mode")
  :link '(url-link :tag "Zephir Forum" "https://forum.zephir-lang.com")
  :link '(url-link :tag "Zephir Site" "https://zephir-lang.com")
  :link '(emacs-commentary-link :tag "Commentary" "zephir-mode"))

(defcustom zephir-mode-hook nil
  "List of functions to call when entering Zephir Mode."
  :tag "Hook"
  :type  'hook
  :group 'zephir)

(defcustom zephir-indent-level 4
  "Amount by which Zephir code is indented.  Default is 4."
  :type 'integer
  :group 'zephir
  :safe #'integerp)


;;; Version information

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


;;; Utilities

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
  "Determine whether POS is inside a string or comment."
  (not (null (zephir-syntax-context pos))))

(defun zephir-in-string-p (&optional pos)
  "Determine whether POS is inside either a single-quoted or double-quoted string."
  (interactive)

  (let ((ctx (zephir-syntax-context pos)))
    (or (eq ctx 'single-quoted)
    (eq ctx 'double-quoted))))


;;; Specialized rx


;;; Navigation


;;; Indentation code


;;; Font Locking


;;; Alignment


;;; Imenu


;;; Initialization

(defvar zephir-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?$ "'" table)
    ;; TODO See: https://github.com/phalcon/php-zephir-parser/issues/63
    ;; (modify-syntax-entry ?\` "\"" table)
    table)
  "Syntax table in use in `zephir-mode' buffers.")

(define-derived-mode zephir-mode prog-mode "Zephir" ()
  "A major mode for editing Zephir code."
  :group 'zephir-mode
  ;; Comment setup
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)
  ;; Navigation
  ;; TODO
  ;; TODO
  ;; Indentation
  ;; TODO
  ;; Font locking
  ;; TODO
  )


;; Invoke zephir-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:

;;; zephir-mode.el ends here
