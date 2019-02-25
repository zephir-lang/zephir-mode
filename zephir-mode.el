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
;;   Zephir -- is a high level language that eases the creation and
;; maintainability of extensions for PHP.  Zephir extensions are
;; exported to C code that can be compiled and optimized by major C
;; compilers such as gcc/clang/vc++.  Functionality is exposed to the
;; PHP language.  For more information see https://zephir-lang.com
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

(defvar zephir-zephir-website "https://zephir-lang.com"
  "Official website of Zephir programming language.")

(defvar zephir-github-mode "https://github.com/zephir-lang/zephir-mode"
  "Zephir Mode GitHub Page.")

(defvar zephir-github-zephir "https://github.com/phalcon/zephir"
  "Zephir GitHub Page.")

(defvar zephir-mode-hook nil
  "List of functions to call when entering Zephir Mode.")

(defcustom zephir-indent-tabs-mode t
  "Indentation can insert tabs in Zephir Mode if this is non-nil."
  :type 'boolean
  :group 'zephir
  :safe 'booleanp)


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


;;; Specialized rx


;;; Navigation


;;; Indentation code


;;; Font Locking



;;; Alignment


;;; Imenu


;;; Initialization

(define-derived-mode zephir-mode prog-mode "Zephir" ()
  "A major mode for editing Zephir code."
  :group 'zephir-mode
  ;; Comment setup
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)
  ;; Navigation
  ;; (setq-local beginning-of-defun-function #'zephir-beginning-of-defun)
  ;; (setq-local end-of-defun-function #'zephir-end-of-defun)
  ;; Indentation
  ;; (setq indent-tabs-mode zephir-indent-tabs-mode)
  ;; Font locking
  ;;(setq font-lock-defaults '((zephir-font-lock-keywords) nil nil))
  )


;; Invoke zephir-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:

;;; zephir-mode.el ends here
