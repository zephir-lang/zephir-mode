;;; zephir-indent.el --- Indentation support for Zephir code -*- lexical-binding: t; -*-

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

;;   Indentation support for Zephir code and various (auto-)indenting functions
;; and variables are defined here.  This feature is used by `zephir-mode'.
;;
;;   There are two options to use auto-indentation when inserting newlines:
;;
;; 1) Enable the minor-mode `electric-indent-mode' (enabled by default) and use
;;    “RET”.  If this mode is disabled use `newline-and-indent', bound to “C-j”.
;;
;; 2) Add the following hook in your init file:
;;
;;    (add-hook 'zephir-mode-hook
;;      #'(lambda ()
;;          (define-key zephir-mode-map "\C-m" 'newline-and-indent)))
;;
;;   The first option is prefered since you'll get the same behavior for all
;; modes out-of-the-box.
;;
;;   `zephir-indent-tabs-mode' can be changed to insert tabs for indentation in
;; Zephir Mode.  `zephir-indent-level' can be used to contol indentation level
;; of Zephir statements.

;;; Code:


;;;; Requirements

(require 'zephir)

;; Pacify the byte compiler
(eval-when-compile
  ;; 25.x compat
  (unless (fboundp 'prog-first-column)
    (defun prog-first-column () 0)))


;;;; Customization

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


;;;; Indentation

(defun zephir-indent-block (block-start)
  "Return the proper indentation for the block.
BLOCK-START must contain open bracket position of the block."
  (save-excursion
    (let ((offset 0))
      (unless (looking-at-p "}")
        (setq offset zephir-indent-level))
      (when (and block-start (progn (goto-char block-start) (looking-at-p "{")))
        (+ (current-indentation) offset)))))

(defun zephir-indent-listlike (pt-start re-close)
  "Return the proper indentation for the ‘listlike’.

PT-START must contain open bracket position of the ‘listlike’.
RE-CLOSE must contain the regular expression that uniquely
identifies the close bracket of the ‘listlike’."
  ;; The diagram below explains the purpose of the variables:
  ;;
  ;;    `pt-start'
  ;;            |
  ;;            |
  ;;  let map = [
  ;;      "none" : \Redis:SERIALIZER_NONE,
  ;;      "php"  : \Redis:SERIALIZER_PHP
  ;;          #];------------------------ `re-close'
  ;;          |
  ;;          |________ Suppose `point' is here (#)
  ;;
  (save-excursion
    (if (looking-at-p re-close)
        ;; Closing bracket on a line by itself
        (progn
          (goto-char pt-start)
          ;; The code below will check for the following list styles:
          ;;
          ;; // array
          ;; let attributes = [ "type" : "text/css",
          ;;                    "href" : "main.css",
          ;;                    "rel"  : "stylesheet" ];
          ;;
          ;; // arguments list
          ;; create_instance_params( definition,
          ;;                         options );
          (if (save-excursion (forward-char) (eolp))
              (current-indentation)
            (current-column)))
      ;; Otherwise, use normal indentation if the `point' is at the
      ;; end of the line:
      ;;
      ;; // array
      ;; let logger = [
      ;;     Logger::ALERT    : LOG_ALERT,
      ;;     Logger::CRITICAL : LOG_CRIT,
      ;;     [ 1 ],
      ;;     [
      ;;         foo,
      ;;         bar
      ;;     ]
      ;; ];
      ;;
      ;; // argument list
      ;; this->interpolate(
      ;;     item->getMessage(),
      ;;     item->getContext()
      ;; );
      (goto-char pt-start)
      (forward-char 1)
      (if (eolp)
          (+ (current-indentation) zephir-indent-level)
        ;; Othewise, align as described above
        (re-search-forward "\\S-")
        (forward-char -1)
        (current-column)))))

(defun zephir--proper-indentation (ctx)
  "Return the proper indentation for the current line.
This uses CTX as a current parse state."
  (save-excursion
    ;; Move `point' to the first non-whitespace character in the current line
    (back-to-indentation)

    (cond
     ;; Inside multiline string.
     ;; If we're inside a string and strings aren't to be indented,
     ;; return current indentation.
     ((zephir-in-string-p) (current-indentation))

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

     ;; When `point' is inside an innermost parenthetical grouping
     ((let ((array-start (zephir-in-ipg "\\["))
            (arglist-start (zephir-in-ipg "("))
            (block-start (zephir-in-ipg "{")))
        (cond
         (array-start (zephir-indent-listlike array-start "]"))
         (arglist-start (zephir-indent-listlike arglist-start ")"))
         (block-start (zephir-indent-block block-start)))))

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

(provide 'zephir-indent)
;;; zephir-indent.el ends here
