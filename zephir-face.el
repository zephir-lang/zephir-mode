;;; zephir-face.el --- Face definitions for Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.6.0
;; URL: https://github.com/zephir-lang/zephir-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
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

;; Face definitions for Zephir code.

;;; Code:

;;;###autoload
(defgroup zephir-faces nil
  "Faces used in Zephir Mode"
  :tag "Zephir Faces"
  :group 'zephir
  :group 'faces)

(defface zephir-function-call-face '((t ()))
  "Zephir Mode face used to highlight function names in calles."
  :group 'zephir-faces
  :tag "Zephir Function Call")

(defface zephir-method-call-face '((t (:inherit zephir-function-call-face)))
  "Zephir Mode face used to highlight method names in calles."
  :group 'zephir-faces
  :tag "Zephir Method Call")

(provide 'zephir-face)
;;; zephir-face.el ends here
