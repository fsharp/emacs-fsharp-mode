;;; fsharp-mode-indent-smie.el --- SMIE indentation for F# -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2015 m00nlight Wang

;; Author: 2015 m00nlight Wang <dot.wangyushi@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'smie)


(defcustom fsharp-indent-level 2
  "Basic indentation step for fsharp mode"
  :type 'integer)

(defconst fsharp-smie-grammar
  ;; SMIE grammar follow the refernce of SML-mode. 
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((expr ("while" expr "do" expr)
	     ("if" expr "then" expr "else" expr)
	     ("for" expr "in" expr "do" expr)
	     ("for" expr "to" expr "do" expr)
	     ("try" expr "with" branches)
	     ("match" expr "with" branches))
       (branches (branches "|" branches)))
     '((assoc "|"))
     '((assoc "typef" "open")))))
  )

(defun fsharp-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) fsharp-indent-level)
    (`(:after . "do") fsharp-indent-level)
    (`(:after . "then") fsharp-indent-level)
    (`(:after . "else") fsharp-indent-level)
    (`(:after . "try") fsharp-indent-level)
    (`(:after . "with") fsharp-indent-level)
    (`(:after . "in") 0)
    (`(:after . "=") fsharp-indent-level)))

(defun fsharp-mode-indent-smie-setup ()
  (smie-setup fsharp-smie-grammar #'fsharp-smie-rules))


(provide 'fsharp-mode-indent-smie)
