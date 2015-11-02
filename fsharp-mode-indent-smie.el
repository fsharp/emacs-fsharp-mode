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


(defcustom fsharp-indent-level 4
  "Basic indentation step for fsharp mode"
  :group 'fsharp
  :type 'integer)

(defconst fsharp-smie-grammar
  ;; SMIE grammar follow the refernce of SML-mode. 
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (expr ("while" expr "do" expr)
	     ("if" expr "then" expr "else" expr)
	     ("for" expr "in" expr "do" expr)
	     ("for" expr "to" expr "do" expr)
	     ("try" expr "with" branches)
	     ("try" expr "finally" expr)
	     ("match" expr "with" branches)
	     ("type" expr "=" branches)
	     ("begin" exprs "end")
	     ("[" exprs "]")
	     ("[|" exprs "|]")
	     ("{" exprs "}")
	     ("<@" exprs "@>")
	     ("<@@" exprs "@@>")
	     ("let" sexp "=" expr)
	     ("fun" expr "->" expr))
       (sexp ("rec")
       	     (sexp ":" type)
       	     (sexp "||" sexp)
       	     (sexp "&&" sexp)
       	     ("(" exprs ")"))
       (exprs (exprs ";" exprs)
	      (exprs "," exprs)
	      (expr))
       (type (type "->" type)
       	     (type "*" type))
       (branches (branches "|" branches))
       (decls (sexp "=" expr))
       (toplevel (decls)
		 (expr)
		 (toplevel ";;" toplevel)))
     '((assoc "|"))
     '((assoc "->") (assoc "*"))
     '((assoc "let" "fun" "type" "open" "->"))
     '((assoc "let") (assoc "="))
     '((assoc "[" "]" "[|" "|]" "{" "}"))
     '((assoc "<@" "@>"))
     '((assoc "<@@" "@@>"))
     '((assoc "&&") (assoc "||") (noassoc ":"))
     '((assoc ";") (assoc ","))
     '((assoc ";;")))
    (smie-precs->prec2
     '((nonassoc (">" ">=" "<>" "<" "<=" "="))
       (assoc "::")
       (assoc "+" "-" "^")
       (assoc "/" "*" "%")))))
  )

(defun fsharp-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) fsharp-indent-level)
    (`(:after . "do") fsharp-indent-level)
    (`(:after . "then") fsharp-indent-level)
    (`(:after . "else") fsharp-indent-level)
    (`(:after . "try") fsharp-indent-level)
    (`(:after . "with") fsharp-indent-level)
    (`(:after . "finally") fsharp-indent-level)
    (`(:after . "in") 0)
    (`(:after . ,(or `"[" `"]" `"[|" `"|]")) fsharp-indent-level)
    (`(,_ . ,(or `";" `",")) (if (smie-rule-parent-p "begin")
				 0
			       (smie-rule-separator kind)))
    (`(:after . "=") fsharp-indent-level)
    (`(:after . ";;") (smie-rule-separator kind))
    (`(:before . ";;") (if (smie-rule-bolp)
			   0))
    ))


(defun fsharp-mode-indent-smie-setup ()
  (smie-setup fsharp-smie-grammar #'fsharp-smie-rules))


(provide 'fsharp-mode-indent-smie)
