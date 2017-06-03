;;; fsharp-mode-font.el --- Syntax highlighting for F#

;; Copyright (C) 1997 INRIA

;; Author: 1993-1997 Xavier Leroy, Jacques Garrigue and Ian T Zimmerman
;;         2010-2011 Laurent Le Brun <laurent@le-brun.eu>
;; Maintainer: Robin Neatherway <robin.neatherway@gmail.com>
;; Keywords: languages

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

;;; Commentary:

;;; Code:

;; (require 'fsharp-mode)
;; (require 'dash)

(defgroup fsharp-ui nil
  "F# UI group for the defcustom interface."
  :prefix "fsharp-ui-"
  :group 'fsharp
  :package-version '(fsharp-mode . "1.9.2"))

(defface fsharp-ui-generic-face
  '((t (:inherit default)))
  "Preprocessor face"
  :group 'fsharp-ui)

(defface fsharp-ui-operator-face
  '((t (:foreground "LightSkyBlue")))
  "Preprocessor face"
  :group 'fsharp-ui)

(defface fsharp-ui-warning-face
  '((t (:inherit font-lock-warning-face)))
  "Face for warnings."
  :group 'fsharp-ui)

(defface fsharp-ui-error-face
  '((t (:inherit font-lock-error-face :underline t)))
  "Face for errors"
  :group 'fsharp-ui)

(defmacro def-fsharp-compiled-var (sym init &optional docstring)
  "Defines a SYMBOL as a constant inside an eval-and-compile form
with initial value INITVALUE and optional DOCSTRING."
  `(eval-and-compile
     (defvar ,sym ,init ,docstring)))

(def-fsharp-compiled-var fsharp-shebang-regexp
  "\\(^#!.*?\\)\\([A-Za-z0-9_-]+\\)$"
  "Capture the #! and path of a shebag in one group and the
  executable in another.")

(def-fsharp-compiled-var fsharp-access-control-regexp
  "private\\s-+\\|internal\\s-+\\|public\\s-+"
  "Match `private', `internal', or `public', followed by a space,
  with no capture.")

(def-fsharp-compiled-var fsharp-access-control-regexp-noncapturing
  (format "\\(?:%s\\)" fsharp-access-control-regexp)
  "Same as `fsharp-access-control-regexp', but captures")

(def-fsharp-compiled-var fsharp-inline-rec-regexp
  "inline\\s-+\\|rec\\s-+"
  "Match `inline' or `rec', followed by a space.")

(def-fsharp-compiled-var fsharp-inline-rec-regexp-noncapturing
  (format "\\(?:%s\\)" fsharp-inline-rec-regexp)
  "Match `inline' or `rec', followed by a space, with no capture.")

(def-fsharp-compiled-var fsharp-valid-identifier-regexp
  "[A-Za-z0-9_']+"
  "Match a normal, valid F# identifier -- alphanumeric characters
  plus ' and underbar. Does not capture")

(def-fsharp-compiled-var fsharp-function-def-regexp
  (concat "\\<\\(?:let\\|and\\|with\\)\\s-+"
          fsharp-inline-rec-regexp-noncapturing "?"
          (format "\\(%s\\)" fsharp-valid-identifier-regexp)
          "\\(?:\\s-+[A-Za-z_]\\|\\s-*(\\)" ;; matches function arguments or open-paren; unclear why 0-9 not in class
          ))

(def-fsharp-compiled-var fsharp-pattern-function-regexp
  (concat "\\<\\(?:let\\|and\\)\\s-+"
          fsharp-inline-rec-regexp-noncapturing "?"
          (format "\\(%s\\)" fsharp-valid-identifier-regexp)
          "\\s-*=\\s-*function")
  "Matches an implicit matcher, eg let foo m = function | \"cat\" -> etc.")

;; Note that this regexp is used for iMenu. To font-lock active patterns, we
;; need to use an anchored match in fsharp-font-lock-keywords.
(def-fsharp-compiled-var fsharp-active-pattern-regexp
  "\\<\\(?:let\\|and\\)\\s-+\\(?:\\(?:inline\\|rec\\)\\s-+\\)?(\\(|[A-Za-z0-9_'|]+|\\))\\(?:\\s-+[A-Za-z_]\\|\\s-*(\\)")

(def-fsharp-compiled-var fsharp-member-access-regexp
  "\\<\\(?:override\\|member\\|abstract\\)\\s-+"
  "Matches members declarations and modifiers on classes.")

(def-fsharp-compiled-var fsharp-member-function-regexp
  (concat fsharp-member-access-regexp
          "\\(?:\\(?:inline\\|rec\\)\\s-+\\)?\\(?:"
          fsharp-valid-identifier-regexp
          "\\.\\)?\\("
          fsharp-valid-identifier-regexp
          "\\)")
  "Captures the final identifier in a member function declaration.")

(def-fsharp-compiled-var fsharp-overload-operator-regexp
  (concat fsharp-member-access-regexp
          "\\(?:\\(?:inline\\|rec\\)\\s-+\\)?\\(([!%&*+-./<=>?@^|~]+)\\)")
  "Match operators when overloaded by a type/class.")

(def-fsharp-compiled-var fsharp-constructor-regexp
  "^\\s-*\\<\\(new\\) *(.*)[^=]*="
  "Matches the `new' keyword in a constructor")

(def-fsharp-compiled-var fsharp-type-def-regexp
  (concat "^\\s-*\\<\\(?:type\\|inherit\\)\\s-+"
          fsharp-access-control-regexp-noncapturing "*" ;; match access control 0 or more times
          "\\([A-Za-z0-9_'.]+\\)"))

(def-fsharp-compiled-var fsharp-var-or-arg-regexp
  "\\_<\\([A-Za-z_][A-Za-z0-9_']*\\)\\_>")

(def-fsharp-compiled-var fsharp-explicit-field-regexp
  (concat "^\\s-*\\(?:val\\|abstract\\)\\s-*\\(?:mutable\\s-+\\)?"
          fsharp-access-control-regexp-noncapturing "*" ;; match access control 0 or more times
          "\\([A-Za-z_][A-Za-z0-9_']*\\)\\s-*:\\s-*\\([A-Za-z_][A-Za-z0-9_'<> \t]*\\)"))

(def-fsharp-compiled-var fsharp-attributes-regexp
  "\\[<[A-Za-z0-9_]+>\\]"
  "Match attributes like [<EntryPoint>]")


;; F# makes extensive use of operators, many of which have some kind of
;; structural significance.
;;
;; In particular:
;; (| ... |)                 -- banana clips for Active Patterns (handled separately)
;; <@ ... @> and <@@ ... @@> -- quoted expressions
;; <| and |>                 -- left and right pipe (also <||, <|||, ||>, |||>)
;; << and >>                 -- function composition
;; |                         -- match / type expressions

(def-fsharp-compiled-var fsharp-operator-quote-regexp
  "\\(<@\\{1,2\\}\\)\\(?:.*\\)\\(@\\{1,2\\}>\\)"
  "Font lock <@/<@@ and @>/@@> operators.")

(def-fsharp-compiled-var fsharp-operator-pipe-regexp
  "<|\\{1,3\\}\\||\\{1,3\\}>"
  "Match the full range of pipe operators -- |>, ||>, |||>, etc.")

(def-fsharp-compiled-var fsharp-operator-case-regexp
  "\\s-+\\(|\\)[A-Za-z0-9_' ]"
  "Match literal | in contexts like match and type declarations.")

(defvar fsharp-imenu-generic-expression
  `((nil ,(concat "^\\s-*" fsharp-function-def-regexp) 1)
    (nil ,(concat "^\\s-*" fsharp-pattern-function-regexp) 1)
    (nil ,(concat "^\\s-*" fsharp-active-pattern-regexp) 1)
    (nil ,(concat "^\\s-*" fsharp-member-function-regexp) 1)
    (nil ,(concat "^\\s-*" fsharp-overload-operator-regexp) 1)
    (nil ,fsharp-constructor-regexp 1)
    (nil ,fsharp-type-def-regexp 1)
    )
  "Provide iMenu support through font-locking regexen.")

(defun fsharp-imenu-load-index ()
  "Hook up the provided regexen to enable imenu support."
  (setq imenu-generic-expression fsharp-imenu-generic-expression))

(add-hook 'fsharp-mode-hook #'fsharp-imenu-load-index)

(defun fsharp-var-pre-form ()
  (save-excursion
    (re-search-forward "\\(:\\s-*\\w[^)]*\\)?=" nil t)
    (match-beginning 0)))

(defun fsharp-fun-pre-form ()
  (save-excursion
    (search-forward "->")))

;; Preprocessor directives (3.3)
(def-fsharp-compiled-var fsharp-ui-preproessor-directives
  '("#if" "#else" "#endif" "#light"))

;; Compiler directives (12.4)
(def-fsharp-compiled-var fsharp-ui-compiler-directives
  '("#nowarn" "#load" "#r" "#reference" "#I"
    "#Include" "#q" "#quit" "#time" "#help"))

;; Lexical matters (18.4)
(def-fsharp-compiled-var fsharp-ui-lexical-matters
  '("#indent"))

;; Line Directives (3.9)
(def-fsharp-compiled-var fsharp-ui-line-directives
  '("#line"))

;; Identifier replacements (3.11)
(def-fsharp-compiled-var fsharp-ui-identifier-replacements
  '("__SOURCE_DIRECTORY__" "__SOURCE_FILE__" "__LINE__"))

;; F# keywords (3.4)
(def-fsharp-compiled-var fsharp-ui-fsharp-threefour-keywords
  '("abstract" "and" "as" "assert" "base" "begin"
    "class" "default" "delegate" "do" "do!" "done"
    "downcast" "downto" "elif" "else" "end"
    "exception" "extern" "false" "finally" "for" "fun"
    "function" "global" "if" "in" "inherit" "inline"
    "interface" "internal" "lazy" "let" "let!"
    "match" "member" "module" "mutable" "namespace"
    "new" "not" "null" "of" "open" "or" "override"
    "private" "public" "rec" "return" "return!"
    "select" "static" "struct" "then" "to" "true"
    "try" "type" "upcast" "use" "use!"  "val" "void"
    "when" "while" "with" "yield" "yield!"))

;; "Reserved because they are reserved in OCaml"
(def-fsharp-compiled-var fsharp-ui-ocaml-reserved-words
  '("asr" "land" "lor" "lsl" "lsr" "lxor" "mod" "sig"))

;; F# reserved words for future use
(def-fsharp-compiled-var fsharp-ui-reserved-words
  '("atomic" "break" "checked" "component" "const"
    "constraint" "constructor" "continue" "eager"
    "event" "external" "fixed" "functor" "include"
    "method" "mixin" "object" "parallel" "process"
    "protected" "pure" "sealed" "tailcall" "trait"
    "virtual" "volatile"))

;; RMD 2016-09-30 -- This was pulled out separately with the following comment
;; when I got here. Not clear to me why it's on it's own, or even precisely what
;; the comment means. But: `async' is a valid F# keyword and needs to go someplace,
;; so I've left it here. For now.
;;
;; Workflows not yet handled by fsautocomplete but async
;; always present
(def-fsharp-compiled-var fsharp-ui-async-words
  '("async")
  "Just the word async, in a list.")

(def-fsharp-compiled-var fsharp-ui-word-list-regexp
  (regexp-opt
   `(,@fsharp-ui-async-words
     ,@fsharp-ui-compiler-directives
     ,@fsharp-ui-fsharp-threefour-keywords
     ,@fsharp-ui-identifier-replacements
     ,@fsharp-ui-lexical-matters
     ,@fsharp-ui-ocaml-reserved-words
     ,@fsharp-ui-preproessor-directives
     ,@fsharp-ui-reserved-words
     ,@fsharp-ui-line-directives)
   'symbols))

(defconst fsharp-font-lock-keywords
  (eval-when-compile
    `((,fsharp-ui-word-list-regexp 0 font-lock-keyword-face)
      ;; shebang
      (,fsharp-shebang-regexp
       (1 font-lock-comment-face)
       (2 font-lock-keyword-face))
      ;; attributes
      (,fsharp-attributes-regexp . font-lock-preprocessor-face)
      ;; ;; type defines
      (,fsharp-type-def-regexp 1 font-lock-type-face)
      (,fsharp-function-def-regexp 1 font-lock-function-name-face)
      (,fsharp-pattern-function-regexp 1 font-lock-function-name-face)
      ;; Active records
      ("(|" (0 'fsharp-ui-operator-face)
       ("\\([A-Za-z'_]+\\)\\(|)\\)" nil nil
        (1 font-lock-function-name-face)
        (2 'fsharp-ui-operator-face)))
      (,fsharp-operator-pipe-regexp . 'fsharp-ui-operator-face)
      (,fsharp-member-function-regexp 1 font-lock-function-name-face)
      (,fsharp-overload-operator-regexp 1 font-lock-function-name-face)
      (,fsharp-constructor-regexp 1 font-lock-function-name-face)
      (,fsharp-operator-case-regexp 1 'fsharp-ui-operator-face)
      (,fsharp-operator-quote-regexp  (1 'fsharp-ui-operator-face)
                                      (2 'fsharp-ui-operator-face))
      ("[^:]:\\s-*\\(\\<[A-Za-z0-9_' ]*[^ ;\n,)}=<-]\\)\\(<[^>]*>\\)?"
       (1 font-lock-type-face)
       ;; 'prevent generic type arguments from being rendered in variable face
       (2 'fsharp-ui-generic-face nil t))
      (,(format "^\\s-*\\<\\(let\\|use\\|override\\|member\\|and\\|\\(?:%snew\\)\\)\\_>"
                (concat fsharp-access-control-regexp "*"))
       (0 font-lock-keyword-face) ; let binding and function arguments
       (,fsharp-var-or-arg-regexp
        (fsharp-var-pre-form) nil
        (1 font-lock-variable-name-face nil t)))
      ("\\<fun\\>"
       (0 font-lock-keyword-face) ; lambda function arguments
       (,fsharp-var-or-arg-regexp
        (fsharp-fun-pre-form) nil
        (1 font-lock-variable-name-face nil t)))
      (,fsharp-type-def-regexp
       (0 'font-lock-keyword-face) ; implicit constructor arguments
       (,fsharp-var-or-arg-regexp
        (fsharp-var-pre-form) nil
        (1 font-lock-variable-name-face nil t)))
      (,fsharp-explicit-field-regexp
       (1 font-lock-variable-name-face)
       (2 font-lock-type-face))

      ;; open namespace
      ("\\<open\s\\([A-Za-z0-9_.]+\\)" 1 font-lock-type-face)

      ;; module/namespace
      ("\\_<\\(?:module\\|namespace\\)\s\\([A-Za-z0-9_.]+\\)" 1 font-lock-type-face)
      )))

(defun fsharp-ui-setup-font-lock ()
  "Set up font locking for F# Mode."
  (setq font-lock-defaults
        '(fsharp-font-lock-keywords)))

(add-hook 'fsharp-mode-hook #'fsharp-ui-setup-font-lock)

(defun fsharp--syntax-propertize-function (start end)
  (goto-char start)
  (fsharp--syntax-string end)
  (funcall (syntax-propertize-rules
            ("\\(@\\)\"" (1 (prog1 "|" (fsharp--syntax-string end)))) ; verbatim string
            ("\\(\"\\)\"\"" (1 (prog1 "|" (fsharp--syntax-string end)))) ; triple-quoted string
            ("\\('\\)\\(?:[^\n\t\r\b\a\f\v\\\\]\\|\\\\[\"'ntrbafv\\\\]\\|\\\\u[0-9A-Fa-f]\\{4\\}\\|\\\\[0-9]\\{3\\}\\)\\('\\)"
             (1 "|") (2 "|")) ; character literal
            ("\\((\\)/" (1 "()"))
            ("\\(\(\\)\\*[!%&*+-\\./<=>@^|~?]*[\n\t\r\b\a\f\v ]*\)" (1 "()")) ; symbolic operator starting (* is not a comment
            ("\\(/\\)\\*" (1 ".")))
           start end))

(defun fsharp--syntax-string (end)
  (let* ((pst (syntax-ppss))
         (instr (nth 3 pst))
         (start (nth 8 pst)))
    (when (eq t instr) ; Then we are in a custom string
      (cond
       ((eq ?@ (char-after start)) ; Then we are in a verbatim string
        (while
            (when (re-search-forward "\"\"?" end 'move)
              (if (> (- (match-end 0) (match-beginning 0)) 1)
                  t ;; Skip this "" and keep looking further.
                (put-text-property (- (match-beginning 0) 1) (- (match-end 0) 1)
                                   'syntax-table (string-to-syntax "."))
                (put-text-property (match-beginning 0) (match-end 0)
                                   'syntax-table (string-to-syntax "|"))
                nil)))
        )

       (t ; Then we are in a triple-quoted string
        (when (re-search-forward "\"\"\"" end 'move)
          (put-text-property (- (match-beginning 0) 1) (match-beginning 0)
                             'syntax-table (string-to-syntax "."))
          (put-text-property (match-beginning 0) (match-end 0)
                             'syntax-table (string-to-syntax "|")))
        )))))

(provide 'fsharp-mode-font)

;;; fsharp-mode-font.el ends here
