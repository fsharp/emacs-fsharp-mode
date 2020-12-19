;;; fsharp-mode-indent.el --- Stucture Definition, Mark, and Motion for F#

;; Copyright (C) 2010 Laurent Le Brun

;; Author: 2010-2011 Laurent Le Brun <laurent@le-brun.eu>
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
;; This module defines variables and functions related to the structure of F#
;; code, and motion around and through that code. SMIE is used to set certain
;; default configurations. In particular, `smie' expects to set
;; `forward-sexp-function' and `indent-line-function', the latter of which we
;; currently override.
;;
;; SMIE configs by m00nlight Wang <dot.wangyushi@gmail.com>, 2015
;; Last major update by Ross Donaldson <@gastove>, 2020

;;; Code:

(require 'comint)
(require 'custom)
(require 'compile)
(require 'dash)
(require 'smie)

;;-------------------------- Customization Variables --------------------------;;

(defcustom fsharp-tab-always-indent t
  "*Non-nil means TAB in Fsharp mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'fsharp)

(defcustom fsharp-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[fsharp-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Fsharp code."
  :type 'integer
  :group 'fsharp)

(defalias 'fsharp-indent-level 'fsharp-indent-offset
  "Backwards-compatibility alias. `fsharp-indent-level' was
  configuring the same thing as `fsharp-indent-offset', but less
  clearly and in a different file, and free from update by
  functions like offset-guessing.")

(defcustom fsharp-continuation-offset 4
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line.  Only those continuation lines for a block opening
statement are given this extra offset."
  :type 'integer
  :group 'fsharp)

(defcustom fsharp-conservative-indentation-after-bracket nil
  "Indent by fsharp-continuation-offset also after an opening bracket.
The default indentation depth on a new line after an opening
bracket is one column further from the opening bracket. Indenting much less is
allowed, because brackets reset the current offside column."
  :type 'boolean
  :group 'fsharp)

(defcustom fsharp-smart-indentation t
  "*Should `fsharp-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `fsharp-mode':

    1. `fsharp-indent-offset' is guessed from existing code in the buffer.
       Only guessed values between 2 and 8 are considered.  If a valid
       guess can't be made (perhaps because you are visiting a new
       file), then the value in `fsharp-indent-offset' is used.

    2. `indent-tabs-mode' is turned off if `fsharp-indent-offset' does not
       equal `tab-width' (`indent-tabs-mode' is never turned on by
       Fsharp mode).  This means that for newly written code, tabs are
       only inserted in indentation if one tab is one indentation
       level, otherwise only spaces are used.

Note that both these settings occur *after* `fsharp-mode-hook' is run,
so if you want to defeat the automagic configuration, you must also
set `fsharp-smart-indentation' to nil in your `fsharp-mode-hook'."
  :type 'boolean
  :group 'fsharp)

(defcustom fsharp-honor-comment-indentation t
  "*Controls how comment lines influence subsequent indentation.

When nil, all comment lines are skipped for indentation purposes, and
if possible, a faster algorithm is used (i.e. X/Emacs 19 and beyond).

When t, lines that begin with a single `//' are a hint to subsequent
line indentation.  If the previous line is such a comment line (as
opposed to one that starts with `fsharp-block-comment-prefix'), then its
indentation is used as a hint for this line's indentation.  Lines that
begin with `fsharp-block-comment-prefix' are ignored for indentation
purposes.

When not nil or t, comment lines that begin with a single `//' are used
as indentation hints, unless the comment character is in column zero."
  :type '(choice
          (const :tag "Skip all comment lines (fast)" nil)
          (const :tag "Single // `sets' indentation for next line" t)
          (const :tag "Single // `sets' indentation except at column zero"
                 other)
          )
  :group 'fsharp)

(defcustom fsharp-backspace-function 'backward-delete-char-untabify
  "*Function called by `fsharp-electric-backspace' when deleting backwards."
  :type 'function
  :group 'fsharp)

(defcustom fsharp-delete-function 'delete-char
  "*Function called by `fsharp-electric-delete' when deleting forwards."
  :type 'function
  :group 'fsharp)


;;--------------------------------- Constants ---------------------------------;;
(defconst fsharp--hanging-operator-re
  (concat ".*\\(" (mapconcat 'identity
                             '("+" "-" "*" "/")
                             "\\|")
          "\\)$")
  "Regular expression matching unterminated algebra expressions.")


;; TODO[gastove|2019-10-22] This doesn't match (* long comments *), but it
;; *does* capture. When used with `looking-at-p', it also returns true on lines
;; that are neither blank nor comment.
(defconst fsharp-blank-or-comment-re "[ \t]*\\(//.*\\)?"
  "Regular expression matching a blank or comment line.")

(defconst fsharp-outdent-re
  (concat "\\(" (mapconcat 'identity
                           '("else"
                             "with"
                             "finally"
                             "end"
                             "done"
                             "elif"
                             "}")
                           "\\|")
          "\\)")
  "Regular expression matching statements to be dedented one level.")


(defconst fsharp-block-closing-keywords-re
  "\\(end\\|done\\|raise\\|failwith\\|failwithf\\|rethrow\\|exit\\)"
  "Regular expression matching keywords which typically close a block.")


(defconst fsharp-no-outdent-re
  (concat
   "\\("
   (mapconcat 'identity
              (list "try"
                    "while\\s +.*"
                    "for\\s +.*"
                    "then"
                    (concat fsharp-block-closing-keywords-re "[ \t\n]")
                    )
              "\\|")
   "\\)")
  "Regular expression matching lines not to dedent after.")


(defconst fsharp-block-opening-re
  (concat "\\(" (mapconcat 'identity
                           '("if"
                             "then"
                             "else"
                             "with"
                             "try"
                             "finally"
                             "class"
                             "struct"
                             "="        ; for example: let f x =
                             "->"
                             "do"
                             "function")
                           "\\|")
          "\\)")
  "Regular expression matching expressions which begin a block.
Captures the specific block opener matched.")


;; TODO: this regexp looks transparently like a python regexp. That means it's almost certainly wrong.
(defvar fsharp-parse-state-re
  (concat
   "^[ \t]*\\(elif\\|else\\|while\\|def\\|class\\)\\>"
   "\\|"
   "^[^ /\t\n]"))


(defsubst fsharp-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol  -- beginning of line
  eol  -- end of line
  bod  -- beginning of def or class
  eod  -- end of def or class
  bob  -- beginning of buffer
  eob  -- end of buffer
  boi  -- back to indentation
  bos  -- beginning of statement

This function preserves point and mark."
  (save-mark-and-excursion
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (fsharp-beginning-of-def-or-class 'either))
     ((eq position 'eod) (fsharp-end-of-def-or-class 'either))
     ((eq position 'bob) (point-min))
     ((eq position 'eob) (point-max))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (fsharp-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position)))

    (point)))


;;-------------------------------- Predicates --------------------------------;;

(defun fsharp-in-literal-p (&optional lim)
  "Return non-nil if point is in a Fsharp literal (a comment or
String). The return value is specifically one of the symbols
'comment or 'string. Optional argument LIM indicates the
beginning of the containing form, i.e. the limit on how far back
to scan."
  ;; NOTE: Watch out for infinite recursion between this function and
  ;; `fsharp-point'.
  (let* ((lim (or lim (fsharp-point 'bod)))
         (state (parse-partial-sexp lim (point))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment)
     ;; The above approach cannot tell if point is on the opening character of a
     ;; comment.
     ((looking-at comment-start) 'comment)
     (t nil))))


(defun fsharp-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (progn (back-to-indentation)
           (looking-at fsharp-outdent-re))
    ))


(defun fsharp--indenting-comment-p ()
  "Returns non-nil if point is in an indenting comment line, otherwise nil.

Definition: Indenting comment line. A line containing only a
comment, but which is treated like a statement for indentation
calculation purposes. Such lines are only treated specially by
the mode; they are not treated specially by the Fsharp
interpreter.

The first non-blank line following an indenting comment line is
given the same amount of indentation as the indenting comment
line.

All other comment-only lines are ignored for indentation
purposes.

Are we looking at a comment-only line which is *not* an indenting
comment line? If so, we assume that it's been placed at the
desired indentation, so leave it alone. Indenting comment lines
are aligned as statements."
  ;; TODO[gastove|2019-10-22] this is a bug. The regular expression here matches
  ;; comments only if there is *no whites space* between the // and the first
  ;; characters in the comment.
  (and (looking-at "[ \t]*//[^ \t\n]")
       (fboundp 'forward-comment)
       (<= (current-indentation)
           (save-excursion
             (forward-comment (- (point-max)))
             (current-indentation)))))


(defun fsharp--hanging-operator-continuation-line-p ()
  "Return t if point is on at least the *second* line of the
buffer, and the previous line matches `fsharp--hanging-operator-re' --
which is to say, it ends in +, -, /, or *."
  (save-excursion
    (beginning-of-line)
    (and
     (not (bobp))
     ;; make sure; since eq test passed, there is a preceding line
     (forward-line -1)                  ; always true -- side effect
     ;; matches any line, so long as it ends with one of +, -, *, or /
     (looking-at fsharp--hanging-operator-re))))


;; TODO[gastove|2019-10-31] This function doesn't do everything it needs to.
;; Currently, it only reports a continuation line if there's a hanging
;; arithmetic operator *or* if we're inside a delimited block (something like {}
;; or []). It _needs_ to also respect symbols that open a new whitespace block
;; -- things like -> at the end of a line, or |> at the beginning of one.
;;
;; The trick is: the other major place where |> and -> lines are considered is
;; in `fsharp-compute-indentation', which... catches "undelimited" blocks as a
;; default case. They aren't _explicitly_ detected.
;;
;; In all, this makes me think we need a cleaner distinction between a
;; "continuation line" and a "relative line" -- that is, a line that continues
;; an ongoing expression (a sequence of items in a list, the completion of an
;; arithmetic expression) and a new block scope opened by a single symbol and
;; terminated with whitespace.
;;
;; We do already have `fsharp-expression-opens-block-p', which we could make much
;; more active use of. However: `fsharp-expression-opens-block-p' calls
;; `fsharp-goto-beyond-current-expression', which... relies on
;; `fsharp-continuation-line-p'. So that will need untangling.
(defun fsharp-continuation-line-p ()
  "Return t if current line continues a line with a hanging
arithmetic operator *or* is inside a nesting construct (a list,
computation expression, etc)."
  (save-excursion
    (beginning-of-line)
    (or (fsharp--hanging-operator-continuation-line-p)
        (fsharp-nesting-level))))


(defun fsharp--previous-line-continuation-line-p ()
  "Returns true if previous line is a continuation line"
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at-p (concat "^" fsharp-blank-or-comment-re "$")))
      (forward-line -1))
    (fsharp-continuation-line-p)))


(defun fsharp--pipe-expression-p ()
  "Returns true if current expression is part of a multi-line
  pipe expression, like a discriminated union or a chain of
  left-pipes."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^\\s-*|\\{1,3\\}>?.*$")))


;; TODO[gastove|2019-11-01] The next two functions are really well intended, but
;; need to specifically know how to handle skipping over blocks and comments.
(defun fsharp--previous-line-pipe-expression-p ()
  "Returns true if the previous line is a pipe expression."
  (save-excursion
    (forward-line -1)
    (while (fsharp-in-literal-p)
      (forwrd-line -1))
    (fsharp--pipe-expression-p)))

(defun fsharp--next-line-pipe-expression-p ()
  "If the next line, skipping over comments and long string
blocks, is a pipe expression, return point at the start of that
pipe expression.."
  (save-excursion
    (forward-line 1)
    (fsharp--skip-past-comment-or-string)
    (when (fsharp--pipe-expression-p)
      (forward-whitespace 1)
      (point))))


(defun fsharp-expression-opens-block-p ()
  "Return t if the current expression opens a block. For instance:

if thing
    stuff()
else otherThing()

or:

fun _ ->

(Most blocks in F# *can* be expressed as single lines.)

Point should be at the start of an expression."
  (unless (looking-at-p "^\\s-+$")
    (save-excursion
      (let ((start (progn
                     (beginning-of-line)
                     (when (looking-at-p "\\s-+")
                      (forward-whitespace 1))
                     (point)))
            (finish (fsharp-point 'eol))
            (searching t)
            (answer nil))
        (goto-char start)
        ;; Keep searching until we're finished.
        (while searching
          (if (re-search-forward fsharp-block-opening-re finish t)
              (when (eq (point) finish)
                ;; sure looks like it opens a block -- but it might
                ;; be in a comment
                (setq searching nil)  ; search is done either way
                (setq answer (not (eq 'comment (fsharp-in-literal-p)))))

            ;; search failed: couldn't find a reason to believe we're opening a block.
            (setq searching nil)))
        answer))))

(defun fsharp--previous-line-opens-block-p ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at-p (concat "^" fsharp-blank-or-comment-re "$")))
      (forward-line -1))
    (fsharp-expression-opens-block-p)))


;; TODO[@gastove|2019-10-22]: the list of keywords this function claims to catch
;; does not at all match the keywords in the regexp it wraps.
(defun fsharp-statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (fsharp-goto-initial-line)
    (back-to-indentation)
    (prog1
        (looking-at (concat fsharp-block-closing-keywords-re "\\>"))
      (goto-char here))))


;;---------------------------- Electric Keystrokes ----------------------------;;

(defun fsharp-electric-colon (arg)
  "Insert a colon.
In certain cases the line is dedented appropriately.  If a numeric
argument ARG is provided, that many colons are inserted
non-electrically.  Electric behavior is inhibited inside a string or
comment."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  ;; are we in a string or comment?
  (if (save-excursion
        (let ((pps (parse-partial-sexp (save-excursion
                                         (fsharp-beginning-of-def-or-class)
                                         (point))
                                       (point))))
          (not (or (nth 3 pps) (nth 4 pps)))))
      (save-excursion
        (let ((here (point))
              (outdent 0)
              (indent (fsharp-compute-indentation t)))
          (if (and (not arg)
                   (fsharp-outdent-p)
                   (= indent (save-excursion
                               (fsharp-next-statement -1)
                               (fsharp-compute-indentation t)))
                   )
              (setq outdent fsharp-indent-offset))
          ;; Don't indent, only dedent.  This assumes that any lines
          ;; that are already dedented relative to
          ;; fsharp-compute-indentation were put there on purpose.  It's
          ;; highly annoying to have `:' indent for you.  Use TAB, C-c
          ;; C-l or C-c C-r to adjust.  TBD: Is there a better way to
          ;; determine this???
          (if (< (current-indentation) indent) nil
            (goto-char here)
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to (- indent outdent))
            )))))


;; Electric deletion
(defun fsharp-electric-backspace (arg)
  "Delete preceding character or levels of indentation.
Deletion is performed by calling the function in `fsharp-backspace-function'
with a single argument (the number of characters to delete).

If point is at the leftmost column, delete the preceding newline.

Otherwise, if point is at the leftmost non-whitespace character of a
line that is neither a continuation line nor a non-indenting comment
line, or if point is at the end of a blank line, this command reduces
the indentation to match that of the line that opened the current
block of code.  The line that opened the block is displayed in the
echo area to help you keep track of where you are.  With
\\[universal-argument] dedents that many blocks (but not past column
zero).

Otherwise the preceding character is deleted, converting a tab to
spaces if needed so that only a single column position is deleted.
\\[universal-argument] specifies how many characters to delete;
default is 1.

When used programmatically, argument ARG specifies the number of
blocks to dedent, or the number of characters to delete, as indicated
above."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (fsharp-continuation-line-p))

      (funcall fsharp-backspace-function arg)
    ;; else indent the same as the colon line that opened the block
    ;; force non-blank so fsharp-goto-block-up doesn't ignore it
    (insert-char ?* 1)
    (backward-char)
    (let ((base-indent 0)               ; indentation of base line
          (base-text "")                ; and text of base line
          (base-found-p nil))
      (save-excursion
        (while (< 0 arg)
          (condition-case nil           ; in case no enclosing block
              (progn
                (fsharp-goto-block-up 'no-mark)
                (setq base-indent (current-indentation)
                      base-text   (fsharp-suck-up-leading-text)
                      base-found-p t))
            (error nil))
          (setq arg (1- arg))))
      (delete-char 1)                   ; toss the dummy character
      (delete-horizontal-space)
      (indent-to base-indent)
      )))


(defun fsharp-electric-delete (arg)
  "Delete preceding or following character or levels of whitespace.

The behavior of this function depends on the variable
`delete-key-deletes-forward'.  If this variable is nil (or does not
exist, as in older Emacsen and non-XEmacs versions), then this
function behaves identically to \\[c-electric-backspace].

If `delete-key-deletes-forward' is non-nil and is supported in your
Emacs, then deletion occurs in the forward direction, by calling the
function in `fsharp-delete-function'.

\\[universal-argument] (programmatically, argument ARG) specifies the
number of characters to delete (default is 1)."
  (interactive "*p")
  (funcall fsharp-delete-function arg))


;; required for pending-del/delsel/delete-selection minor modes
(put 'fsharp-electric-colon 'delete-selection t) ;delsel
(put 'fsharp-electric-colon 'pending-delete   t) ;pending-del
(put 'fsharp-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'fsharp-electric-backspace 'pending-delete   'supersede) ;pending-del
(put 'fsharp-electric-delete    'delete-selection 'supersede) ;delsel
(put 'fsharp-electric-delete    'pending-delete   'supersede) ;pending-del


;;-------------------------------- Indentation --------------------------------;;
(defun fsharp--compute-indent-levels ()
  (let ((levels (list))
        ci)
    (save-excursion
      (while (not (or (bobp)
                      (eq ci 0)))
        (if (looking-at-p "^\\s-*$")
            (forward-line -1)
          (let ((last-seen (car levels))
                (ci (current-indentation)))
            (when (or (not last-seen)
                      (< ci last-seen))
              (setq levels (cons ci levels))))
          (forward-line -1)))
      (reverse levels))))

(defun fsharp-indent-line (&optional arg)
  "Fix the indentation of the current line according to Fsharp rules.
With \\[universal-argument] (programmatically, the optional argument
ARG non-nil), ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

This function is normally bound to `indent-line-function' so
\\[indent-for-tab-command] will call it."
  (interactive "P")
  (let* ((ci (current-indentation))
         (levels (fsharp--compute-indent-levels))
         (next-level-out (or (car (-filter (lambda (i) (> ci i)) levels)) fsharp-indent-offset))
         (move-to-indentation-p (<= (current-column) ci))
         (need (fsharp-compute-indentation (not arg)))
         (cc (current-column)))

    ;; dedent out a level if previous command was the same unless we're in
    ;; column 1
    (if (and (equal last-command this-command)
             (/= cc 0))
        (progn
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to next-level-out))

      ;; else
      ;; see if we need to dedent
      (if (fsharp-outdent-p)
          (setq need (- need fsharp-indent-offset)))

      (if (or fsharp-tab-always-indent
              move-to-indentation-p)
          (progn
            (when (/= ci need)
              ;; This when body was originally a save-excursion, which kinda makes
              ;; no sense? Why save-excursion while moving point?
              (beginning-of-line)
              (delete-horizontal-space)
              (indent-to need))
            (if move-to-indentation-p (back-to-indentation)))
        (insert-tab)))
    ))


;; NOTE[gastove|2019-10-25] An interesting point: this function is *only* ever
;; called if `open-bracket-pos' is non-nil; `open-bracket-pos' is generated by
;; `fsharp-nesting-level', which *only* returns non nil for non-string
;; characters. And yet: we don't just rely on `open-bracket-pos' as we compute
;; indentation, and I'm honestly not sure why.
(defun fsharp--compute-indentation-open-bracket (open-bracket-pos)
  "Computes indentation for a line within an open bracket expression."
  (save-excursion
    (let ((startpos (point))
          placeholder)
      ;; align with first item in list; else a normal
      ;; indent beyond the line with the open bracket
      (goto-char (1+ open-bracket-pos)) ; just beyond bracket
      ;; NOTE[gastove|2019-10-25] -- consider switching to a forward regexp search
      ;;     with a whitepsace character class.
      ;; is the first list item on the same line?
      (skip-chars-forward " \t")
      (if (and (null (memq (following-char) '(?\n ?# ?\\)))
               (not fsharp-conservative-indentation-after-bracket))
                                        ; yes, so line up with it
          (current-column)
        ;; here follows the else
        ;; first list item on another line, or doesn't exist yet
        ;; TODO[gastove|2019-10-25] this needs to skip past whitespace, newlines,
        ;; *and* comments. I'm not convinced it does.
        (forward-line 1)
        (while (and (< (point) startpos)
                    (looking-at "[ \t]*\\(//\\|[\n\\\\]\\)")) ; skip noise
          (forward-line 1))
        (if (and (< (point) startpos)
                 (/= startpos
                     (save-excursion
                       (goto-char (1+ open-bracket-pos))
                       (forward-comment (point-max))
                       (point))))
            ;; again mimic the first list item
            (current-indentation)
          ;; else they're about to enter the first item

          ;; NOTE[gastove|2019-10-25] Okay, this is all really hard to follow, but
          ;; I *think* what's going on here is:
          ;; - We go to the position of the opening bracket we're trying to compute indentation against.
          ;; - We set placeholder to point (meaning we set `placeholder' to `open-bracket-pos')
          ;; - We call a function that claims to go to the first line of a statement
          ;; - We call a function that I *believe* tries to take us to the opening delimiter of a matched pair
          ;; - We return the current indentation of *that*, plus indent offset
          ;; ... holy moly.
          (goto-char open-bracket-pos)
          (setq placeholder (point))
          (fsharp-goto-initial-line)
          (fsharp-goto-beginning-of-tqs
           (save-excursion (nth 3 (parse-partial-sexp
                                   placeholder (point)))))
          (+ (current-indentation) fsharp-indent-offset))))))


(defun fsharp--compute-indentation-continuation-line ()
  "Computes the indentation for a line which continues the line
above, but only when the previous line is not itself a continuation line."
  (save-excursion
    (forward-line -11)
    (let ((startpos (point))
          (open-bracket-pos (fsharp-nesting-level))
          endpos searching found state placeholder)

      ;; Started on 2nd line in block, so indent more. if base line is an
      ;; assignment with a start on a RHS, indent to 2 beyond the leftmost "=";
      ;; else skip first chunk of non-whitespace characters on base line, + 1 more
      ;; column
      (end-of-line)
      (setq endpos (point)
            searching t)
      (back-to-indentation)
      (setq startpos (point))
      ;; look at all "=" from left to right, stopping at first one not nested in a
      ;; list or string
      (while searching
        (skip-chars-forward "^=" endpos)
        (if (= (point) endpos)
            (setq searching nil)
          (forward-char 1)
          (setq state (parse-partial-sexp startpos (point)))
          (if (and (zerop (car state)) ; not in a bracket
                   (null (nth 3 state))) ; & not in a string
              (progn
                (setq searching nil) ; done searching in any case
                (setq found
                      (not (or
                            (eq (following-char) ?=)
                            (memq (char-after (- (point) 2))
                                  '(?< ?> ?!)))))))))
      (if (or (not found)       ; not an assignment
              (looking-at "[ \t]*\\\\")) ; <=><spaces><backslash>
          (progn
            (goto-char startpos)
            (skip-chars-forward "^ \t\n")))
      ;; if this is a continuation for a block opening
      ;; statement, add some extra offset.
      (+ (current-column) (if (fsharp-expression-opens-block-p)
                              fsharp-continuation-offset 0)
         1)
      )))

(defun fsharp--get-last-indentation ()
  "Tries to find the indentation of the last line of code,
searching up from point."
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (while (and
            (or (looking-at "^\\s-*$")
                (fsharp--indenting-comment-p))
            (not (bobp)))
      (forward-line -1))
    (current-indentation)))

(defun fsharp--compute-indentation-relative-to-previous (honor-block-close-p)
  "Indentation based on that of the statement that precedes us;
use the first line of that statement to establish the base, in
case the user forced a non-std indentation for the continuation
lines (if any)"
  ;; skip back over blank & non-indenting comment lines note:
  ;; will skip a blank or non-indenting comment line that
  ;; happens to be a continuation line too.
  (save-excursion
    (let ((bod (fsharp-point 'bod))
          (last-indentation (fsharp--get-last-indentation))
          placeholder)
      (unless fsharp-honor-comment-indentation
        (forward-comment (- (point-max))))

      ;; if we landed inside a string, go to the beginning of that
      ;; string. this handles triple quoted, multi-line spanning
      ;; strings.
      (fsharp-goto-beginning-of-tqs (nth 3 (parse-partial-sexp bod (point))))
      ;; now skip backward over continued lines
      (setq placeholder (point))
      (fsharp-goto-initial-line)
      ;; we may *now* have landed in a TQS, so find the beginning of
      ;; this string.
      (fsharp-goto-beginning-of-tqs
       (save-excursion (nth 3 (parse-partial-sexp
                               placeholder (point)))))
      ;; Okay. We have three basic cases:
      ;;
      ;; - Increase indent
      ;; - Decrease indent
      ;; - Stay the same
      ;;
      ;; Given how F# works, I think the most sensible approach is to default to
      ;; *staying the same* in most cases.
      (+ last-indentation
         ;; TODO: this appears to be where things are broken, oh boy
         (cond
          ((or (fsharp-expression-opens-block-p)
               (fsharp--previous-line-opens-block-p)) fsharp-indent-offset)
          ((and honor-block-close-p (fsharp-statement-closes-block-p))
           (- fsharp-indent-offset))
          ;; Default case, don't increase over current
          (t 0)))
      )))


(defun fsharp-newline-and-indent ()
  "Strives to act like the Emacs `newline-and-indent'.
This is just `strives to' because correct indentation can't be computed
from scratch for Fsharp code.  In general, deletes the whitespace before
point, inserts a newline, and takes an educated guess as to how you want
the new line indented."
  (interactive)
  (let ((ci (current-indentation)))
    (if (< ci (current-column))                 ; if point beyond indentation
        (newline-and-indent)
      ;; else try to act like newline-and-indent "normally" acts
      (beginning-of-line)
      (insert-char ?\n 1)
      (move-to-column ci))))


(defun fsharp-compute-indentation (honor-block-close-p)
  "Compute Fsharp indentation.
When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of
dedenting."
  (save-excursion
    (beginning-of-line)
    (let* ((bod (fsharp-point 'bod))
           (pps (parse-partial-sexp bod (point)))
           (boipps (parse-partial-sexp bod (fsharp-point 'boi)))
           (open-bracket-pos (fsharp-nesting-level)))

      (cond
       ;; Continuation Lines
       ((fsharp-continuation-line-p)
        (if open-bracket-pos
            ;; There are a set of circumstances in which one might write, for
            ;; instance, a list of functions that need evaluating. Think of
            ;; Expecto tests, often written like:
            ;; [<Tests>]
            ;; let tests =
            ;;     testList "Serialization Round-Tripping"
            ;;         [ testCase "Some description" <| fun _ ->
            ;;               Expecto.equal 1 1
            ;;           testCase "Other description" <| fun _ ->
            ;;               Expecto.equal true false ]
            ;;
            ;; In this case, `open-bracket-pos' will be true, *but* we want to
            ;; indent a level deeper than that usually implies. If one were to
            ;; want to add another assertion after `Expecto.equal 1 1', for
            ;; instance, we'd want to be at the indent level of the E in
            ;; Expecto, not of the opening square bracket.
            ;;
            ;; Thus, we compute both numbers and return the max, letting the
            ;; programmer tab back a level as needed.
            (progn
              ;; (message "Open bracket is %s" (fsharp--compute-indentation-open-bracket open-bracket-pos))
              ;; (message "Relative to previous is %s" (fsharp--compute-indentation-relative-to-previous honor-block-close-p))
              ;; (message "Last is %s" (fsharp--get-last-indentation))
              (max
               (fsharp--compute-indentation-open-bracket open-bracket-pos)
               ;; (fsharp--compute-indentation-relative-to-previous honor-block-close-p)
               (fsharp--get-last-indentation)))
          (fsharp--compute-indentation-continuation-line)))

       ;; Previous line is a continuation line, use indentation of previous line
       ((fsharp--previous-line-continuation-line-p)
        (while (and (not (bobp))
                    (looking-at-p (concat "^" fsharp-blank-or-comment-re "$")))
          (forward-line -1))
        (current-indentation))

       ((or
         ;; Beginning of Buffer; not on a continuation line
         (bobp)
         ;; "Indenting Comment"
         (fsharp--indenting-comment-p))
        (current-indentation))

       ;; Final case includes things like pipe expressions, anonymous functions,
       ;; long assignment expressions, and if/else blocks.
       ;;
       ;; else indentation based on that of the statement that
       ;; precedes us; use the first line of that statement to
       ;; establish the base, in case the user forced a non-std
       ;; indentation for the continuation lines (if any)
       (t
        (fsharp--compute-indentation-relative-to-previous honor-block-close-p))))))

(defun fsharp-guess-indent-offset (&optional global)
  "Guess a good value for, and change, `fsharp-indent-offset'.

By default, make a buffer-local copy of `fsharp-indent-offset' with the
new value, so that other Fsharp buffers are not affected.  With
\\[universal-argument] (programmatically, optional argument GLOBAL),
change the global value of `fsharp-indent-offset'.  This affects all
Fsharp buffers (that don't have their own buffer-local copy), both
those currently existing and those created later in the Emacs session.

Some people use a different value for `fsharp-indent-offset' than you use.
There's no excuse for such foolishness, but sometimes you have to deal
with their ugly code anyway.  This function examines the file and sets
`fsharp-indent-offset' to what it thinks it was when they created the
mess.

Specifically, it searches forward from the statement containing point,
looking for a line that opens a block of code.  `fsharp-indent-offset' is
set to the difference in indentation between that line and the Fsharp
statement following it.  If the search doesn't succeed going forward,
it's tried again going backward."
  (interactive "P")                     ; raw prefix arg
  (let (new-value
        (start (point))
        (restart (point))
        (found nil)
        colon-indent)
    (fsharp-goto-initial-line)
    (while (not (or found (eobp)))
      (when (and (re-search-forward fsharp-block-opening-re nil 'move)
                 (not (fsharp-in-literal-p restart)))
        (setq restart (point))
        (fsharp-goto-initial-line)
        (if (fsharp-expression-opens-block-p)
            (setq found t)
          (goto-char restart))))
    (unless found
      (goto-char start)
      (fsharp-goto-initial-line)
      (while (not (or found (bobp)))
        (setq found (and
                     (re-search-backward fsharp-block-opening-re nil 'move)
                     (or (fsharp-goto-initial-line) t) ; always true -- side effect
                     (fsharp-expression-opens-block-p)))))
    (setq colon-indent (current-indentation)
          found (and found (zerop (fsharp-next-statement 1)))
          new-value (- (current-indentation) colon-indent))
    (goto-char start)
    (if (not found)
        (message "Unable to determine default value for fsharp-indent-offset")
      (funcall (if global 'kill-local-variable 'make-local-variable)
               'fsharp-indent-offset)
      (setq fsharp-indent-offset new-value)
      (or noninteractive
          (message "%s value of fsharp-indent-offset set to %d"
                   (if global "Global" "Local")
                   fsharp-indent-offset)))
    ))

(defun fsharp-comment-indent-function ()
  "Fsharp version of `comment-indent-function'."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (fsharp-point 'eol)))
      (and comment-start-skip
           (re-search-forward comment-start-skip eol t)
           (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1)))
      )))

(defun fsharp-narrow-to-defun (&optional class)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `fsharp-beginning-of-def-or-class'."
  (interactive "P")
  (save-excursion
    (widen)
    (fsharp-end-of-def-or-class class)
    (let ((end (point)))
      (fsharp-beginning-of-def-or-class class)
      (narrow-to-region (point) end))))


(defun fsharp-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun fsharp-shift-region-left (start end &optional count)
  "Shift region of Fsharp code to the left.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the left, by `fsharp-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, dedent only the current line.
You cannot dedent the region if any line is already at column zero."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (fsharp-shift-region start end (- (prefix-numeric-value
                                     (or count fsharp-indent-offset)))))


(defun fsharp-shift-region-right (start end &optional count)
  "Shift region of Fsharp code to the right.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the right, by `fsharp-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, indent only the current line."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (fsharp-shift-region start end (prefix-numeric-value
                                  (or count fsharp-indent-offset))))


(defun fsharp-indent-region (start end &optional indent-offset)
  "Reindent a region of Fsharp code.

The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
reindented.  If the first line of the region has a non-whitespace
character in the first column, the first line is left alone and the
rest of the region is reindented with respect to it.  Else the entire
region is reindented with respect to the (closest code or indenting
comment) statement immediately preceding the region.

This is useful when code blocks are moved or yanked, when enclosing
control structures are introduced or removed, or to reformat code
using a new value for the indentation offset.

If a numeric prefix argument is given, it will be used as the value of
the indentation offset.  Else the value of `fsharp-indent-offset' will be
used.

Warning: The region must be consistently indented before this function
is called!  This function does not compute proper indentation from
scratch (that's impossible in Fsharp), it merely adjusts the existing
indentation to be correct in context.

Warning: This function really has no idea what to do with
non-indenting comment lines, and shifts them as if they were indenting
comment lines.  Fixing this appears to require telepathy.

Special cases: whitespace is deleted from blank lines; continuation
lines are shifted by the same amount their initial line was shifted,
in order to preserve their relative indentation with respect to their
initial line; and comment lines beginning in column 1 are ignored."
  (interactive "*r\nP")                         ; region; raw prefix arg
  (save-excursion
    (goto-char end)   (beginning-of-line) (setq end (point-marker))
    (goto-char start) (beginning-of-line)
    (let ((fsharp-indent-offset (prefix-numeric-value
                                 (or indent-offset fsharp-indent-offset)))
          (indents '(-1))               ; stack of active indent levels
          (target-column 0)             ; column to which to indent
          (base-shifted-by 0)           ; amount last base line was shifted
          (indent-base (if (looking-at "[ \t\n]")
                           (fsharp-compute-indentation t)
                         0))
          ci)
      (while (< (point) end)
        (setq ci (current-indentation))
        ;; figure out appropriate target column
        (cond
         ((or (looking-at "//")         ; comment in column 1
              (looking-at "[ \t]*$"))   ; entirely blank
          (setq target-column 0))
         ((fsharp-continuation-line-p)  ; shift relative to base line
          (setq target-column (+ ci base-shifted-by)))
         (t                             ; new base line
          (if (> ci (car indents))      ; going deeper; push it
              (setq indents (cons ci indents))
            ;; else we should have seen this indent before
            (setq indents (memq ci indents)) ; pop deeper indents
            (if (null indents)
                (error "Bad indentation in region, at line %d"
                       (save-restriction
                         (widen)
                         (1+ (count-lines 1 (point)))))))
          (setq target-column (+ indent-base
                                 (* fsharp-indent-offset
                                    (- (length indents) 2))))
          (setq base-shifted-by (- target-column ci))))
        ;; shift as needed
        (if (/= ci target-column)
            (progn
              (delete-horizontal-space)
              (indent-to target-column)))
        (forward-line 1))))
  (set-marker end nil))


;;------------------------------ Motion and Mark ------------------------------;;

(defun fsharp-previous-statement (count)
  "Go to the start of the COUNTth preceding Fsharp statement.
By default, goes to the previous statement.  If there is no such
statement, goes to the first statement.  Return count of statements
left to move.  `Statements' do not include blank, comment, or
continuation lines."
  (interactive "p")                     ; numeric prefix arg
  (if (< count 0) (fsharp-next-statement (- count))
    (fsharp-goto-initial-line)
    (let (start)
      (while (and
              (setq start (point))      ; always true -- side effect
              (> count 0)
              (zerop (forward-line -1))
              (fsharp-goto-statement-at-or-above))
        (setq count (1- count)))
      (if (> count 0) (goto-char start)))
    count))

(defun fsharp-next-statement (count)
  "Go to the start of next Fsharp statement.
If the statement at point is the i'th Fsharp statement, goes to the
start of statement i+COUNT.  If there is no such statement, goes to the
last statement.  Returns count of statements left to move.  `Statements'
do not include blank, comment, or continuation lines."
  (interactive "p")                     ; numeric prefix arg
  (if (< count 0) (fsharp-previous-statement (- count))
    (beginning-of-line)
    (let (start)
      (while (and
              (setq start (point))      ; always true -- side effect
              (> count 0)
              (fsharp-goto-statement-below))
        (setq count (1- count)))
      (if (> count 0) (goto-char start)))
    count))

;; TODO[gastove|2019-10-31] This errors, and hard, if point is already on the
;; start of the current block. This should be a noop, with no error.
(defun fsharp-goto-block-up (&optional nomark)
  "Move up to start of current block.
Go to the statement that starts the smallest enclosing block; roughly
speaking, this will be the closest preceding statement that ends with a
colon and is indented less than the statement you started on.  If
successful, also sets the mark to the starting point.

`\\[fsharp-mark-block]' can be used afterward to mark the whole code
block, if desired.

If called from a program, the mark will not be set if optional argument
NOMARK is not nil."
  (interactive)
  (let ((start (point))
        (found nil)
        initial-indent)
    (fsharp-goto-initial-line)
    ;; if on and (mutually recursive bindings), blank or non-indenting comment line, use the preceding stmt
    (when (or (looking-at "[ \t]*\\($\\|//[^ \t\n]\\)")
              (looking-at-p "[ \t]*and[ \t]+"))
      (fsharp-goto-statement-at-or-above)
      (setq found (fsharp-expression-opens-block-p)))
    ;; search back for colon line indented less
    (setq initial-indent (current-indentation))
    (if (zerop initial-indent)
        ;; force fast exit
        (goto-char (point-min)))
    (while (not (or found (bobp)))
      (setq found
            (and
             (re-search-backward fsharp-block-opening-re nil 'move)
             (or (fsharp-goto-initial-line) t) ; always true -- side effect
             (< (current-indentation) initial-indent)
             (fsharp-expression-opens-block-p))))
    (if found
        (progn
          (or nomark (push-mark start))
          (back-to-indentation))
      (goto-char start)
      (when (called-interactively-p)
        (error "Enclosing block not found")))))


;; TODO[gastove|2019-11-01] F# doesn't have defs. Name needs fixing, and this
;; thing needs to get under test, see if it does what it claims.
(defun fsharp-beginning-of-def-or-class (&optional class count)
  "Move point to start of `def' or `class'.

Searches back for the closest preceding `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth start of `def'.

If point is in a `def' statement already, and after the `d', simply
moves point to the start of the statement.

Otherwise (i.e. when point is not in a `def' statement, or at or
before the `d' of a `def' statement), searches for the closest
preceding `def' statement, and leaves point at its start.  If no such
statement can be found, leaves point at the start of the buffer.

Returns t iff a `def' statement is found by these rules.

Note that doing this command repeatedly will take you closer to the
start of the buffer each time.

To mark the current `def', see `\\[fsharp-mark-def-or-class]'."
  (interactive "P")                     ; raw prefix arg
  (setq count (or count 1))
  (let ((at-or-before-p (<= (current-column) (current-indentation)))
        (start-of-line (goto-char (fsharp-point 'bol)))
        (start-of-stmt (goto-char (fsharp-point 'bos)))
        (start-re (cond ((eq class 'either) "^[ \t]*\\(type\\|let\\)\\>")
                        (class "^[ \t]*type\\>")
                        (t "^[ \t]*let\\>")))
        )
    ;; searching backward
    (if (and (< 0 count)
             (or (/= start-of-stmt start-of-line)
                 (not at-or-before-p)))
        (end-of-line))
    ;; search forward
    (if (and (> 0 count)
             (zerop (current-column))
             (looking-at start-re))
        (end-of-line))
    (if (re-search-backward start-re nil 'move count)
        (goto-char (match-beginning 0)))))

;; Backwards compatibility
(defalias 'beginning-of-fsharp-def-or-class 'fsharp-beginning-of-def-or-class)

(defun fsharp-end-of-def-or-class (&optional class count)
  "Move point beyond end of `def' or `class' body.

By default, looks for an appropriate `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth end of `def'.

If point is in a `def' statement already, this is the `def' we use.

Else, if the `def' found by `\\[fsharp-beginning-of-def-or-class]'
contains the statement you started on, that's the `def' we use.

Otherwise, we search forward for the closest following `def', and use that.

If a `def' can be found by these rules, point is moved to the start of
the line immediately following the `def' block, and the position of the
start of the `def' is returned.

Else point is moved to the end of the buffer, and nil is returned.

Note that doing this command repeatedly will take you closer to the
end of the buffer each time.

To mark the current `def', see `\\[fsharp-mark-def-or-class]'."
  (interactive "P")                     ; raw prefix arg
  (if (and count (/= count 1))
      (fsharp-beginning-of-def-or-class (- 1 count)))
  (let ((start (progn (fsharp-goto-initial-line) (point)))
        (which (cond ((eq class 'either) "\\(type\\|let\\)")
                     (class "type")
                     (t "let")))
        (state 'not-found))
    ;; move point to start of appropriate def/class
    (if (looking-at (concat "[ \t]*" which "\\>")) ; already on one
        (setq state 'at-beginning)
      ;; else see if fsharp-beginning-of-def-or-class hits container
      (if (and (fsharp-beginning-of-def-or-class class)
               (progn (fsharp-goto-beyond-block)
                      (> (point) start)))
          (setq state 'at-end)
        ;; else search forward
        (goto-char start)
        (if (re-search-forward (concat "^[ \t]*" which "\\>") nil 'move)
            (progn (setq state 'at-beginning)
                   (beginning-of-line)))))
    (cond
     ((eq state 'at-beginning) (fsharp-goto-beyond-block) t)
     ((eq state 'at-end) t)
     ((eq state 'not-found) nil)
     (t (error "Internal error in `fsharp-end-of-def-or-class'")))))


;; Helper functions


;; TODO: we only return the parse state if we are *not* inside a string. This
;; doesn't make a lot of sense; checking for being inside a triple-quoted string
;; is a thing we frequently need to do. Need to figure out a reason and/or
;; abstract over the top of this.
(defun fsharp-parse-state ()
  "Return the parse state at point (see `parse-partial-sexp' docs)."
  (save-excursion
    (let ((here (point))
          pps done)
      (while (not done)
        ;; back up to the first preceding line (if any; else start of
        ;; buffer) that begins with a popular Fsharp keyword, or a
        ;; non- whitespace and non-comment character.  These are good
        ;; places to start parsing to see whether where we started is
        ;; at a non-zero nesting level.  It may be slow for people who
        ;; write huge code blocks or huge lists ... tough beans.
        (re-search-backward fsharp-parse-state-re nil 'move)
        (beginning-of-line)
        ;; In XEmacs, we have a much better way to test for whether
        ;; we're in a triple-quoted string or not.  Emacs does not
        ;; have this built-in function, which is its loss because
        ;; without scanning from the beginning of the buffer, there's
        ;; no accurate way to determine this otherwise.
        ;;
        ;; NOTE[@gastove|2019-10-21]: it is not at *all* clear what this comment is on
        ;; about. Emacs has all the functions used in this function.
        (save-excursion (setq pps (parse-partial-sexp (point) here)))
        ;; make sure we don't land inside a triple-quoted string
        (setq done (or (not (nth 3 pps))
                       (bobp)))
        ;; Just go ahead and short circuit the test back to the
        ;; beginning of the buffer.  This will be slow, but not
        ;; nearly as slow as looping through many
        ;; re-search-backwards.
        (if (not done)
            (goto-char (point-min))))
      pps)))

(defun fsharp-nesting-level ()
  "Return the buffer position of the opening character of the
current enclosing pair. If nesting level is zero, return nil.

At time of writing, enclosing pair can be [], {} or (), but not
quotes (single or triple) or <>. Note that registering []
implicitly also registers [||], though the pipes are ignored."
  (let ((status (fsharp-parse-state)))
    (if (zerop (car status))
        nil                             ; not in a nest
      (car (cdr status)))))             ; char of open bracket


;; NOTE[gastove|2019-10-25] this function baffles me. A triple-quoted string is,
;; definitionally, always delimited by *triple quotes*. I suspect this function
;; of being something more akin to, "go to beginning of opening of pair", or
;; just "go to delimiter."
(defun fsharp-goto-beginning-of-tqs (delim)
  "Go to the beginning of the triple quoted string we find ourselves in.
DELIM is the TQS string delimiter character we're searching backwards
for."
  (let ((skip (and delim (make-string 1 delim)))
        (continue t))
    (when skip
      (save-excursion
        (while continue
          (search-backward skip nil t)
          (setq continue (and (not (bobp))
                              (= (char-before) ?\\))))
        (if (and (= (char-before) delim)
                 (= (char-before (1- (point))) delim))
            (setq skip (make-string 3 delim))))
      ;; we're looking at a triple-quoted string
      (search-backward skip nil t))))


(defun fsharp-goto-initial-line ()
  "Go to the initial line of the current statement.
Usually this is the line we're on, but if we're on the 2nd or
following lines of a continuation block, we need to go up to the first
line of the block."
  ;; Tricky: We want to avoid quadratic-time behavior for long
  ;; continued blocks, whether of the backslash or open-bracket
  ;; varieties, or a mix of the two.  The following manages to do that
  ;; in the usual cases.
  ;;
  ;; Also, if we're sitting inside a triple quoted string, this will
  ;; drop us at the line that begins the string.
  (let (open-bracket-pos)
    (while (fsharp-continuation-line-p)
      (beginning-of-line)
      (if (fsharp--hanging-operator-continuation-line-p)
          (while (fsharp--hanging-operator-continuation-line-p)
            (forward-line -1))
        ;; else zip out of nested brackets/braces/parens
        (while (setq open-bracket-pos (fsharp-nesting-level))
          (goto-char open-bracket-pos)))))
  (beginning-of-line))


(defun fsharp--goto-end-of-string ()
  "If point is inside an F# string literal or looking at the
quotes on either side, move point to one past the closing
character of the string. If point is before or after the string,
do not move point."
  ;; If point is on a quote, scoot forward past it. This will either take us
  ;; *in* to the string, so `parse-partial-sexp' can do its work, or out past
  ;; the end, which is what we want in any case.
  (while (looking-at-p "\"")
    (forward-char 1))

  ;; FIXME: currently, the syntax-table for fsharp-mode doesn't appear to
  ;; register triple-quoted strings, which has a lot of weird effects. We'll
  ;; handle that here until syntax-table can be fixed.
  (while (eq 'string (fsharp-in-literal-p))
    ;; `parse-partial-sexp' can have strange side-effects if not called inside a
    ;; string, so check first.
    (when (fsharp-in-literal-p)
      (progn
        ;; `parse-partial-sexp' moves point to the closing character of the string, so
        ;; this parse is all we need to do.
        (parse-partial-sexp (point) (point-max) nil nil nil 'syntax-table)
        ;; Due to the aforementioned syntax-table troubles, we might wind up
        ;; inside the triple-quotes rather than after. If this happens, scoot
        ;; forward.
        (while (looking-at-p "\"")
          (forward-char 1))
        ))))


(defun fsharp--skip-past-comment-or-string ()
  "If point is inside a comment or string, go to the end of that
comment or string."
  (-let [continue t]
    (while continue
      (-if-let (current-literal (progn
                                  ;; Use forward-whitespace before checking for
                                  ;; a comment, in case of indentation (like,
                                  ;; for instance, this comment line itself.)
                                  (cond
                                   ((looking-at-p "\\s-*\\(//\\|(\\*\\)")
                                    (forward-whitespace 1))
                                   ((looking-at-p "\"\"\"") (forward-char 3))
                                   ((looking-at-p "\"") (forward-char 1)))
                                  (fsharp-in-literal-p)))
          (cond
           ((eq current-literal 'comment) (forward-line 1))
           ((eq current-literal 'string)
            (fsharp--goto-end-of-string))
           ;; Something has happened, stop.
           (:default (setq continue nil)))
        (setq continue nil)))
    ))


(defun fsharp--skip-past-pipes ()
  "Move point to the line after the end of a long pipe
  expression, skipping over comments and long strings in the
  process."
  (-let [continue t]
    (while continue
      (if (fsharp--pipe-expression-p)
          ;; Pipes not infrequently open blocks, in which case, traverse
          (if (fsharp-expression-opens-block-p)
              ;; TODO[gastove|2019-12-14] Remove this function use... somehow.
              (fsharp--goto-end-of-block)
            ;; Otherwise, normal block, just go to the end.
            (forward-line 1))
        ;; Next possibility: the next pipe in the sequence is after some number
        ;; of comments or a long string literal.
        (-if-let (next-pipe (fsharp--next-line-pipe-expression-p))
            (goto-char next-pipe)
          ;; Final else
          (setq continue nil)
          ))
      )))


(defun fsharp-goto-beyond-current-expression ()
  "Go to the beginning of the line after the final line of the
current expression. E.g, with █ representing point after the
function is called:

let x = 5
█

let thing =
    long
    |> pipe
    |> sequence
█

\"Current Expression\" is a conceptually complex term, as F#
allows for arbitrarily nested expressions. This function attempts
to move to the end of the smallest complete expression possible.
That is, given this (line numbers included for clarity):

1| let x =
2|     let y = 5
3|     let z = 10
4|     y ** z
5|

With point on line 1, this function would move point to line 5.
However, with point on line 2, this function advances to line 3,
and on line 3, we advance to line 4.

The algorithm here is roughly as follows:

1. Record: initial indentation level; set `continue' to `true'.

2. Is the current line a continuation of an ongoing expression?
  (That is, part of a pipe expression, multi-line string,
  multi-line comment, etc.)

  - Yes, current line is a continuation line: move forward to the
    end of the continuation.
  - No: move on to next check

3. Does the current line open a multi-line expression?

  - Yes: advance one line; go back to 2
  - No: advance one line.

4. Is the current line empty?

  - Yes: are we currently trying to get to the end of an ongoing, multi-line expression?
    - Yes: we're done if the next line of code is at the same or less indentation than the initial indentation.
    - No: we're done; set `continue' to `false'

  - No: does the current line increase indentation?
    - Yes: advance one line
    - No: we're done; set `continue' to `false'

5. Is `continue' true?

  - Yes: loop back to 2
  - No: exit.
"
  (-let ((init (point))
         (benchmark-indentation (current-indentation))
         (continue t))
    (beginning-of-line)
    ;; We pretty much always go forward one line.
    (forward-line 1)
    (while continue
      (cond
       ;; The most common kind of block is pipes. If we start in pipes or wind
       ;; up in pipes, we scoot past 'em, then exit.
       ((or (fsharp--pipe-expression-p)
            (fsharp--next-line-pipe-expression-p))
        (fsharp--skip-past-pipes)
        (setq continue nil))

       ;; Next most common kind of multi-line expression is literals. Skip past comments or strings.
       ((fsharp-in-literal-p)
        (fsharp--skip-past-comment-or-string))

       ;; Line opens a block. Advance one line.
       ((fsharp-expression-opens-block-p)
        (forward-line 1))

       ;; NOTE: not actually sure we want this
       ;; Point is in a "single-line block"; go to the next line and exit
       ;; ((fsharp--single-line-block-p)
       ;;  (forward-line 1)
       ;;  (setq continue nil))

       ;; If the current line is blank, we're done if:
       ;; - The next line of code is indented <= benchmark
       ;; Otherwise, go forward one line
       ((looking-at-p "^\\s-*$")
        (if (save-excursion
              (re-search-forward "\\w" (point-max) t)
              (and
               (/= 0 (current-indentation))
               (> (current-indentation) benchmark-indentation)))

            (forward-line 1)
          ;; If the above failed, exit!
          (setq continue nil)))

       ;; Maybe we win? If we've dedented, exit.
       ((< (current-indentation) benchmark-indentation)
        (setq continue nil))

       ;; ran out of search, return to start
       ((eobp)
        (goto-char init)
        (setq continue nil))

       ((= (current-indentation) benchmark-indentation)
        (setq continue nil))

       ;; If there's nothing else to say or do, scoot forward one line.
       (t
        (forward-line 1))))))


(defun fsharp--anonymous-function-single-line-p ()
  "Returns true if point is somewhere in the body of an anonymous
function is fully expressed on a single line, nil otherwise."
  (save-excursion
    (-let ((back-lim (fsharp-point 'bol))
           (forward-lim (fsharp-point 'eol))
           (curr-indent (current-indentation))
           (next-indent (save-excursion (forward-line 1) (current-indentation))))
      (beginning-of-line)
      (and
       ;; Needs to be an anonymous function on this line _at all_
       (re-search-forward "\\<fun\\>" forward-lim t)
       ;; The line ends in an arrow; definitely multi-line
       (not (looking-at ".*->$"))
       (or
        ;; The next line is indented as much or less than the current line,
        ;; indicating no new block was opened.
        (<= curr-indent next-indent)
        ;; We have the `fun' keyword and matched surrounding parens
        ;; TODO: look in to doing this with parse-partial-sexp.
        (and (looking-back "(\\s-*fun" back-lim)
             (looking-at ".*)")))
       ))))

;; TODO[gastove|2019-11-02] Not actually sure how this one will work. Just
;; inverting the previous is wrong. I also... might not need this? Good
;; question.
;; (defun fsharp--anonymous-function-multi-line-p ()
;;   (and
;;    (re-search-forward "\\<fun\\>" forward-lim t)
;;    (not (fsharp--anonymous-function-single-line-p))))

(defun fsharp--single-line-if-p ()
  (save-excursion
    (or
     (looking-at ".*if.*else")
     (and (looking-back "if.*" (fsharp-point 'bol))
          (looking-at ".*else \\w+$"))
     (looking-back "if.*else.*" (fsharp-point 'bol)))))


(defun fsharp--single-line-block-p ()
  "Returns true if point is in a 'single-line block'.
That is: some F# expressions _can_ open new blocks, but aren't
_required_ to do so. This includes if/else expressions and
anonymous functions."
  (save-excursion
    (beginning-of-line)
    (or
     ;; Single-line if/else
     (fsharp--single-line-if-p)
     ;; Single-line anonymous function
     (fsharp--anonymous-function-single-line-p)
     )))


;; TODO[gastove|2019-12-07] Remove this once I'm sure it's not needed any more.
;; NOTE[gastove|2019-12-14] It's still used by `fsharp--skip-past-pipes'.
(defun fsharp--goto-end-of-block ()
  "Go to the line after the end of the block opened under point.
If the block is single-line, go to the next line. If indentation
never changes, or line under point does not open a block, point
does not move."
  (-let ((init (point))
         (benchmark-indentation (current-indentation))
         (continue (fsharp-expression-opens-block-p)))
    (beginning-of-line)
    (while continue
      (cond
       ;; Line opens a block. Advance one line.
       ((re-search-forward fsharp-block-opening-re (fsharp-point 'eol) t)
        (forward-line 1))

       ;; The most common kind of block is pipes.
       ((fsharp--pipe-expression-p)
        (setq benchmark-indentation (current-indentation))
        (while (fsharp--pipe-expression-p)
          (forward-line 1)))

       ;; Point is in a "single-line block"; go to the next line and exit
       ((fsharp--single-line-block-p)
        (forward-line 1)
        (setq continue nil))

       ;; Mostly for comments, but if there _is_ a big string in there
       ;; somewhere, we should skip that too.
       ((fsharp-in-literal-p) (fsharp--skip-past-comment-or-string))

       ;; If the current line is blank, we're only done if the next line isn't a
       ;; member of the current block.
       ((looking-at-p "^\\s-*$")
        (if (save-excursion
              (forward-line 1)
              (> (current-indentation) benchmark-indentation))
            (forward-line 1)
          ;; If the above failed, exit!
          (setq continue nil)))

       ;; Maybe we win? If we've dedented, exit.
       ((< (current-indentation) benchmark-indentation)
        (setq continue nil))

       ;; ran out of search, return to start
       ((eobp)
        (goto-char init)
        (setq continue nil))

       ;; If there's nothing else to say or do, scoot forward one line.
       (t (forward-line 1))))
    )
  )


(defun fsharp-goto-beyond-block ()
  "Go to point just beyond the final line of block begun by the current line.
This is the same as where `fsharp-goto-beyond-current-expression' goes unless
we're on colon line, in which case we go to the end of the block.
Assumes point is at the beginning of the line."
  (if (fsharp-expression-opens-block-p)
      (fsharp-mark-block nil 'just-move)
    (fsharp-goto-beyond-current-expression)))


(defun fsharp-goto-statement-at-or-above ()
  "Go to the start of the first statement at or preceding point.
Return t if there is such a statement, otherwise nil.  `Statement'
does not include blank lines, comments, or continuation lines."
  (fsharp-goto-initial-line)
  (if (looking-at fsharp-blank-or-comment-re)
      ;; skip back over blank & comment lines
      ;; note:  will skip a blank or comment line that happens to be
      ;; a continuation line too
      (if (re-search-backward "^[ \t]*\\([^ \t\n]\\|//\\)" nil t)
          (progn (fsharp-goto-initial-line) t)
        nil)
    t))

(defun fsharp-goto-statement-below ()
  "Go to start of the first statement following the statement containing point.
Return t if there is such a statement, otherwise nil.  `Statement'
does not include blank lines, comments, or continuation lines."
  (beginning-of-line)
  (let ((start (point)))
    (fsharp-goto-beyond-current-expression)
    (while (and
            (or (looking-at fsharp-blank-or-comment-re)
                (fsharp-in-literal-p))
            (not (eobp)))
      (forward-line 1))
    (if (eobp)
        (progn (goto-char start) nil)
      t)))

(defun fsharp-go-up-tree-to-keyword (key)
  "Go to begining of statement starting with KEY, at or preceding point.

KEY is a regular expression describing a Fsharp keyword.  Skip blank
lines and non-indenting comments.  If the statement found starts with
KEY, then stop, otherwise go back to first enclosing block starting
with KEY.  If successful, leave point at the start of the KEY line and
return t.  Otherwise, leave point at an undefined place and return nil."
  ;; skip blanks and non-indenting //
  (fsharp-goto-initial-line)
  (while (and
          (looking-at "[ \t]*\\($\\|//[^ \t\n]\\)")
          (zerop (forward-line -1)))    ; go back
    nil)
  (fsharp-goto-initial-line)
  (let* ((re (concat "[ \t]*" key "\\>"))
         (case-fold-search nil)                 ; let* so looking-at sees this
         (found (looking-at re))
         (dead nil))
    (while (not (or found dead))
      (condition-case nil               ; in case no enclosing block
          (fsharp-goto-block-up 'no-mark)
        (error (setq dead t)))
      (or dead (setq found (looking-at re))))
    (beginning-of-line)
    found))


(defun fsharp-suck-up-leading-text ()
  "Return string in buffer from start of indentation to end of line.
Prefix with \"...\" if leading whitespace was skipped."
  (save-excursion
    (back-to-indentation)
    (concat
     (if (bolp) "" "...")
     (buffer-substring (point) (progn (end-of-line) (point))))))


(defun fsharp-suck-up-first-keyword ()
  "Return first keyword on the line as a Lisp symbol.
`Keyword' is defined (essentially) as the regular expression
([a-z]+).  Returns nil if none was found."
  (let ((case-fold-search nil))
    (if (looking-at "[ \t]*\\([a-z]+\\)\\>")
        (intern (buffer-substring (match-beginning 1) (match-end 1)))
      nil)))

(defun fsharp-current-defun ()
  "Fsharp value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable."
  (save-excursion

    ;; Move back to start of the current statement.

    (fsharp-goto-initial-line)
    (back-to-indentation)
    (while (and (or (looking-at fsharp-blank-or-comment-re)
                    (fsharp-in-literal-p))
                (not (eq (point-at-bol) (point-min))))
      (backward-to-indentation 1))
    (fsharp-goto-initial-line)

    (let ((scopes "")
          (sep "")
          dead assignment)

      ;; Check for an assignment.  If this assignment exists inside a
      ;; def, it will be overwritten inside the while loop.  If it
      ;; exists at top lever or inside a class, it will be preserved.

      (when (looking-at "[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*=")
        (setq scopes (buffer-substring (match-beginning 1) (match-end 1)))
        (setq assignment t)
        (setq sep "."))

      ;; Prepend the name of each outer socpe (def or class).

      (while (not dead)
        (if (and (fsharp-go-up-tree-to-keyword "\\(class\\|def\\)")
                 (looking-at
                  "[ \t]*\\(class\\|def\\)[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*"))
            (let ((name (buffer-substring (match-beginning 2) (match-end 2))))
              (if (and assignment (looking-at "[ \t]*def"))
                  (setq scopes name)
                (setq scopes (concat name sep scopes))
                (setq sep "."))))
        (setq assignment nil)
        (condition-case nil             ; Terminate nicely at top level.
            (fsharp-goto-block-up 'no-mark)
          (error (setq dead t))))
      (if (string= scopes "")
          nil
        scopes))))


(defun fsharp-beginning-of-block ()
  "Move point to the beginning of the current top-level block"
  (interactive)
  (let ((prev (point)))
    (condition-case nil
        (while (progn (fsharp-goto-block-up 'no-mark)
                      (< (point) prev))
          (setq prev (point)))
      (error (while (fsharp-continuation-line-p)
               (forward-line -1)))))
  (beginning-of-line))


(defun fsharp-end-of-block ()
  "Move point to the end of the current top-level block"
  (interactive)
  (forward-line 1)
  (if (not (eobp))
      (progn
        (beginning-of-line)
        (condition-case nil
            (progn (re-search-forward "^[a-zA-Z#0-9([]")
                   (while (fsharp-continuation-line-p)
                     (forward-line 1))
                   (forward-line -1))
          (error
           (progn (goto-char (point-max)))))
        (end-of-line)
        (when (looking-at-p "\n[ \t]*and[ \t]+")
          (forward-line 1)
          (fsharp-end-of-block)))
    (goto-char (point-max))))


(defun fsharp-mark-phrase ()
  "Mark current phrase"
  (interactive)
  (fsharp-beginning-of-block)
  (push-mark (point))
  (fsharp-end-of-block)
  (exchange-point-and-mark))


(defun fsharp-mark-block (&optional extend just-move)
  "Mark following block of lines.  With prefix arg, mark structure.
Easier to use than explain.  It sets the region to an `interesting'
block of succeeding lines.  If point is on a blank line, it goes down to
the next non-blank line.  That will be the start of the region.  The end
of the region depends on the kind of line at the start:

 - If a comment, the region will include all succeeding comment lines up
   to (but not including) the next non-comment line (if any).

 - Else if a prefix arg is given, and the line begins one of these
   structures:

     if elif else try except finally for while def class

   the region will be set to the body of the structure, including
   following blocks that `belong' to it, but excluding trailing blank
   and comment lines.  E.g., if on a `try' statement, the `try' block
   and all (if any) of the following `except' and `finally' blocks
   that belong to the `try' structure will be in the region.  Ditto
   for if/elif/else, for/else and while/else structures, and (a bit
   degenerate, since they're always one-block structures) def and
   class blocks.

 - Else if no prefix argument is given, and the line begins a Fsharp
   block (see list above), and the block is not a `one-liner' (i.e.,
   the statement ends with a colon, not with code), the region will
   include all succeeding lines up to (but not including) the next
   code statement (if any) that's indented no more than the starting
   line, except that trailing blank and comment lines are excluded.
   E.g., if the starting line begins a multi-statement `def'
   structure, the region will be set to the full function definition,
   but without any trailing `noise' lines.

 - Else the region will include all succeeding lines up to (but not
   including) the next blank line, or code or indenting-comment line
   indented strictly less than the starting line.  Trailing indenting
   comment lines are included in this case, but not trailing blank
   lines.

A msg identifying the location of the mark is displayed in the echo
area; or do `\\[exchange-point-and-mark]' to flip down to the end.

If called from a program, optional argument EXTEND plays the role of
the prefix arg, and if optional argument JUST-MOVE is not nil, just
moves to the end of the block (& does not set mark or display a msg)."
  (interactive "P")                     ; raw prefix arg
  (fsharp-goto-initial-line)
  ;; skip over blank lines
  (while (and
          (looking-at "[ \t]*$")        ; while blank line
          (not (eobp)))                         ; & somewhere to go
    (forward-line 1))
  (if (eobp)
      (error "Hit end of buffer without finding a non-blank stmt"))
  (let ((initial-pos (point))
        (initial-indent (current-indentation))
        last-pos                        ; position of last stmt in region
        (followers
         '((if elif else) (elif elif else) (else)
           (try except finally) (except except) (finally)
           (for else) (while else)
           (def) (class) ) )
        first-symbol next-symbol)

    (cond
     ;; if comment line, suck up the following comment lines
     ((looking-at "[ \t]*//")
      (re-search-forward "^[ \t]*\\([^ \t]\\|//\\)" nil 'move) ; look for non-comment
      (re-search-backward "^[ \t]*//")  ; and back to last comment in block
      (setq last-pos (point)))

     ;; else if line is a block line and EXTEND given, suck up
     ;; the whole structure
     ((and extend
           (setq first-symbol (fsharp-suck-up-first-keyword) )
           (assq first-symbol followers))
      (while (and
              (or (fsharp-goto-beyond-block) t) ; side effect
              (forward-line -1)                 ; side effect
              (setq last-pos (point))   ; side effect
              (fsharp-goto-statement-below)
              (= (current-indentation) initial-indent)
              (setq next-symbol (fsharp-suck-up-first-keyword))
              (memq next-symbol (cdr (assq first-symbol followers))))
        (setq first-symbol next-symbol)))

     ;; else if line *opens* a block, search for next stmt indented <=
     ((fsharp-expression-opens-block-p)
      (while (and
              (setq last-pos (point))   ; always true -- side effect
              (fsharp-goto-statement-below)
              (> (current-indentation) initial-indent)
              )))

     ;; else plain code line; stop at next blank line, or stmt or
     ;; indenting comment line indented <
     (t
      (while (and
              (setq last-pos (point))   ; always true -- side effect
              (or (fsharp-goto-beyond-current-expression) t)
              (not (looking-at "[ \t]*$")) ; stop at blank line
              (or
               (>= (current-indentation) initial-indent)
               (looking-at "[ \t]*//[^ \t\n]"))) ; ignore non-indenting //
        nil)))

    ;; skip to end of last stmt
    (goto-char last-pos)
    (fsharp-goto-beyond-current-expression)

    ;; set mark & display
    (if just-move
        ()                              ; just return
      (push-mark (point) 'no-msg)
      (forward-line -1)
      (message "Mark set after: %s" (fsharp-suck-up-leading-text))
      (goto-char initial-pos))))

(defun fsharp-mark-def-or-class (&optional class)
  "Set region to body of def (or class, with prefix arg) enclosing point.
Pushes the current mark, then point, on the mark ring (all language
modes do this, but although it's handy it's never documented ...).

In most Emacs language modes, this function bears at least a
hallucinogenic resemblance to `\\[fsharp-end-of-def-or-class]' and
`\\[fsharp-beginning-of-def-or-class]'.

And in earlier versions of Fsharp mode, all 3 were tightly connected.
Turned out that was more confusing than useful: the `goto start' and
`goto end' commands are usually used to search through a file, and
people expect them to act a lot like `search backward' and `search
forward' string-search commands.  But because Fsharp `def' and `class'
can nest to arbitrary levels, finding the smallest def containing
point cannot be done via a simple backward search: the def containing
point may not be the closest preceding def, or even the closest
preceding def that's indented less.  The fancy algorithm required is
appropriate for the usual uses of this `mark' command, but not for the
`goto' variations.

So the def marked by this command may not be the one either of the
`goto' commands find: If point is on a blank or non-indenting comment
line, moves back to start of the closest preceding code statement or
indenting comment line.  If this is a `def' statement, that's the def
we use.  Else searches for the smallest enclosing `def' block and uses
that.  Else signals an error.

When an enclosing def is found: The mark is left immediately beyond
the last line of the def block.  Point is left at the start of the
def, except that: if the def is preceded by a number of comment lines
followed by (at most) one optional blank line, point is left at the
start of the comments; else if the def is preceded by a blank line,
point is left at its start.

The intent is to mark the containing def/class and its associated
documentation, to make moving and duplicating functions and classes
pleasant."
  (interactive "P")                     ; raw prefix arg
  (let ((start (point))
        (which (cond ((eq class 'either) "\\(type\\|let\\)")
                     (class "type")
                     (t "let"))))
    (push-mark start)
    (if (not (fsharp-go-up-tree-to-keyword which))
        (progn (goto-char start)
               (error "Enclosing %s not found"
                      (if (eq class 'either)
                          "def or class"
                        which)))
      ;; else enclosing def/class found
      (setq start (point))
      (fsharp-goto-beyond-block)
      (push-mark (point))
      (goto-char start)
      (if (zerop (forward-line -1))     ; if there is a preceding line
          (progn
            (if (looking-at "[ \t]*$")  ; it's blank
                (setq start (point))    ; so reset start point
              (goto-char start))        ; else try again
            (if (zerop (forward-line -1))
                (if (looking-at "[ \t]*//") ; a comment
                    ;; look back for non-comment line
                    ;; tricky: note that the regexp matches a blank
                    ;; line, cuz \n is in the 2nd character class
                    (and
                     (re-search-backward "^[ \t]*\\([^ \t]\\|//\\)" nil 'move)
                     (forward-line 1))
                  ;; no comment, so go back
                  (goto-char start)))))))
  (exchange-point-and-mark))


;;------------------------------- SMIE Configs -------------------------------;;

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
    (`(:elem . basic) fsharp-indent-offset)
    (`(:after . "do") fsharp-indent-offset)
    (`(:after . "then") fsharp-indent-offset)
    (`(:after . "else") fsharp-indent-offset)
    (`(:after . "try") fsharp-indent-offset)
    (`(:after . "with") fsharp-indent-offset)
    (`(:after . "finally") fsharp-indent-offset)
    (`(:after . "in") 0)
    (`(:after . ,(or `"[" `"]" `"[|" `"|]")) fsharp-indent-offset)
    (`(,_ . ,(or `";" `",")) (if (smie-rule-parent-p "begin")
                                 0
                               (smie-rule-separator kind)))
    (`(:after . "=") fsharp-indent-offset)
    (`(:after . ";;") (smie-rule-separator kind))
    (`(:before . ";;") (if (smie-rule-bolp)
                           0))
    ))


(defun fsharp-mode-indent-smie-setup ()
  (smie-setup fsharp-smie-grammar #'fsharp-smie-rules))


(provide 'fsharp-mode-structure)
;;; fsharp-mode-structure.el ends here
