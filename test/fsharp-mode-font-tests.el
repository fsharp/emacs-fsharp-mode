;;; fsharp-mode-font-tests.el --- Regression test for FSharp font lock

;; Keywords: faces languages
;; Created: 2014-09-13
;; Version: 0.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Regression test of `fsharp-mode-face', a package providing
;; font-lock rules for F#. This module verifies fontification of a
;; number of files taken from real projects. This is done by keeing a
;; text representation of the fontification using `faceup' markup, in
;; addition to the original source file.
;;
;; To regenerate the faceup representation, use M-x faceup-write-file RET.
;;
;; `faceup' is located at https://github.com/Lindydancer/faceup
;;
;; The actual check is performed using `ert' (Emacs Regression Test),
;; with a font-lock test function provided by `faceup'.

;;; Code:

(require 'faceup)
(require 'ert)

(defun fsharp-testing-should-match (regexp match-list)
  (dolist (should-match match-list)
    (should (= 0 (string-match-p regexp should-match)))))

(defun fsharp-testing-should-not-match (regexp not-match-list)
  (dolist (should-not-match not-match-list)
    (should-not (string-match-p regexp should-not-match))))

(defun fsharp-testing-match-lists (regexp match-list not-match-list)
  (fsharp-testing-should-match regexp match-list)
  (fsharp-testing-should-not-match regexp not-match-list))

;;---------------------------Test Font-Lock Regexen-----------------------------
;; Simple regexen
(ert-deftest fsharp-mode-access-control-regexp-test ()
  (let ((should-matches '("private " "internal " "public "))
        (should-not-matches '("privateer" "internalized" "publication")))
    (fsharp-testing-match-lists fsharp-access-control-regexp-noncapturing
                                should-matches
                                should-not-matches)))

(ert-deftest fsharp-mode-inline-rec-regex-test ()
  (let ((should-matches '("inline " "rec "))
        (should-not-matches '("inlined" "reccomend")))
    (fsharp-testing-match-lists fsharp-inline-rec-regexp-noncapturing
                                should-matches
                                should-not-matches)))

(ert-deftest fsharp-mode-valid-identifier-regex-test ()
  (let ((should-matches '("testIdentifier" "Test_Identifier" "Int32'"))
        ;; Primarily, this regex needs to not catch symbolic operators
        (should-not-matches '("|>" "{}" "<@" ">>>")))
    (fsharp-testing-match-lists fsharp-valid-identifier-regexp
                                should-matches
                                should-not-matches)))

;; Composite regexen
(defun test-capture (regexp s should-capture &optional second-capture)
  "Be sure REGEXP matches against S, and captures SHOULD-CAPTURE."
  (and (string-match regexp s)
       (string= should-capture (match-string 1 s))
       (if second-capture
           (string= second-capture (match-string 2 s))
         't)))

(ert-deftest fsharp-mode-function-def-regexp-test ()
  (let ((test-string-1 "let functionName arg1 = 5")
        (test-string-2 "let identifier = 2")
        (test-string-3 "let funcWithNoArgs() = [|2|]")
        (test-string-4 "let funcWithTupleArgs (hi: string) = hi")
        (test-fdef-regexp (apply-partially 'test-capture fsharp-function-def-regexp)))
    ;; test-string-1 -- match a funciton name
    (funcall test-fdef-regexp test-string-1 "functionName")
    (should (not (string-match fsharp-function-def-regexp test-string-2)))
    (funcall test-fdef-regexp test-string-3 "funcWithNoArgs")
    (funcall test-fdef-regexp test-string-4 "funcWithTupleArgs")))

(ert-deftest fsharp-pattern-function-regexp-test ()
  (let ((test-string "let funcForMatching = function"))
    (test-capture fsharp-pattern-function-regexp test-string "funcForMatching")))


(ert-deftest fsharp-active-pattern-regexp-test ()
  (let ((test-string-1 "let (|Foo|) = ")
        (test-string-2 "let (|Foo|Bar|)")
        (test-apattern-capture (apply-partially 'test-capture fsharp-active-pattern-regexp)))
    (funcall test-apattern-capture test-string-1 "Foo")
    (funcall test-apattern-capture test-string-2 "Foo|Bar")))

(ert-deftest fsharp-member-function-regexp-test ()
  (let ((test-string-1 "member rec this.functionName")
        (test-string-2 "member self.DifferentFunction")
        (test-mpattern-capture (apply-partially 'test-capture fsharp-member-function-regexp)))
    (funcall test-mpattern-capture test-string-1 "functionName")
    (funcall test-mpattern-capture test-string-2 "DifferentFunction")))

(ert-deftest fsharp-constructor-regexp-test ()
  (let ((test-string-1 "new Foo()")
        (test-string-2 "let newFoo ="))
    (test-capture fsharp-constructor-regexp test-string-1 "new")
    (should (not (string-match fsharp-constructor-regexp test-string-2)))))

;; Operators
(ert-deftest fsharp-operator-quote-regexp-test ()
  (let ((test-string "<@ SomeCode @>"))
    (test-capture fsharp-operator-quote-regexp test-string "<@" "@>")))

(ert-deftest fsharp-operator-pipe-regexp-test ()
  (let ((test-string-1 "|> foo")
        (test-string-2 "foo |> bar")
        (test-string-3 "(foo, bar) ||> zorb")
        (test-string-4 "|||> blerp")
        (test-pipe-pattern (apply-partially 'test-capture fsharp-operator-pipe-regexp)))
    (funcall test-pipe-pattern test-string-1 "|>")
    (funcall test-pipe-pattern test-string-2 "|>")
    (funcall test-pipe-pattern test-string-3 "||>")
    (funcall test-pipe-pattern test-string-4 "|||>")))

(ert-deftest fsharp-operator-case-regexp-test ()
  (let ((test-string-1 "| foo")
        (test-string-2 "|> foo"))
    (test-capture fsharp-operator-case-regexp test-string-1 "|")
    (should (not (string-match fsharp-operator-case-regexp test-string-2)))))

;; Faceup Tests of Font Locking
(defvar fsharp-mode-face-test-file-name (faceup-this-file-directory)
  "The file name of this file.")

(defun fsharp-mode-face-test-apps (file)
  "Test that FILE is fontifies as the .faceup file describes.

FILE is interpreted as relative to this source directory."
  (faceup-test-font-lock-file 'fsharp-mode
                              (concat
                               fsharp-mode-face-test-file-name
                               file)))
(faceup-defexplainer fsharp-mode-face-test-apps)


(ert-deftest fsharp-mode-face-file-test ()
  (let ((coding-system-for-read 'utf-8))
    (should (fsharp-mode-face-test-apps "apps/FQuake3/NativeMappings.fs"))
    (should (fsharp-mode-face-test-apps "apps/FSharp.Compatibility/Format.fs"))
    (should (fsharp-mode-face-test-apps "apps/RecordHighlighting/Test.fsx"))
    ))


(defun fsharp-font-lock-test (faceup)
  (faceup-test-font-lock-string 'fsharp-mode faceup))
(faceup-defexplainer fsharp-font-lock-test)

(ert-deftest fsharp-mode-face-test-snippets ()
  (should (fsharp-font-lock-test "«k:let» «v:x» = is a keyword"))
  (should (fsharp-font-lock-test "«k:open» «t:FSharp.Charting»"))
  (should (fsharp-font-lock-test "«k:let» (*) «v:x» «v:y» = x + y «m:// »«x:*)»"))
  (should (fsharp-font-lock-test "«k:let» ( * ) «v:x» «v:y» = x + y «m:// »«x:*)»"))
  (should (fsharp-font-lock-test "«k:let» ( *!=> ) x y = x + y «m:// »«x:*)»"))
  )

;;; fsharp-mode-font-tests.el ends here
