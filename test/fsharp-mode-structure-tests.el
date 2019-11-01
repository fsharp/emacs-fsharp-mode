(require 'fsharp-mode-structure)
(require 'test-common)

(defvar fsharp-struct-test-files-dir (concat test-dir "StructureTest/"))

;;-------------------------------- Regex Tests --------------------------------;;

(ert-deftest fsharp-stringlit-re-test ())

;;--------------------------- Structure Navigation ---------------------------;;
;; TODO[gastove|2019-10-31] This function turns out to be incredibly broken! It
;; wont move past the final line of _most_ multi-line expressions. Wonderful.
;;
;; This will get fixed in the next PR.
;; (ert-deftest fsharp-goto-beyond-final-line-test ()
;;   (let ((blocks-file (file-truename (concat fsharp-struct-test-files-dir "Blocks.fs"))))
;;     (using-file blocks-file
;;       ;; A single-line expression
;;       (goto-char 1)
;;       (fsharp-goto-beyond-final-line)
;;       (should (eq (point) 19))

;;       ;; A multi-line expression using a pipe. We should wind up in the same
;;       ;; place whether we start at the beginning or the end of the expression.
;;       (goto-char 20)
;;       (fsharp-goto-beyond-final-line)
;;       (should (eq (point) 88))
;;       (goto-char 46)
;;       (fsharp-goto-beyond-final-line)
;;       (should (eq (point) 88))

;;       ;; A multi-line discriminated union.
;;       (goto-char 89)
;;       (fsharp-goto-beyond-final-line)
;;       (should (eq (point) 146))
;;       (goto-char 122)
;;       (fsharp-goto-beyond-final-line)
;;       (should (eq (point) 146))

;;       ;; A function using an if/else block
;;       (goto-char 147)
;;       (fsharp-goto-beyond-final-line)
;;       (should (eq (point) 218))
;;       (goto-char 171)
;;       (fsharp-goto-beyond-final-line)
;;       (should (eq (point) 218))
;;       )))

;;-------------------------------- Predicates --------------------------------;;

(ert-deftest fsharp--hanging-operator-continuation-line-p-test ()
  "Does `fsharp-backslash-continuation-line-p' return true when we expect it to?"
  (let ((continuation-file (file-truename (concat fsharp-struct-test-files-dir "ContinuationLines.fs"))))
    (using-file continuation-file
      (beginning-of-buffer)
      (should (eq (fsharp--hanging-operator-continuation-line-p) nil))
      (forward-line 1)
      (should (eq (fsharp--hanging-operator-continuation-line-p) nil))
      (forward-line 5)
      (should (eq (fsharp--hanging-operator-continuation-line-p) t))
      )))

(ert-deftest fsharp-in-literal-p-test ()
  "Does `fsharp-in-literal-p' return non-nil in both strings and comments?"
  (let ((literals-file (file-truename (concat fsharp-struct-test-files-dir "Literals.fs"))))
    (using-file literals-file
      ;; Comments
      (goto-char 3)
      (should (eq (fsharp-in-literal-p) 'comment))
      (goto-char 642)
      (should (eq (fsharp-in-literal-p) 'comment))
      (goto-char 968)
      (should (eq (fsharp-in-literal-p) 'comment))
      (goto-char 1481)
      (should (eq (fsharp-in-literal-p) 'comment))
      (goto-char 2124)
      (should (eq (fsharp-in-literal-p) 'comment))
      ;; String literals
      (goto-char 2717)
      (should (eq (fsharp-in-literal-p) 'string))
      ;; This string contains an inner, backslash-escaped string.
      ;; First, with point outside the backslash-escaped string:
      (goto-char 2759)
      (should (eq (fsharp-in-literal-p) 'string))
      ;; ...and now with point inside it
      (goto-char 2774)
      (should (eq (fsharp-in-literal-p) 'string))
      ;; Inside triple-quoted strings
      (goto-char 2835)
      (should (eq (fsharp-in-literal-p) 'string))
      (goto-char 2900)
      (should (eq (fsharp-in-literal-p) 'string)))))

;; NOTE[gastove|2019-10-31] I am entirely convinced this doesn't work precisely
;; as it should, because it depends on `fsharp-goto-beyond-final-line', which I
;; am positive is buggy.
;;
;; Udate: yep! It's buggy! Will uncomment and fix in the next PR.
;; (ert-deftest fsharp-statement-opens-block-p-test ()
;;   "Does `fsharp-statement-opens-block-p' correctly detect block-opening statements?"
;;   (let ((blocks-file (file-truename (concat fsharp-struct-test-files-dir "Blocks.fs"))))
;;     (using-file blocks-file
;;       (goto-char 1)
;;       (should-not (fsharp-statement-opens-block-p))
;;       (goto-char 20)
;;       (should (fsharp-statement-opens-block-p))
;;       (goto-char 89)
;;       (should (fsharp-statement-opens-block-p)))))

;;--------------------- Nesting and Indentation Functions ---------------------;;

(ert-deftest fsharp-nesting-level-test-should-nil ()
  "Does `fsharp-nesting-level' return nil when we expect it to?"
  (with-temp-buffer
    (insert "let x = 5")
    (end-of-buffer)
    (should (eq (fsharp-nesting-level) nil))))


(ert-deftest fsharp-nesting-level-test ()
  "Does `fsharp-nesting-level' correctly return the point
position of the opening pair closest to point?"
  ;; The character positions use here reference characters noted in comments in Nesting.fs
  (let ((nesting-file (file-truename (concat fsharp-struct-test-files-dir "Nesting.fs"))))
    ;; Test a normal list
    (using-file nesting-file
      (goto-char 645)
      (should (eq (fsharp-nesting-level) 640)))

    ;; Get the opening bracket of an inner list from a single-line nested list
    (using-file nesting-file
      (goto-char 717)
      (should (eq (fsharp-nesting-level) 706)))

    ;; Opening bracket for a multi-line non-nested list
    (using-file nesting-file
      (goto-char 795)
      (should (eq (fsharp-nesting-level) 777)))

    ;; Inner most opening bracket for a multi-line multi-nested list
    (using-file nesting-file
      (goto-char 960)
      (should (eq (fsharp-nesting-level) 955)))
    ;; Middle opening bracket for same list as previous
    (using-file nesting-file
      (goto-char 954)
      (should (eq (fsharp-nesting-level) 953)))
    (using-file nesting-file
      (goto-char 974)
      (should (eq (fsharp-nesting-level) 953)))
    ;; Outermost opening bracket for same list
    (using-file nesting-file
      (goto-char 977)
      (should (eq (fsharp-nesting-level) 947)))

    ;; Basic Async form, should return the opening {
    (using-file nesting-file
      (goto-char 1088)
      (should (eq (fsharp-nesting-level) 1060)))
    ;; Same async form, inner async call
    (using-file nesting-file
      (goto-char 1129)
      (should (eq (fsharp-nesting-level) 1121)))

    ;; Lambda, wrapped in parens, should return the opening (
    (using-file nesting-file
      (goto-char 1238)
      (should (eq (fsharp-nesting-level) 1208)))
    ))


(ert-deftest fsharp--compute-indentation-open-bracket-test ()
  "Does `fsharp--compute-indentaiton-open-bracket' return the
  correct indentation in a variety of cases?"
  (let ((bracket-file (file-truename (concat fsharp-struct-test-files-dir "BracketIndent.fs"))))
    (using-file bracket-file
      ;; Opening bracket on same line as let, elements on same line; test element
      (goto-char 44)
      (let* ((nesting-level (fsharp-nesting-level))
             (indent-at-point (fsharp--compute-indentation-open-bracket nesting-level)))
        ;; The value we expect
        (should (eq indent-at-point 18))
        ;; Both entrypoints should have the same answer
        (should (eq indent-at-point (fsharp-compute-indentation t))))

      ;; Opening bracket on same line as let, elements on same line; test newline
      (goto-char 81)
      (let* ((nesting-level (fsharp-nesting-level))
             (indent-at-point (fsharp--compute-indentation-open-bracket nesting-level)))
        ;; The value we expect
        (should (eq indent-at-point 18))
        ;; Both entrypoints should have the same answer
        (should (eq indent-at-point (fsharp-compute-indentation t))))

      ;; Opening bracket on same line as let, elements on new line; test element
      (goto-char 148)
      (let* ((nesting-level (fsharp-nesting-level))
             (indent-at-point (fsharp--compute-indentation-open-bracket nesting-level)))
        (should (eq indent-at-point 4))
        (should (eq indent-at-point (fsharp-compute-indentation t))))

      ;; Opening bracket on same line as let, elements on new line; test newline
      (goto-char 155)
      (let* ((nesting-level (fsharp-nesting-level))
             (indent-at-point (fsharp--compute-indentation-open-bracket nesting-level)))
        (should (eq indent-at-point 4))
        (should (eq indent-at-point (fsharp-compute-indentation t))))

      ;; Opening bracket on own line; test element
      (goto-char 231)
      (let* ((nesting-level (fsharp-nesting-level))
             (indent-at-point (fsharp--compute-indentation-open-bracket nesting-level)))
        (should (eq indent-at-point 6))
        (should (eq indent-at-point (fsharp-compute-indentation t))))

      ;; Opening bracket on own line; test newline
      (goto-char 236)
      (let* ((nesting-level (fsharp-nesting-level))
             (indent-at-point (fsharp--compute-indentation-open-bracket nesting-level)))
        (should (eq indent-at-point 6))
        (should (eq indent-at-point (fsharp-compute-indentation t)))))))


(ert-deftest fsharp--compute-indentation-continuation-line ()
  (let ((continuation-line "let x = 5 +"))
    (with-temp-buffer
      (fsharp-mode)
      (insert continuation-line)
      (fsharp-newline-and-indent)
      (should (eq (fsharp--compute-indentation-continuation-line) 8))
      (should (eq (fsharp--compute-indentation-continuation-line) (fsharp-compute-indentation t))))))


(ert-deftest fsharp-compute-indentation-relative-to-previous-test ()
  (let ((relative-file (concat fsharp-struct-test-files-dir "Relative.fs")))
    ;; Discriminated unions
    (using-file relative-file
      (goto-char 57)
      (should (eq (fsharp--compute-indentation-relative-to-previous t) 4))
      (should (eq (fsharp--compute-indentation-relative-to-previous t)
                  (fsharp-compute-indentation t)))

      ;; If/Else blocks
      ;; if an if then are on the same line, the next line is indented
      (goto-char 96)
      (should (eq (fsharp--compute-indentation-relative-to-previous t) 4))
      (should (eq (fsharp--compute-indentation-relative-to-previous t)
                  (fsharp-compute-indentation t)))

      ;; An else is not indented further; *however*, the indentation relative to
      ;; previous will be 4, but `fsharp-compute-indentation' will return 0
      ;; because the previous line is not a continuation line.
      ;;
      ;; However! This test case doesn't currently work. Indentation code
      ;; produces indent of 0, but the compute indentation functions proudce an
      ;; indent of 4, which is wrong.
      ;;
      ;; (goto-char 124)
      ;; (should (eq (fsharp--compute-indentation-relative-to-previous t) 4))
      ;; (should-not (eq (fsharp--compute-indentation-relative-to-previous t)
      ;;                 (fsharp-compute-indentation t)))

      ;; when a then is on its own line, the next line is indented
      (goto-char 154)
      (should (eq (fsharp--compute-indentation-relative-to-previous t) 4))
      (should (eq (fsharp--compute-indentation-relative-to-previous t)
                  (fsharp-compute-indentation t)))
      ;; likewise an else
      (goto-char 180)
      (should (eq (fsharp--compute-indentation-relative-to-previous t) 4))
      (should (eq (fsharp--compute-indentation-relative-to-previous t)
                  (fsharp-compute-indentation t)))
      )))
