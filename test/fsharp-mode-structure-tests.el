(require 'fsharp-mode-structure)
(require 'test-common)

(defvar fsharp-struct-test-files-dir "StructureTest/")

(ert-deftest fsharp-nesting-level--test-should-nil ()
  "Does `fsharp-nesting-level' return nil when we expect it to?"
  (with-temp-buffer
    (insert "let x = 5")
    (end-of-buffer)
    (should (eq (fsharp-nesting-level) nil))))

(defun assert-against-file (path point-pos fn expected)
  (using-file path
    (goto-char point-pos)
    (should (eq (funcall fn) expected))))


(ert-deftest fsharp-nesting-level--test-should-return-position ()
  "Does `fsharp-nesting-level' correctly return the point
position of the opening pair closest to point?"
  ;; The character positions use here reference characters noted in comments in Nesting.fs
  (let ((nesting-file (file-truename (concat fsharp-struct-test-files-dir "Nesting.fs"))))
    ;; Test a normal list
    (assert-against-file nesting-file 645 #'fsharp-nesting-level 640)
    ;; Get the opening bracket of an inner list from a single-line nested list
    (assert-against-file nesting-file 717 #'fsharp-nesting-level 706)

    ;; Opening bracket for a multi-line non-nested list
    (assert-against-file nesting-file 795 #'fsharp-nesting-level 777)

    ;; Inner most opening bracket for a multi-line multi-nested list
    (assert-against-file nesting-file 960 #'fsharp-nesting-level 955)
    ;; Middle opening bracket for same list as previous
    (assert-against-file nesting-file 954 #'fsharp-nesting-level 953)
    (assert-against-file nesting-file 974 #'fsharp-nesting-level 953)
    ;; Outermost opening bracket for same list
    (assert-against-file nesting-file 977 #'fsharp-nesting-level 947)

    ;; Basic Async form, should return the opening {
    (assert-against-file nesting-file 1088 #'fsharp-nesting-level 1060)
    ;; Same async form, inner async call
    (assert-against-file nesting-file 1129 #'fsharp-nesting-level 1121)

    ;; Lambda, wrapped in parens, should return the opening (
    (assert-against-file nesting-file 1238 #'fsharp-nesting-level 1208)
    )))
