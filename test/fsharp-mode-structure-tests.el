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
