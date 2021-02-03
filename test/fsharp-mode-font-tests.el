;;; fsharp-mode-font-tests.el ---                         -*- lexical-binding: t; -*-

(require 'buttercup)

(require 'fsharp-mode)
(require 'fsharp-mode-font)

(defmacro with-highlighted (src &rest body)
  "Insert SRC in a temporary fsharp-mode buffer, apply syntax highlighting,
then run BODY."
  `(with-temp-buffer
     (fsharp-mode)
     (insert ,src)
     (goto-char (point-min))
     ;; Ensure we've syntax-highlighted the whole buffer.
     (if (fboundp 'font-lock-ensure)
         (font-lock-ensure)
       (with-no-warnings
         (font-lock-fontify-buffer)))
     ,@body))

(defun str-face (op)
  (goto-char (point-min))
  (search-forward op)
  (left-char)
  (face-at-point))

(describe "When locking operators"
  (it "uses ui operator face for pipes"
    (with-highlighted "<<| |>> |> ||> |||> <| <|| <||| <|> <<|!"
      (should (eq (str-face "|>") 'fsharp-ui-operator-face))
      (should (eq (str-face "||>") 'fsharp-ui-operator-face))
      (should (eq (str-face "|||>") 'fsharp-ui-operator-face))
      (should (eq (str-face "<|") 'fsharp-ui-operator-face))
      (should (eq (str-face "<||") 'fsharp-ui-operator-face))
      (should (eq (str-face "<|||") 'fsharp-ui-operator-face)))))

(describe "When locking operators"
  (it "uses ui generic face for custom operators containing pipes"
    (with-highlighted "<<| |>> |> ||> |||> <| <|| <||| <|> <<|!"
      (should (eq (str-face "<<|") 'fsharp-ui-generic-face))
      (should (eq (str-face "|>>") 'fsharp-ui-generic-face))
      (should (eq (str-face "<|>") 'fsharp-ui-generic-face))
      (should (eq (str-face "<<|!") 'fsharp-ui-generic-face)))))
