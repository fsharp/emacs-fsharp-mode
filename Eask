(package "fsharp-mode"
         "1.11"
         "Support for the F# programming language")

(package-file "fsharp-mode.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "25")

;; FIXME: Use multiple packages: https://github.com/melpa/melpa#example-multiple-packages-in-one-repository ?
(development
 (depends-on "buttercup")
 (depends-on "eglot"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
