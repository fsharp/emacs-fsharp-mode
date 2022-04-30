(package "fsharp-mode"
         "1.11"
         "Support for the F# programming language")

;; XXX: this is multiple-packages-in-one-repository situation, ignore/unset
;; package main file.
;;(package-file "fsharp-mode.el")

(files "*.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "25")
(depends-on "eglot")
(depends-on "jsonrpc")

(development
 (depends-on "buttercup"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
