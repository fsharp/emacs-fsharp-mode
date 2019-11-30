;;; integration-tests.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;; Keywords: abbrev, abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is part of fsharp-mode

;;; Code:

(require 'buttercup)
(require 'eglot-fsharp)
(require 'eglot-tests)

(defun eglot-fsharp--sniff-diagnostics ()
  (eglot--sniffing (:server-notifications s-notifs)
    (eglot--wait-for (s-notifs 20)
	(&key _id method &allow-other-keys)
      (string= method "textDocument/publishDiagnostics"))))

(describe "F# LSP server"
  (it "Can be installed"
    (eglot-fsharp--maybe-install)
    (expect (file-exists-p  (eglot-fsharp--path-to-server)) :to-be t))
  (it "shows flymake errors"
    (with-current-buffer (eglot--find-file-noselect "test/Test1/Error.fs")
      (eglot--tests-connect 10)
      (search-forward "nonexisting")
      (flymake-mode t)
      (flymake-start)
      (goto-char (point-min))
      (eglot-fsharp--sniff-diagnostics)
      (flymake-goto-next-error 1 '() t)
      (expect (face-at-point) :to-be 'flymake-error )))
  (it "is enabled on F# Files"
    (with-current-buffer (eglot--find-file-noselect "test/Test1/FileTwo.fs")
      (expect (type-of (eglot--current-server-or-lose)) :to-be 'eglot-fsautocomplete)))
  (it "provides completion"
    (with-current-buffer (eglot--find-file-noselect "test/Test1/FileTwo.fs")
      (eglot-fsharp--sniff-diagnostics)
      (expect (plist-get (eglot--capabilities (eglot--current-server-or-lose)) :completionProvider) :not :to-be nil)))
  (it "completes function in other modules"
    (with-current-buffer (eglot--find-file-noselect "test/Test1/Program.fs")
      (search-forward "X.func")
      (delete-char -3)
      ;; ERROR in fsautocomplet.exe?  Should block instead of "no type check results"
      (eglot-fsharp--sniff-diagnostics)
      (completion-at-point)
      (expect (looking-back "X\\.func") :to-be t)))
  (it "doesn't throw error when definition does not exist"
    (with-current-buffer (eglot--find-file-noselect "test/Test1/Program.fs")
      (eglot-fsharp--sniff-diagnostics)
      (goto-char 253)
      (expect (current-word) :to-equal "printfn") ;sanity check
      (expect
       (condition-case err
	   (call-interactively #'xref-find-definitions)
	 (user-error
	  (cadr err)))
       :to-equal "No definitions found for: LSP identifier at point.")))
  (it "finds definitions in other files of Project"
    (with-current-buffer (eglot--find-file-noselect "test/Test1/Program.fs")
      (goto-char 150)
      (expect (current-word) :to-equal "NewObjectType") ;sanity check
      (call-interactively #'xref-find-definitions)
      (expect (file-name-nondirectory (buffer-file-name)) :to-equal "FileTwo.fs"))))

(provide 'integration-tests)
;;; integration-tests.el ends here
