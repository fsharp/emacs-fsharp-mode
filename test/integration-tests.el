;;; integration-tests.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Jürgen Hötzel

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

(defun eglot-fsharp--sniff-diagnostics (file-name-suffix)
  (eglot--sniffing (:server-notifications s-notifs)
                   (eglot--wait-for (s-notifs 20)
                                    (&key _id method params &allow-other-keys)
                                    (and
                                     (string= method "textDocument/publishDiagnostics")
                                     (string-suffix-p file-name-suffix (plist-get params :uri))))))

(describe "F# LSP server"
	  :var (latest-version)
	  :before-all (setq latest-version (eglot-fsharp--latest-version))
          (it "can be installed @version 0.52.0"
              (eglot-fsharp--maybe-install "0.52.0")
              (expect (eglot-fsharp--installed-version) :to-equal "0.52.0"))
          (it (format "Can be installed (using latest version: %s)" latest-version)
              (eglot-fsharp--maybe-install)
              (expect (eglot-fsharp--installed-version) :to-equal latest-version))
          (it "Can be invoked"
              ;; FIXME: Should use dotnet tool run
              (expect (car (process-lines (eglot-fsharp--path-to-server) "--version"))
	              :to-match (rx line-start (? "FsAutoComplete" (1+ space)) (eval (eglot-fsharp--installed-version)))))
          (it "shows flymake errors"
              (with-current-buffer (eglot--find-file-noselect "test/Test1/Error.fs")
                (eglot--tests-connect 10)
                (search-forward "nonexisting")
                (flymake-mode t)
                (flymake-start)
                (goto-char (point-min))
                (eglot-fsharp--sniff-diagnostics "test/Test1/Error.fs")
                (flymake-goto-next-error 1 '() t)
                (expect (face-at-point) :to-be 'flymake-error )))
          (it "is enabled on F# Files"
              (with-current-buffer (eglot--find-file-noselect "test/Test1/FileTwo.fs")
                (expect (type-of (eglot--current-server-or-lose)) :to-be 'eglot-fsautocomplete)))
          (it "provides completion"
              (with-current-buffer (eglot--find-file-noselect "test/Test1/FileTwo.fs")
                (eglot-fsharp--sniff-diagnostics "test/Test1/FileTwo.fs")
                (expect (plist-get (eglot--capabilities (eglot--current-server-or-lose)) :completionProvider) :not :to-be nil)))
          (it "completes function in other modules"
              (with-current-buffer (eglot--find-file-noselect "test/Test1/Program.fs")
                (search-forward "X.func")
                (delete-char -3)
                (eglot-fsharp--sniff-diagnostics "test/Test1/Program.fs")
                (completion-at-point)
                (expect (looking-back "X\\.func") :to-be t)))
          (it "finds definition in pervasives"
              (with-current-buffer (eglot--find-file-noselect "test/Test1/Program.fs")
	        (eglot--tests-connect 10)
	        (search-forward "printfn")
	        (eglot-fsharp--sniff-diagnostics "test/Test1/Program.fs")
	        (expect (current-word) :to-equal "printfn") ;sanity check
	        (call-interactively #'xref-find-definitions)
	        (expect (file-name-nondirectory (buffer-file-name)) :to-equal "fslib-extra-pervasives.fs")))
          (it "finds definitions in other files of Project"
              (with-current-buffer (eglot--find-file-noselect "test/Test1/Program.fs")
                (goto-char 150)
                (expect (current-word) :to-equal "NewObjectType") ;sanity check
                (call-interactively #'xref-find-definitions)
                (expect (file-name-nondirectory (buffer-file-name)) :to-equal "FileTwo.fs"))))

(provide 'integration-tests)
;;; integration-tests.el ends here
