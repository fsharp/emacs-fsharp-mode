;;; integration-tests.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Jürgen Hötzel

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

(load "project")                        ;Emacs 27 workaround: https://github.com/joaotavora/eglot/issues/549
(require 'buttercup)
(require 'eglot-fsharp)
(load "test/eglot-fsharp-integration-util.el")

;; FIXME/HELP WANTED: fsautocomplete process don't seem to terminate on windows (Access denied when trying to install
;; different version)
(unless (eq system-type 'windows-nt)
  (describe "F# LSP Installation"
            :before-all (setq latest-version (eglot-fsharp--latest-version))
            (it "succeeds using version 0.52.0"
                (eglot-fsharp--maybe-install "0.52.0")
                (expect (eglot-fsharp--installed-version) :to-equal "0.52.0"))
            (it (format "succeeds using latest version: %s)" latest-version)
                (eglot-fsharp--maybe-install)
                (expect (eglot-fsharp--installed-version) :to-equal latest-version))))

(describe "F# LSP Client"
  :before-all (progn (setq latest-version (eglot-fsharp--latest-version))
                     (with-temp-buffer (unless (zerop (process-file "dotnet"  nil (current-buffer) nil "restore" "test/Test1"))
                                         (signal 'file-error (buffer-string))))
                     (eglot-fsharp--maybe-install)
                     (with-current-buffer (eglot-fsharp--find-file-noselect "test/Test1/FileTwo.fs")
                       (eglot-fsharp--tests-connect 10)
                       ;; (eglot-fsharp--sniff-method "fsharp/notifyWorkspace")
                       )
                     )

  (it "Can be invoked"
    ;; FIXME: Should use dotnet tool run
    (expect (process-file (eglot-fsharp--path-to-server) nil nil nil "--version")
            :to-equal 0))
  (it "is enabled on F# Files"
    (with-current-buffer (eglot-fsharp--find-file-noselect "test/Test1/FileTwo.fs")
      (expect (type-of (eglot--current-server-or-lose)) :to-be 'eglot-fsautocomplete)))
  ;; (it "shows flymake errors"
  ;;   (with-current-buffer (eglot-fsharp--find-file-noselect "test/Test1/Error.fs")
  ;;     (flymake-mode t)
  ;;     (flymake-start)
  ;;     (eglot-fsharp--sniff-diagnostics "test/Test1/Error.fs")
  ;;     (goto-char (point-min))
  ;;     (search-forward "nonexisting")
  ;;     (insert "x")
  ;;     (eglot--signal-textDocument/didChange)
  ;;     (flymake-goto-next-error 1 '() t)
  ;;     (expect (face-at-point) :to-be 'flymake-error )))
  (it "provides completion"
    (with-current-buffer (eglot-fsharp--find-file-noselect "test/Test1/FileTwo.fs")
      (expect (plist-get (eglot--capabilities (eglot--current-server-or-lose)) :completionProvider) :not :to-be nil)))
  (it "completes function in other modules"
    (with-current-buffer (eglot-fsharp--find-file-noselect "test/Test1/Program.fs")
      (search-forward "X.func")
      (delete-char -3)
      (completion-at-point)
      (expect (looking-back "X\\.func") :to-be t)))
  (it "finds definition in pervasives"
    (with-current-buffer (eglot-fsharp--find-file-noselect "test/Test1/Program.fs")
      (search-forward "printfn")
      (expect (current-word) :to-equal "printfn") ;sanity check
      (call-interactively #'xref-find-definitions)
      (expect (file-name-nondirectory (buffer-file-name)) :to-equal "fslib-extra-pervasives.fs")))
  (it "finds definitions in other files of Project"
    (with-current-buffer (eglot-fsharp--find-file-noselect "test/Test1/Program.fs")
      (goto-char 150)
      (expect (current-word) :to-equal "NewObjectType") ;sanity check
      (call-interactively #'xref-find-definitions)
      (expect (file-name-nondirectory (buffer-file-name)) :to-equal "FileTwo.fs"))))


(provide 'integration-tests)
;;; integration-tests.el ends here
