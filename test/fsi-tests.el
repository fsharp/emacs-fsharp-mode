;;; fsi-tests.el --- Tests for F# interactive        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;; Keywords: processes

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

;; 

;;; Code:

(load "project")                        ;Emacs 27 workaround: https://github.com/joaotavora/eglot/issues/549
(require 'buttercup)
(require 'fsharp-mode)

(defun fsi-tests-wait-for-regex (timeout regexp)
  (while (and (> timeout 0) (not (progn (goto-char (point-min)) (search-forward-regexp regexp nil t))))
    (message "[FSI Interactive] Waiting a bit...")
    (accept-process-output (get-buffer-process (current-buffer)) 0.2)
    (setq timeout (1- timeout))))


(describe "F# interactive"
          :before-all (run-fsharp inferior-fsharp-program)
          :before-each (with-current-buffer (get-buffer inferior-fsharp-buffer-name)
                         (comint-clear-buffer))
          (it "can eval expressions"
              (let ((fsharp-autosave-on-file-load t)
                    (fsx-file (make-temp-file "fsi" nil ".fsx" "
1 + 1;;
")))
                (with-current-buffer (find-file-noselect fsx-file)
                  (fsharp-eval-region (point-min) (point-max))
                  (with-current-buffer (get-buffer inferior-fsharp-buffer-name)
                    (fsi-tests-wait-for-regex 25 "it: int = 2$")
                    (let ((result (match-string-no-properties 0)))
                      (expect result :to-equal "it: int = 2"))))))
          (it "can load nuget references"
              (let ((fsharp-autosave-on-file-load t)
                    (timeout 50)
                    (fsx-file (make-temp-file "fsi" nil ".fsx" "
#r \"nuget: Newtonsoft.Json\";;
open Newtonsoft.Json;;

let o = {| X = 2; Y = \"Hello\" |};;

printfn \"xxx:%s:xxx\" (JsonConvert.SerializeObject o);;")))
                (with-current-buffer (find-file-noselect fsx-file)
                  (fsharp-load-buffer-file)
                  (with-current-buffer (get-buffer inferior-fsharp-buffer-name)
                    (fsi-tests-wait-for-regex 25 "xxx:\\(.*\\):xxx")
                    (let ((json-str (match-string-no-properties 1)))
                      (unless json-str
                        (warn "FSI output doesn't contain marker: %s" (buffer-substring-no-properties (point-min) (point-max))))
                      (expect json-str :to-equal "{\"X\":2,\"Y\":\"Hello\"}")))))))

(provide 'fsi-tests)
;;; fsi-tests.el ends here