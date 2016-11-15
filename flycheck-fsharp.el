;;; fsharp-flycheck.el --- Flycheck support for F#

;; Copyright (C) 2016 Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@archlinux.org>
;; Maintainer: Robin Neatherway <robin.neatherway@gmail.com>
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Automatically configure Flycheck for F#

;;; Code:

(require 'flycheck)
(require 'fsharp-mode-completion)

(defvar flycheck-fsharp--lint-callback-info nil)

(defun flycheck-fsharp-fsautocomplete-lint-start (checker callback)
  "Start a F# syntax check with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  (setq flycheck-fsharp--lint-callback-info (cons checker callback))
  (fsharp-ac-send-pos-request
   "lint"
   (fsharp-ac--buffer-truename)
   (line-number-at-pos)
   (+ 1 (current-column))))

(flycheck-define-generic-checker 'fsharp-fsautocomplete-lint
  "A syntax checker for F# using FSharp.AutoComplete.
See URL `https://github.com/fsharp/FsAutoComplete'."
  :start #'flycheck-fsharp-fsautocomplete-lint-start
  :modes '(fsharp-mode))

(defvar flycheck-fsharp--error-callback-info nil)

(defun flycheck-fsharp-fsautocomplete-start (checker callback)
  "Start a F# syntax check with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  (setq flycheck-fsharp--error-callback-info (cons checker callback))
  (fsharp-ac-parse-current-buffer))

(flycheck-define-generic-checker 'fsharp-fsautocomplete
  "A syntax checker for F# using FSharp.AutoComplete.
See URL `https://github.com/fsharp/FsAutoComplete'."
  :start #'flycheck-fsharp-fsautocomplete-start
  :modes '(fsharp-mode)
  :next-checkers '((info . fsharp-fsautocomplete-lint)))

(defun flycheck-fsharp-handle-lint (data)
  "Extract the errors from the given process response DATA.  Return a list of `flycheck-error'."
  (-if-let ((checker . callback) flycheck-fsharp--lint-callback-info)
      (condition-case err
          (funcall callback 'finished
                   (--map (let* ((range (gethash "Range" it))
                                 (line (gethash "StartLine" range))
                                 (column (gethash "StartColumn" range))
                                 (msg (gethash "Info" it))
                                 (file (fsharp-ac--tramp-file (gethash "FileName" range))))
                            (flycheck-error-new-at
                             line column 'info msg :checker checker :filename file)) data))
        (funcall callback 'errored (error-message-string err)))
    ;; XXX this should use (funcall callback 'suspicious "the message below"),
    ;; but it would require refactoring things to be able to use this function
    ;; as the flycheck :start function instead of having a :start function that
    ;; sets the above "callback info"
    (message "Warning: `flycheck-fsharp--lint-callback-info` not set (flycheck-fsautocomplete not enabled?)")))

(defun flycheck-fsharp-handle-nothing-changed ()
  (-when-let ((checker . callback) flycheck-fsharp--error-callback-info)
    (funcall callback 'finished fsharp-ac-errors)))

(defun flycheck-fsharp-handle-errors (data)
  "Extract the errors from the given process response DATA.  Return a list of `flycheck-error'."
  (-if-let ((checker . callback) flycheck-fsharp--error-callback-info)
      (condition-case err
          (progn
            (setq
             fsharp-ac-errors
             (--map (let ((line (gethash "StartLine" it))
                          (column (gethash "StartColumn" it))
                          ;; we ignore EndLine and EndColumn here because flycheck uses
                          ;; (bounds-of-thing-at-point thing) to identify the region, see:
                          ;; (bounds-of-thing-at-point thing)
                          (level (if (string= "Error" (gethash "Severity" it))
                                     'error
                                   'warning))
                          (msg (gethash "Message" it))
                          (file (fsharp-ac--tramp-file (gethash "FileName"
                                                                it))))
                      (flycheck-error-new-at
                       line column level msg :checker checker :filename file))
                    data))
            (funcall callback 'finished fsharp-ac-errors))
        (funcall callback 'errored (error-message-string err)))
    ;; XXX this should use (funcall callback 'suspicious "the message below"),
    ;; but it would require refactoring things to be able to use this function
    ;; as the flycheck :start function instead of having a :start function that
    ;; sets the above "callback info"
    (message "Warning: `flycheck-fsharp--error-callback-info` not set (flycheck-fsautocomplete not enabled?)")))

(setq fsharp-ac-handle-errors-function 'flycheck-fsharp-handle-errors)
(setq fsharp-ac-handle-lint-function 'flycheck-fsharp-handle-lint)

(add-to-list 'flycheck-checkers 'fsharp-fsautocomplete-lint)
(add-to-list 'flycheck-checkers 'fsharp-fsautocomplete)
(provide 'flycheck-fsharp)
;;; flycheck-fsharp.el ends here
