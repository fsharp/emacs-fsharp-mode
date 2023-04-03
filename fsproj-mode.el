;;; fsproj-mode.el -- fsproj-mode eglot fsharp integration                             -*- lexical-binding: t; -*-
;; Copyright (C) 2023  Andrew McGuier

;; Author: Andrew McGuier <amcguier@gmail.com>
;; Package-Requires: ((emacs "27.1") (eglot "1.4") (jsonrpc "1.0.14"))
;; Version: 1.10
;; Keywords: languages
;; URL: https://github.com/fsharp/emacs-fsharp-mode

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


;;; Code:
(require 'dom)
(require 'eglot)

(defvar fsproj-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'fsproj-move-up)
    (define-key map "d" 'fsproj-move-down) map)
  "Local keymap for `fsproj-mode' buffers.")


(defun fsproj--read-files (file-name)
  "Pull out all the compileable files in an fsproj file in FILE-NAME."
  (with-temp-buffer (insert-file-contents file-name)
                    (mapcar (lambda (x)
                              (list nil (vector (dom-attr x 'Include))))
                            (dom-by-tag (libxml-parse-xml-region (point-min)
                                                                 (point-max)) 'Compile))))

(defun fsproj--set-tab-list ()
  "Use the local fsproj-name variable to calculate the list of fsproj files to display."
  (setq-local tabulated-list-entries (fsproj--read-files fsproj-name)))


(defun fsproj-list-files () 
  "Read an fsproj file contents and allow manipulating the file contents.
This functionality requires eglot to function and should be called on an 
fs file with an active eglot session."
  (interactive)
  (let ((current-server (eglot--current-server-or-lose))
        (fsproj-nm (fsharp-mode/find-sln-or-fsproj (buffer-file-name))))
    (pop-to-buffer (concat "*" (file-name-nondirectory fsproj-nm)  " info*"))
    (fsproj-mode)
    (setq-local current-eglot-server current-server)
    (setq-local fsproj-name fsproj-nm)
    (fsproj--set-tab-list)
    (tabulated-list-print t)))

(defun fsproj-move-up ()
  "Move file up in the compilation order."
  (interactive)
  (let ((file-name (elt (tabulated-list-get-entry) 0)))
    (if file-name (progn (jsonrpc-request current-eglot-server
                                          :fsproj/moveFileUp `(:fsProj ,fsproj-name
                                                                       :fileVirtualPath ,file-name))
                         (tabulated-list-revert)
                         (previous-line)))))

(defun fsproj-move-down ()
  "Move file up in the compilation order."
  (interactive)
  (let ((file-name (elt (tabulated-list-get-entry) 0)))
    (if file-name (progn (jsonrpc-request current-eglot-server
                                          :fsproj/moveFileDown `(:fsProj ,fsproj-name
                                                                         :fileVirtualPath ,file-name))
                         (tabulated-list-revert)
                         (next-line)))))

(define-derived-mode fsproj-mode tabulated-list-mode
  "fsproj"
  "Major mode for fsproj."
  (setq-local tabulated-list-format [("File Name" 10 nil) ])
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'fsproj--set-tab-list nil t))

(add-to-list 'auto-mode-alist '("\\.fsproj info?\\'" . fsproj-mode))



(provide 'fsproj-mode)
;;; fsproj-mode.el ends here
