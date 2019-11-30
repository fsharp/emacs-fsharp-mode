;;; eglot-fsharp.el --- Lua eglot integration                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@archlinux.org>
;; Package-Requires: ((eglot "1.4"))
;; Keywords: languages

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

;; Lua eglot introduced

;;; Code:

(require 'eglot)
(require 'fsharp-mode)
(require 'gnutls)

(defgroup eglot-fsharp nil
  "LSP support for the F# Programming Language, using F# compiler service."
  :link '(url-link "https://github.com/fsharp/FsAutoComplete")
  :group 'eglot)

(defcustom eglot-fsharp-server-install-dir
  (locate-user-emacs-file "FsAutoComplete/")
  "Install directory for FsAutoComplete."
  :group 'eglot-fsharp
  :risky t
  :type 'directory)

(defcustom eglot-fsharp-server-version "0.38.1"
  "FsAutoComplete version."
  :group 'eglot-fsharp
  :risky t
  :type 'string)

(defcustom eglot-fsharp-server-runtime
  (if (executable-find "dotnet")
      'net-core
    'net-framework)
  "The .NET runtime to use."
  :group 'eglot-fsharp
  :type '(choice (const :tag "Use .Net Core" net-core)
                 (const :tag "Use .Net Framework" net-framework)))

(defun eglot-fsharp--path-to-server ()
  "Return FsAutoComplete path."
  (file-truename (concat eglot-fsharp-server-install-dir
			 (if (eq eglot-fsharp-server-runtime 'net-core)
			     "netcore/fsautocomplete.dll"
			   "netframework/fsautocomplete.exe"))))

(defun eglot-fsharp--maybe-install ()
  "Downloads F# compiler service, and install in `eglot-fsharp-server-install-dir'."
  (make-directory (file-name-directory (eglot-fsharp--path-to-server)) t)
  (let* ((url (format "https://ci.appveyor.com/api/projects/fsautocomplete/fsautocomplete/artifacts/bin/pkgs/fsautocomplete%szip?branch=master"
		      (if (eq eglot-fsharp-server-runtime 'net-core)
			  ".netcore."
			".")))
	 (exe (eglot-fsharp--path-to-server))
	 (zip (concat (file-name-directory exe) (file-name-nondirectory url)))
	 (gnutls-algorithm-priority
	  (if (and (not gnutls-algorithm-priority)
			  (boundp 'libgnutls-version)
			  (>= libgnutls-version 30603)
			  (version<= emacs-version "26.2"))
		     "NORMAL:-VERS-TLS1.3"
		   gnutls-algorithm-priority)))
    (unless (file-exists-p exe)
      (url-copy-file url zip t)
      ;; FIXME: Windows
      (let ((default-directory (file-name-directory (eglot-fsharp--path-to-server))))
	(unless (zerop (call-process "unzip" nil nil nil "-x" zip))
	  (error "Failed to unzip %s" zip))))))

 ;;;###autoload
(defun eglot-fsharp (interactive)
"Return `eglot' contact when FsAutoComplete is installed.
Ensure FsAutoComplete is installed (when called INTERACTIVE)."
  (unless (or (file-exists-p (eglot-fsharp--path-to-server)) (not interactive))
    (eglot-fsharp--maybe-install))
  (when (file-exists-p (eglot-fsharp--path-to-server))
    (cons 'eglot-fsautocomplete
	  `(,(if (eq eglot-fsharp-server-runtime 'net-core)
		 "dotnet"
	       ;; FIXME: Windows
	       "mono") ,(eglot-fsharp--path-to-server) "--background-service-enabled"))))


(defclass eglot-fsautocomplete (eglot-lsp-server) ()
  :documentation "F# FsAutoComplete langserver.")

(cl-defmethod eglot-initialization-options ((server eglot-fsautocomplete))
  "Passes through required FsAutoComplete initialization options."
  '(:automaticWorkspaceInit t))

;; FIXME: this should be fixed in FsAutocomplete
(cl-defmethod xref-backend-definitions :around ((type symbol) _identifier)
  "FsAutoComplete breaks spec and and returns error instead of empty list."
  (if (eq major-mode 'fsharp-mode)
      (condition-case err
	  (cl-call-next-method)
	(jsonrpc-error
	 (when (equal (cadddr err) '(jsonrpc-error-message . "Could not find declaration"))
	   nil)))
    (when (cl-next-method-p)
      (cl-call-next-method))))

(add-to-list 'eglot-server-programs `(fsharp-mode . eglot-fsharp))

(provide 'eglot-fsharp)
;;; eglot-fsharp.el ends herep
