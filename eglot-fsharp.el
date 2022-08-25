;;; eglot-fsharp.el --- fsharp-mode eglot integration                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;; Package-Requires: ((emacs "27.1") (eglot "1.4") (fsharp-mode "1.10") (jsonrpc "1.0.14"))
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

(defcustom eglot-fsharp-server-version 'latest
  "FsAutoComplete version to install or update."
  :group 'eglot-fsharp
  :risky t
  :type '(choice
          (const :tag "Latest release" latest)
          (string :tag "Version string")))

(defcustom eglot-fsharp-server-args '("--adaptive-lsp-server-enabled")
  "Arguments for the fsautocomplete command when using `eglot-fsharp'."
  :type '(repeat string))

(defun eglot-fsharp--path-to-server ()
  "Return FsAutoComplete path."
  (file-truename (concat eglot-fsharp-server-install-dir "netcore/fsautocomplete" (if (eq system-type 'windows-nt) ".exe" ""))))

;; cache to prevent repetitive queries
(defvar eglot-fsharp--latest-version nil "Latest fsautocomplete.exe version string.")

(defun eglot-fsharp--latest-version ()
  "Return latest fsautocomplete.exe version."
  (let* ((json (with-temp-buffer (url-insert-file-contents "https://azuresearch-usnc.nuget.org/query?q=fsautocomplete&prerelease=false&packageType=DotnetTool")
			         (json-parse-buffer)))
         (versions (gethash "versions" (aref (gethash "data" json) 0))))
    (gethash "version" (aref versions (1- (length versions))))))

(defun eglot-fsharp--installed-version ()
  "Return version string of fsautocomplete."
  (seq-some (lambda (s) (and (string-match "^fsautocomplete[[:space:]]+\\([0-9\.]*\\)[[:space:]]+" s) (match-string 1 s)))
	    (process-lines "dotnet"  "tool" "list" "--tool-path" (file-name-directory (eglot-fsharp--path-to-server)))))

(defun eglot-fsharp-current-version-p (version)
  "Return t if the installation is not outdated."
  (when (file-exists-p (eglot-fsharp--path-to-server))
    (if (eq version 'latest)
	(equal (eglot-fsharp--latest-version)
	       (eglot-fsharp--installed-version))
      (equal eglot-fsharp-server-version (eglot-fsharp--installed-version)))))

(defun eglot-fsharp--install-core (version)
  "Download and install fsautocomplete as a dotnet tool at version VERSION in `eglot-fsharp-server-install-dir'."
  (let ((default-directory (file-name-directory (eglot-fsharp--path-to-server)))
        (stderr-file (make-temp-file "dotnet_stderr")))
    (condition-case err
        (progn
          (unless (eglot-fsharp-current-version-p version)
            (message "Installing fsautocomplete version %s" version)
            (when (file-exists-p (eglot-fsharp--path-to-server))
	      (unless (zerop (call-process "dotnet" nil `(nil
							  ,stderr-file)
					   nil "tool" "uninstall"
					   "fsautocomplete" "--tool-path"
					   default-directory))
                (error  "'dotnet tool uninstall fsautocomplete --tool-path %s' failed" default-directory))))
          (unless (zerop (call-process "dotnet" nil `(nil ,stderr-file) nil
				       "tool" "install" "fsautocomplete"
				       "--tool-path" default-directory "--version"
				       version))
            (error "'dotnet tool install fsautocomplete --tool-path %s --version %s' failed" default-directory  version)))
      (error
       (let ((stderr (with-temp-buffer
                       (insert-file-contents stderr-file)
                       (buffer-string))))
         (delete-file stderr-file)
         (signal (car err) (format "%s: %s" (cdr err) stderr)))))))

(defun eglot-fsharp--maybe-install (&optional version)
  "Downloads F# compiler service, and install in `eglot-fsharp-server-install-dir'."
  (make-directory (file-name-directory (eglot-fsharp--path-to-server)) t)
  (let* ((version (or version (if (eq eglot-fsharp-server-version 'latest)
				  (eglot-fsharp--latest-version)
				eglot-fsharp-server-version))))
    (eglot-fsharp--install-core version)))

 ;;;###autoload
(defun eglot-fsharp
    (interactive)
  "Return `eglot' contact when FsAutoComplete is installed.
Ensure FsAutoComplete is installed (when called INTERACTIVE)."
  (when interactive (eglot-fsharp--maybe-install))
  (when (file-exists-p (eglot-fsharp--path-to-server))
    (cons 'eglot-fsautocomplete (cons (eglot-fsharp--path-to-server)
                                      eglot-fsharp-server-args))))


(defclass eglot-fsautocomplete (eglot-lsp-server) ()
  :documentation "F# FsAutoComplete langserver.")

(cl-defmethod eglot-initialization-options ((_server eglot-fsautocomplete))
  "Passes through required FsAutoComplete initialization options."
  '(
    :automaticWorkspaceInit t
    :keywordsAutocomplete t
    :externalAutocomplete nil
    :linter t
    :unionCaseStubGeneration t
    :recordStubGeneration t
    :interfaceStubGeneration t
    :interfaceStubGenerationObjectIdentifier "this"
    :unusedOpensAnalyzer t
    :unusedDeclarationsAnalyzer t
    :useSdkScripts t
    :simplifyNameAnalyzer nil
    :resolveNamespaces t
    :enableReferenceCodeLens t))

;; FIXME: this should be fixed in FsAutocomplete
(cl-defmethod xref-backend-definitions :around ((_type symbol) _identifier)
  "FsAutoComplete breaks spec and and returns error instead of empty list."
  (if (eq major-mode 'fsharp-mode)
      (condition-case err
          (cl-call-next-method)
        (jsonrpc-error
         (not (equal (cadddr err) '(jsonrpc-error-message . "Could not find declaration")))))
    (when (cl-next-method-p)
      (cl-call-next-method))))

(add-to-list 'eglot-server-programs `(fsharp-mode . eglot-fsharp))

(provide 'eglot-fsharp)
;;; eglot-fsharp.el ends here
