;;; eglot-fsharp.el --- fsharp-mode eglot integration                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Jürgen Hötzel

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

(defcustom eglot-fsharp-fsautocomplete-args '(
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
    :enableReferenceCodeLens t)
    "Arguments for the fsautocomplete initialization."
    :group 'eglot-fsharp
    :risky t
  )

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
  (with-temp-buffer
    (process-file "dotnet" nil t nil "tool" "list" "--tool-path" (file-name-directory (eglot-fsharp--path-to-server)))
    (goto-char (point-min))
    (when (search-forward-regexp "^fsautocomplete[[:space:]]+\\([0-9\.]*\\)[[:space:]]+" nil t)
      (match-string 1))))

(defun eglot-fsharp-current-version-p (version)
  "Return t if the installation is up-to-date compared to VERSION string."
  (and (file-exists-p (concat (file-remote-p default-directory) (eglot-fsharp--path-to-server)))
       (equal version (eglot-fsharp--installed-version))))

(defun eglot-fsharp--install-core (version)
  "Download and install fsautocomplete as a dotnet tool at version VERSION in `eglot-fsharp-server-install-dir'."
  (let* ((default-directory (concat (file-remote-p default-directory)
                                    (file-name-directory (eglot-fsharp--path-to-server))))
         (stderr-file (make-temp-file "dotnet_stderr"))
         (local-tool-path (or (file-remote-p default-directory 'localname) default-directory)))
    (condition-case err
        (progn
          (unless (eglot-fsharp-current-version-p version)
            (message "Installing fsautocomplete version %s" version)
            (when (file-exists-p (concat (file-remote-p default-directory)
                                         (eglot-fsharp--path-to-server)))
	      (unless (zerop (process-file "dotnet" nil `(nil
							  ,stderr-file)
					   nil "tool" "uninstall"
					   "fsautocomplete" "--tool-path"
					   local-tool-path))
                (error  "'dotnet tool uninstall fsautocomplete --tool-path %s' failed" default-directory))))
          (unless (zerop (process-file "dotnet" nil `(nil ,stderr-file) nil
				       "tool" "install" "fsautocomplete"
				       "--tool-path" local-tool-path "--version"
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
  (make-directory (concat (file-remote-p default-directory)
                          (file-name-directory (eglot-fsharp--path-to-server))) t)
  (let* ((version (or version (if (eq eglot-fsharp-server-version 'latest)
				  (eglot-fsharp--latest-version)
				eglot-fsharp-server-version))))
    (unless (eglot-fsharp-current-version-p version)
      (eglot-fsharp--install-core version))))

 ;;;###autoload
(defun eglot-fsharp (interactive)
    "Return `eglot' contact when FsAutoComplete is installed.
Ensure FsAutoComplete is installed (when called INTERACTIVE)."
  (when interactive (eglot-fsharp--maybe-install))
  (cons 'eglot-fsautocomplete
        (if (file-remote-p default-directory)
            `("sh" ,shell-command-switch ,(concat "cat|"  (mapconcat #'shell-quote-argument
                                                                     (cons (eglot-fsharp--path-to-server) eglot-fsharp-server-args) " ")))
          (cons (eglot-fsharp--path-to-server) eglot-fsharp-server-args))))


(defclass eglot-fsautocomplete (eglot-lsp-server) ()
  :documentation "F# FsAutoComplete langserver.")

(cl-defmethod eglot-initialization-options ((_server eglot-fsautocomplete))
  "Passes through required FsAutoComplete initialization options."
  `(:fSharp ,eglot-fsharp-fsautocomplete-args))

;; FIXME: this should be fixed in FsAutocomplete
(cl-defmethod xref-backend-definitions :around ((_type symbol) _identifier)
  "FsAutoComplete breaks spec and and return error instead of empty list."
  (if (eq major-mode 'fsharp-mode)
      (condition-case err
          (cl-call-next-method)
        (jsonrpc-error
         (not (equal (cadddr err) '(jsonrpc-error-message . "Could not find declaration")))))
    (when (cl-next-method-p)
      (cl-call-next-method))))

;;; create buffer local settings for workspace reload based on mode hook

(defun eglot-fsharp--set-workspace-args ()
  "Set a buffer local variable with the workspace settings for eglot."
  (make-local-variable 'eglot-workspace-configuration)
  (let ((settings-json (json-serialize `(:fSharp ,eglot-fsharp-fsautocomplete-args))) )
    (setq eglot-workspace-configuration settings-json ))
  )

(add-to-list 'eglot-server-programs `(fsharp-mode . eglot-fsharp))

(provide 'eglot-fsharp)
;;; eglot-fsharp.el ends here
