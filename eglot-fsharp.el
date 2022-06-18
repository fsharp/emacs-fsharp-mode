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

(defcustom eglot-fsharp-server-verbose nil
  "If non-nil include debug output in the server logs.")

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
                             (concat "netcore/fsautocomplete" (if (eq system-type 'windows-nt) ".exe" ""))
                           "netframework/fsautocomplete.exe"))))

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

(defun eglot-fsharp--install-w32 (version)
  "Download and install the full framework version of F# compiler service at version VERSION in `eglot-fsharp-server-install-dir'."
  (let* ((url (format "https://github.com/fsharp/FsAutoComplete/releases/download/%s/fsautocomplete.zip"
                      version))
	 (exe (eglot-fsharp--path-to-server))
         (zip (concat (file-name-directory exe) "fsautocomplete.zip"))
         (gnutls-algorithm-priority
          (if (and (not gnutls-algorithm-priority)
                   (boundp 'libgnutls-version)
                   (>= libgnutls-version 30603)
                   (version<= emacs-version "26.2"))
              "NORMAL:-VERS-TLS1.3"
            gnutls-algorithm-priority))))
  (unless (eglot-fsharp-current-version-p version)
    (url-copy-file url zip t)
    ;; FIXME: Windows (unzip preinstalled?)
    (let ((default-directory (file-name-directory (eglot-fsharp--path-to-server))))
      (unless (zerop (call-process "unzip" nil nil nil "-o" zip))
        (error "Failed to unzip %s" zip))
      (unless (eq system-type 'windows-nt)
	(dolist  (file (directory-files-recursively (file-name-directory (eglot-fsharp--path-to-server)) "." t))
	  (if (file-directory-p file)
	      (chmod file #o755)
	    (chmod file #o644)))))
    (delete-file zip)))


(defun eglot-fsharp--process-tool-action (response)
  "Process the result of calling the dotnet tool installation returning RESPONSE code."
  (when (>= response 1)
    (error "Failed to install dotnet tool fsautocomplete: %s"
           (with-temp-buffer
             (insert-file-contents "error_output.txt")
             (buffer-string)))))

(defun eglot-fsharp--install-core (version)
  "Download and install fsautocomplete as a dotnet tool at version VERSION in `eglot-fsharp-server-install-dir'."
  (let ((default-directory (file-name-directory (eglot-fsharp--path-to-server))))
    (unless (eglot-fsharp-current-version-p version)
      (message "Installing fsautocomplete version %s" version)
      (if (file-exists-p (eglot-fsharp--path-to-server))
	  (eglot-fsharp--process-tool-action	  (call-process "dotnet" nil '(nil
									       "error_output.txt")
								nil "tool" "uninstall"
								"fsautocomplete" "--tool-path"
								default-directory)))
      (eglot-fsharp--process-tool-action (call-process "dotnet" nil '(nil "error_output.txt") nil
						       "tool" "install" "fsautocomplete"
						       "--tool-path" default-directory "--version"
						       version)))))

(defun eglot-fsharp--maybe-install (&optional version)
  "Downloads F# compiler service, and install in `eglot-fsharp-server-install-dir'."
  (make-directory (file-name-directory (eglot-fsharp--path-to-server)) t)
  (let* ((version (or version (if (eq eglot-fsharp-server-version 'latest)
				  (eglot-fsharp--latest-version)
				eglot-fsharp-server-version))))
    (if (eq eglot-fsharp-server-runtime 'net-core)
	(eglot-fsharp--install-core version)
      (eglot-fsharp--install-w32 version))))

 ;;;###autoload
(defun eglot-fsharp
    (interactive)
  "Return `eglot' contact when FsAutoComplete is installed.
Ensure FsAutoComplete is installed (when called INTERACTIVE)."
  (when interactive (eglot-fsharp--maybe-install))
  (when (file-exists-p (eglot-fsharp--path-to-server))
    (let ((cmd-list (cond ((eq eglot-fsharp-server-runtime 'net-core)
			   `(,(eglot-fsharp--path-to-server)))
			  ((eq window-system 'w32)
			   `("" , (eglot-fsharp--path-to-server)))
			  (t `("mono" ,(eglot-fsharp--path-to-server)))))
	  (arg-list (if eglot-fsharp-server-verbose
			`("--background-service-enabled" "-v")
            	      `("--background-service-enabled")
		      )))
      (cons 'eglot-fsautocomplete (append cmd-list arg-list)))))




(defclass eglot-fsautocomplete (eglot-lsp-server) ()
  :documentation "F# FsAutoComplete langserver.")

(cl-defmethod eglot-initialization-options ((_server eglot-fsautocomplete))
  "Passes through required FsAutoComplete initialization options."
  '(:automaticWorkspaceInit t))

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
