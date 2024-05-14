;;; eglot-fsharp.el --- fsharp-mode eglot integration                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024  Jürgen Hötzel

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
  :type '(choice directory (const :tag "Use dotnet default for tool-path" nil)))

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
                                              :abstractClassStubGeneration t
				              :abstractClassStubGenerationMethodBody
				              "failwith \"Not Implemented\""
				              :abstractClassStubGenerationObjectIdentifier "this"
				              :addFsiWatcher nil
				              :codeLenses (:references (:enabled t)
							               :signature (:enabled t))
				              :disableFailedProjectNotifications nil
				              :dotnetRoot ""
				              :enableAdaptiveLspServer t
				              :enableAnalyzers nil
				              :enableMSBuildProjectGraph nil
				              :enableReferenceCodeLens t
				              :excludeProjectDirectories [".git" "paket-files" ".fable" "packages" "node_modules"]
				              :externalAutocomplete nil
				              :fsac (:attachDebugger nil
                                                                     :cachedTypeCheckCount 200
				                                     :conserveMemory nil
				                                     :dotnetArgs nil
				                                     :netCoreDllPath ""
				                                     :parallelReferenceResolution nil
				                                     :silencedLogs nil)
				              :fsiExtraParameters nil
				              :fsiSdkFilePath ""
				              :generateBinlog nil
				              :indentationSize 4
				              :inlayHints (:disableLongTooltip nil
								               :enabled t
								               :parameterNames t
								               :typeAnnotations t)
				              :inlineValues (:enabled nil
							              :prefix "//")
				              :interfaceStubGeneration t
				              :interfaceStubGenerationMethodBody "failwith \"Not Implemented\""
				              :interfaceStubGenerationObjectIdentifier "this"
				              :keywordsAutocomplete t
				              :lineLens (:enabled "replaceCodeLens"
					                          :prefix " // ")
				              :linter t
				              :pipelineHints (:enabled t
							               :prefix " // ")
				              :recordStubGeneration t
				              :recordStubGenerationBody "failwith \"Not Implemented\""
				              :resolveNamespaces t
				              :saveOnSendLastSelection nil
				              :simplifyNameAnalyzer t
				              :smartIndent nil
				              :suggestGitignore t
				              :suggestSdkScripts t
				              :unionCaseStubGeneration t
				              :unionCaseStubGenerationBody "failwith \"Not Implemented\""
				              :unusedDeclarationsAnalyzer t
				              :unusedOpensAnalyzer t
				              :verboseLogging nil
				              :workspaceModePeekDeepLevel 4
				              :workspacePath "")
  "Arguments for the fsautocomplete workspace configuration."
  :group 'eglot-fsharp
  :risky t
  )

(defun eglot-fsharp--path-to-server ()
  "Return FsAutoComplete path."
  (let ((base (if eglot-fsharp-server-install-dir
                  (concat eglot-fsharp-server-install-dir "netcore/")
                "~/.dotnet/tools/")))
    (expand-file-name (concat base "fsautocomplete" (if (eq system-type 'windows-nt) ".exe" "")))))

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
    (if eglot-fsharp-server-install-dir
        (process-file "dotnet" nil t nil "tool" "list" "--tool-path" (file-name-directory (eglot-fsharp--path-to-server)))
      (process-file "dotnet" nil t nil "tool" "list" "-g"))
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
         (local-tool-path (or (file-remote-p default-directory 'localname) default-directory))
         (process-file-uninstall-args (if eglot-fsharp-server-install-dir
                                          (list "dotnet" nil `(nil ,stderr-file) nil "tool" "uninstall" "fsautocomplete" "--tool-path" local-tool-path)
                                        (list "dotnet" nil `(nil ,stderr-file) nil "tool" "uninstall" "-g" "fsautocomplete")))
         (process-file-install-args (if eglot-fsharp-server-install-dir
                                        (list "dotnet" nil `(nil ,stderr-file) nil "tool" "install" "fsautocomplete" "--tool-path" local-tool-path "--version" version)
                                      (list "dotnet" nil `(nil ,stderr-file) nil "tool" "install" "fsautocomplete" "-g" "--version" version))))
    (make-directory default-directory t)
    (condition-case err
        (progn
          (unless (or (eglot-fsharp-current-version-p version) (not (eglot-fsharp--installed-version)))
            (message "Uninstalling fsautocomplete version %s" (eglot-fsharp--installed-version))
            (unless (zerop (apply #'process-file process-file-uninstall-args))
              (error  "'dotnet tool uninstall fsautocomplete ... failed")))
          (unless (zerop (apply #'process-file process-file-install-args))
            (error "'dotnet tool install fsautocomplete --tool-path %s --version %s' failed" default-directory  version)))
      (error
       (let ((stderr (with-temp-buffer
                       (insert-file-contents stderr-file)
                       (buffer-string))))
         (delete-file stderr-file)
         (signal (car err) (format "%s: %s" (cdr err) stderr)))))
    (message "Installed fsautocomplete to %s" (eglot-fsharp--path-to-server))))

(defun eglot-fsharp--maybe-install (&optional version)
  "Downloads F# compiler service, and install in `eglot-fsharp-server-install-dir'."
  (unless eglot-fsharp-server-install-dir
    (make-directory (concat (file-remote-p default-directory)
                            (file-name-directory (eglot-fsharp--path-to-server))) t))
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
  eglot-fsharp-fsautocomplete-args)

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
