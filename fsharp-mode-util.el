;;; fsharp-mode-util.el --- utility functions

;; Copyright (C) 2015 Robin Neatherway

;; Author: 2015 Robin Neatherway <robin.neatherway@gmail.com>
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

(with-no-warnings (require 'cl))
(require 'dash)


(defvar fsharp-ac-runtime ()
  "The .NET runtime for FSAutoComplete.
Possible options are dotnet, mono, and dotnetcore"
    (case system-type
      ((windows-nt cygwin msdos) 'dotnet)
      (otherwise 'mono)))

(defun fsharp-mode--program-files-x86 ()
  (file-name-as-directory
   (car (-drop-while 'not
                     (list (getenv "ProgramFiles(x86)")
                           (getenv "ProgramFiles")
                           "C:\\Program Files (x86)")))))

(defun fsharp-mode--program-files-dotnetcore ()
  (let* ((currentSdk (string-trim (shell-command-to-string "dotnet --version")))
         ;; dotnet --list-sdks outputs a list of sdk versions, followed by the sdk path surrounded in square brackets
         (sdkPaths (-map #'fsharp-mode--parse-dotnet-list-sdk
                         (split-string (string-trim (shell-command-to-string "dotnet --list-sdks")) "\n")))
         (sdkInfo (-first (lambda (sdk) (equal currentSdk (car sdk))) sdkPaths))
         (sdkPath (concat (file-name-as-directory (substring (cdr sdkInfo) 1 -1)) (car sdkInfo))))
    (concat (file-name-as-directory sdkPath) "FSharp/")))

(defun fsharp-mode--parse-dotnet-list-sdk (sdk)
  (let* ((splitSdk (split-string sdk))
         (version (-first-item splitSdk))
         (path (-second-item splitSdk)))
    (cons version path)))
    ;; (-second-item (split-string sdkPath))))

(defun fsharp-mode--vs2017-msbuild-find (exe)
  "Return EXE absolute path for Visual Studio 2017, if existent, else nil."
  (->> (--map (concat (fsharp-mode--program-files-x86)
		      "Microsoft Visual Studio/2017/"
		      it
		      "msbuild/15.0/bin/"
		      exe)
	      '("Enterprise/" "Professional/" "Community/" "BuildTools/"))
       (--first (file-executable-p it))))

(defun fsharp-mode--msbuild-find (exe)
  "Find the build tool EXE based off fs-ac-runtime."
  (case fsharp-ac-runtime
    (dotnetcore (executable-find exe))
    (dotnet (let* ((searchdirs (--map (concat (fsharp-mode--program-files-x86)
                                      "MSBuild/" it "/Bin")
                              '("14.0" "13.0" "12.0")))
                   (exec-path (append searchdirs exec-path)))
              (or (fsharp-mode--vs2017-msbuild-find exe) (executable-find exe))))
    (mono (executable-find exe))))

(defun fsharp-mode--executable-find (exe)
  (case fsharp-ac-runtime
    (dotnetcore (let* ((exec-path (append (list (fsharp-mode--program-files-dotnetcore)) exec-path))
                       (dotnet-exec (executable-find "dotnet"))
                      (executable (locate-file exe exec-path)))
                  (concat dotnet-exec " " executable)))
    (dotnet (let* ((searchdirs (--map (concat (fsharp-mode--program-files-x86)
                                              "Microsoft SDKs/F#/" it "/Framework/v4.0")
                                      '("10.1" "4.0" "3.1" "3.0")))
                   (exec-path (append searchdirs exec-path)))
              (executable-find exe)))
    (mono (executable-find exe))))

(provide 'fsharp-mode-util)

;;; fsharp-mode-util.el ends here
