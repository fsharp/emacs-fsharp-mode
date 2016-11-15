;;; fsharp-mode-completion.el --- Autocompletion support for F#

;; Copyright (C) 2012-2013 Robin Neatherway

;; Author: Robin Neatherway <robin.neatherway@gmail.com>
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
(require 'tramp)
(require 's)
(require 'dash)
(require 'company)
(require 'json)
(require 'etags)
(require 'fsharp-mode-util)

(autoload 'pos-tip-fill-string "pos-tip")
(autoload 'pos-tip-show "pos-tip")
(autoload 'popup-tip "popup")

(declare-function flycheck-fsharp-handle-nothing-changed "flycheck-fsharp.el")
(declare-function fsharp-fontify-string "fsharp-doc.el" (str))
(declare-function fsharp-mode/find-fsproj "fsharp-mode.el" (dir-or-file))

;;; User-configurable variables

(defvar fsharp-ac-executable "fsautocomplete.exe")

(defvar fsharp-ac-complete-command
  (let ((exe (or (executable-find fsharp-ac-executable)
                 (concat (file-name-directory (or load-file-name buffer-file-name))
                         "bin/" fsharp-ac-executable))))
    (if fsharp-ac-using-mono
        (list "mono" exe)
      (list exe)))
  "Command to start the completion process.
If using Tramp this command must be also valid on remote the Host.")

(defvar fsharp-ac-use-popup t
  "Display tooltips using a popup at point.
If set to nil, display in a help buffer instead.")

(defvar fsharp-ac-intellisense-enabled t
  "Whether autocompletion is automatically triggered on '.'.")

(defface fsharp-usage-face
  '((t :inherit match))
  "Face used for marking a usage of a symbol in F#"
  :group 'fsharp)

;;; In seconds. Note that background process uses ms.
(defvar fsharp-ac-blocking-timeout 0.4)

;;; ----------------------------------------------------------------------------

(defvar fsharp-ac-debug nil)
(defvar fsharp-ac-status 'idle)
(defvar fsharp-ac-completion-process-alist nil)

(defun fsharp-ac-completion-process (host)
  "Return completion process on HOST."
  (cdr (assoc host fsharp-ac-completion-process-alist)))

(defun fsharp-ac-completion-process-add (host process)
  (push (cons host process) fsharp-ac-completion-process-alist))

(defun fsharp-ac-completion-process-del (host)
  (delq (assoc host fsharp-ac-completion-process-alist) fsharp-ac-completion-process-alist))

(defvar fsharp-ac--project-data (make-hash-table :test 'equal)
  "Data returned by fsautocomplete for loaded projects.")
(defvar fsharp-ac--project-files (make-hash-table :test 'equal)
  "Reverse mapping from files to the project that contains them.")
(defvar fsharp-ac-verbose nil)
(defvar fsharp-ac-current-candidate nil)
(defvar fsharp-ac-current-helptext (make-hash-table :test 'equal))
(defvar-local fsharp-ac-last-parsed-ticks 0
  "BUFFER's tick counter, when the file was parsed.")
(defvar fsharp-ac--last-parsed-buffer nil
  "Last parsed BUFFER, so that we reparse if we switch buffers.")

(defvar-local fsharp-company-callback nil)

(defconst fsharp-ac--log-buf "*fsharp-debug*")
(defconst fsharp-ac--completion-procname "fsharp-complete")

(defvar-local fsharp-ac-last-parsed-line -1
  "The line number that we last requested a parse for completions")

(defvar fsharp-ac-handle-errors-function nil
  "Function to call to handle errors messages from fsautocomplete.exe.")

(defvar fsharp-ac-handle-lint-function nil
  "Function to call to handle lint messages from fsautocomplete.exe.")

;; only needs to stash errors, not lint info
(defvar-local fsharp-ac-errors nil
  "The most recent flycheck errors for the buffer, if any.")

(defun fsharp-ac--log (str)
  (when fsharp-ac-debug
    (with-current-buffer (get-buffer-create fsharp-ac--log-buf)
      (let ((pt (point))
            (atend (eq (point-max) (point))))
        (goto-char (point-max))
        (insert-before-markers (format "%s: %s" (float-time) str))
        (unless atend
          (goto-char pt))))))

(defun log-psendstr (proc str)
  (fsharp-ac--log str)
  (process-send-string proc str))

(defun fsharp-ac-parse-current-buffer (&optional force-sync)
  "The optional FORCE-SYNC argument is for testing purposes.
When used, the buffer is parsed even if it has not changed
since the last request."
  (if (not (or (/= (buffer-chars-modified-tick) fsharp-ac-last-parsed-ticks)
               (not (eq fsharp-ac--last-parsed-buffer (current-buffer)))
               force-sync))
      ;; we were called by flycheck, but nothing has actually changed, so use
      ;; the old errors
      (flycheck-fsharp-handle-nothing-changed)
    (setq fsharp-ac--last-parsed-buffer (current-buffer))
    (save-restriction
      (let ((file (fsharp-ac--buffer-truename)))
        (widen)
        (fsharp-ac--log (format "Parsing \"%s\"\n" file))
        (process-send-string
         (fsharp-ac-completion-process (fsharp-ac--hostname file))
         (format "parse \"%s\" %s\n%s<<EOF>>\n"
                 (fsharp-ac--localname file)
                 (if force-sync " sync" "")
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (setq fsharp-ac-last-parsed-ticks (buffer-chars-modified-tick)))))

(defun fsharp-ac--isIdChar (c)
  (let ((gc (get-char-code-property c 'general-category)))
    (or
     (--any? (string= gc it) '("Lu" "Ll" "Lt" "Lm" "Lo" "Nl" "Nd" "Pc" "Mn" "Mc"))
     (eq c 39))))

(defun fsharp-ac--isNormalId (s)
  (--all? it (mapcar 'fsharp-ac--isIdChar s)))

(defun fsharp-ac--hostname (file)
  "Return host of a Tramp filename.

If FILENAME is not a Tramp filename return nil"
  (when (tramp-tramp-file-p file)
    (with-parsed-tramp-file-name file nil
      host)))

(defun fsharp-ac--localname (file)
  "Return localname of a Tramp filename.

If FILENAME is not a Tramp filename return FILENAME"
  (if (tramp-tramp-file-p file)
      (with-parsed-tramp-file-name file nil
	localname)
    file))

(defun fsharp-ac--tramp-file (file)
  "Return Tramp filename of FILE

When completion process is not started on a remote location return FILE.
This function should always be evaluated in the process-buffer!"
  (if (tramp-tramp-file-p default-directory)
      (with-parsed-tramp-file-name default-directory nil
	(tramp-make-tramp-file-name method user host file))
      file))

;;; ----------------------------------------------------------------------------
;;; File Parsing and loading

(defun fsharp-ac--buffer-truename (&optional buf)
  "Get the truename of BUF, or the current buffer by default.
For indirect buffers return the truename of the base buffer."
  (-some-> (buffer-file-name (or (buffer-base-buffer buf) buf))
	   (file-truename)))

(defun fsharp-ac/load-project (file)
  "Load the specified fsproj FILE as a project."
  (interactive
  ;; Prompt user for an fsproj, searching for a default.
   (let* ((proj (fsharp-mode/find-fsproj buffer-file-name))
          (relproj (when proj (file-relative-name proj (file-name-directory buffer-file-name))))
          (prompt (if relproj (format "Path to project (default %s): " relproj)
                    "Path to project: ")))
     (list (read-file-name prompt nil (fsharp-mode/find-fsproj buffer-file-name) t))))

  (when (fsharp-ac--valid-project-p file)
    (setq fsharp-ac-intellisense-enabled t)
    (unless (fsharp-ac--process-live-p (fsharp-ac--hostname file))
      (fsharp-ac/start-process))
    ;; Load given project.
    (when (fsharp-ac--process-live-p (fsharp-ac--hostname file))
      (log-psendstr (fsharp-ac-completion-process (fsharp-ac--hostname file))
		      (format "project \"%s\"%s\n"
			      (fsharp-ac--localname (file-truename file))
			      (if (and (numberp fsharp-ac-debug)
				       (>= fsharp-ac-debug 2))
				  " verbose"
				""))))
    file))

(defun fsharp-ac/load-file (file)
  "Start the compiler binding for an individual F# script FILE."
  (when (and (fsharp-ac--script-file-p file) (file-exists-p file))
    (unless (fsharp-ac--process-live-p (fsharp-ac--hostname file))
      (fsharp-ac/start-process))
    (add-hook 'after-save-hook 'fsharp-ac--load-after-save nil 'local)))

(defun fsharp-ac--load-after-save ()
  (remove-hook 'fsharp-ac--load-after-save 'local)
  (fsharp-ac/load-file (buffer-file-name)))

(defun fsharp-ac--valid-project-p (file)
  (and file
       (file-exists-p file)
       (string-match-p (rx "." "fsproj" eol) file)))

(defun fsharp-ac--fs-file-p (file)
  (and file
       (s-equals? "fs" (downcase (file-name-extension file)))))

(defun fsharp-ac--script-file-p (file)
  (and file
       (string-match-p (rx (or "fsx" "fsscript"))
                       (downcase (file-name-extension file)))))

(defun fsharp-ac--in-project-p (file)
  (gethash file fsharp-ac--project-files))

(defun fsharp-ac--reset ()
  (setq fsharp-ac-status 'idle
        fsharp-ac-current-candidate nil)
  (fsharp-ac-completion-process-del (fsharp-ac--hostname default-directory))
  (clrhash fsharp-ac-current-helptext)
  (let (files projects)
    (maphash (lambda (file project) (when (equal (fsharp-ac--hostname file) (fsharp-ac--hostname default-directory))
				      (push file files)
				      (push project projects)))
	     fsharp-ac--project-files)
    (--each projects (remhash it fsharp-ac--project-files))
    (--each files (remhash it fsharp-ac--project-files))))

;;; ----------------------------------------------------------------------------
;;; Display Requests
(defun fsharp-ac-send-pos-request (cmd file line col)
  (let ((linestr (replace-regexp-in-string "\"" "\\\""(buffer-substring-no-properties (line-beginning-position) (line-end-position)) t t)))
    (log-psendstr (fsharp-ac-completion-process (fsharp-ac--hostname file))
                  (format "%s \"%s\" \"%s\" %d %d %d %s\n" cmd (fsharp-ac--localname file) linestr line col
                          (* 1000 fsharp-ac-blocking-timeout)
                          (if (string= cmd "completion") "filter=StartsWith" "")))))

(defun fsharp-ac--process-live-p (host)
  "Check whether the background process is live for Tramp HOST.
If HOST is nil, check process on local system."
  (process-live-p (fsharp-ac-completion-process host)))

(defun fsharp-ac/stop-process ()
  (interactive)
  (let ((host (fsharp-ac--hostname default-directory)))
    (when (fsharp-ac--process-live-p host)
      (fsharp-ac-message-safely "Quitting fsharp completion process")
      (log-psendstr (fsharp-ac-completion-process host) "quit\n")
      (sleep-for 1)
      (when (fsharp-ac--process-live-p host)
        (kill-process (fsharp-ac-completion-process host))
        (kill-buffer  (process-buffer (fsharp-ac-completion-process host))))))
  (fsharp-ac--reset))

(defun fsharp-ac/start-process ()
  "Launch the F# completion process in the background."
  (interactive)
  (when fsharp-ac-intellisense-enabled
    (fsharp-ac/stop-process)
    (condition-case err
        (progn
          (fsharp-ac--reset)
          (fsharp-ac-completion-process-add (fsharp-ac--hostname default-directory) (fsharp-ac--configure-proc)))
      (error
       (setq fsharp-ac-intellisense-enabled nil)
       (message "Failed to start fsautocomplete (%s). Disabling intellisense. To reenable, set fsharp-ac-intellisense-enabled to t."
                (error-message-string err))))))

(defun fsharp-ac--process-sentinel (process event)
  "Default sentinel used by `fsharp-ac--configure-proc`."
  (when (memq (process-status process) '(exit signal))
    (--each (buffer-list) (with-current-buffer it
			    (when (eq major-mode 'fsharp-mode)
			      (setq fsharp-ac-last-parsed-ticks 0)
			      (fsharp-ac--clear-symbol-uses))))
    (fsharp-ac--reset)))

(defun fsharp-ac--configure-proc ()
  (let* ((fsac (if (tramp-tramp-file-p default-directory)
		   (with-parsed-tramp-file-name default-directory nil
		     (tramp-make-tramp-file-name
		      method user host (car (last fsharp-ac-complete-command))))
		 (car (last fsharp-ac-complete-command))))
	 (process-environment
	  (if (null fsharp-ac-using-mono)
	      process-environment
	    ;; workaround for Mono = 4.2.1 thread pool bug
	    ;; https://bugzilla.xamarin.com/show_bug.cgi?id=37288
	    (let ((x (getenv "MONO_THREADS_PER_CPU")))
	      (if (or (null x)
		      (< (string-to-number x) 8))
		  (cons "MONO_THREADS_PER_CPU=8" process-environment)
		process-environment))))
	 process-connection-type)
    (if (file-exists-p fsac)
	(let ((proc (apply 'start-file-process
			   fsharp-ac--completion-procname
			   (get-buffer-create (generate-new-buffer-name "*fsharp-complete*"))
			   fsharp-ac-complete-command)))
	  (sleep-for 0.1)
	  (if (process-live-p proc)
	      (progn
		(set-process-sentinel proc #'fsharp-ac--process-sentinel)
		(set-process-coding-system proc 'utf-8-auto)
		(set-process-filter proc 'fsharp-ac-filter-output)
		(set-process-query-on-exit-flag proc nil)
		(with-current-buffer (process-buffer proc)
		  (delete-region (point-min) (point-max)))
		proc)
	    (error "Failed to launch: '%s'" (s-join " " fsharp-ac-complete-command))
	    nil))
      (error "%s not found" fsac))))

(defun fsharp-ac-document (item)
  (let* ((ticks (s-match "^``\\(.*\\)``$" item))
         (key (if ticks (cadr ticks) item))
         (prop (gethash key fsharp-ac-current-helptext)))
    (let ((help
           (if prop prop
             (log-psendstr (fsharp-ac-completion-process (fsharp-ac--hostname default-directory))
                           (format "helptext %s\n" key))
             (with-local-quit
               (accept-process-output (fsharp-ac-completion-process (fsharp-ac--hostname default-directory)) 0 100))
             (gethash key fsharp-ac-current-helptext
                      "Loading documentation..."))))
             help)))

(defun fsharp-ac-make-completion-request ()
  (interactive)
  (setq fsharp-ac-status 'wait)
  (setq fsharp-ac-current-candidate nil)
  (clrhash fsharp-ac-current-helptext)
  (let ((line (line-number-at-pos)))
    (unless (eq fsharp-ac-last-parsed-line line)
      (setq fsharp-ac-last-parsed-line line)
      (fsharp-ac-parse-current-buffer))
    (fsharp-ac-send-pos-request
   "completion"
   (fsharp-ac--buffer-truename)
   (line-number-at-pos)
   (+ 1 (current-column)))))

(require 'cl-lib)

(defun fsharp-company-candidates (callback)
  (when (eq company-prefix "")
    ;; discard any pending requests as we
    ;; just pressed '.' or are at the start of a new line
    (setq fsharp-ac-status 'idle))

  (if (and (fsharp-ac-can-make-request t)
             (eq fsharp-ac-status 'idle))
      (progn
        (setq fsharp-company-callback callback)
        (fsharp-ac-make-completion-request))
    (funcall callback nil)))

(defun fsharp-ac-add-annotation-prop (s candidate)
  (propertize s 'annotation (gethash "GlyphChar" candidate)))

(defun fsharp-ac-completion-done ()
  (->> (--map (let ((s (gethash "Name" it)))
                (if (fsharp-ac--isNormalId s) (fsharp-ac-add-annotation-prop s it)
		  (s-append "``" (s-prepend "``" (fsharp-ac-add-annotation-prop s it)))))
              fsharp-ac-current-candidate)
       (funcall fsharp-company-callback)))

(defun completion-char-p (c)
  "True if the character before the point is a word char or ."
  (or (= c ?.)
      (= ?w (char-syntax c))))

(defun fsharp-ac-get-prefix ()
  ;; returning nil here causes company mode to not fetch completions
  (when (completion-char-p (char-before))
    (buffer-substring-no-properties (fsharp-ac--residue) (point))))

(defun fsharp-ac/company-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
        (interactive (company-begin-backend 'fsharp-ac/company-backend))
        (prefix  (when (not (company-in-string-or-comment))
		     ;; Don't pass to next backend if we are not inside a string or comment
		     (-if-let (prefix (fsharp-ac-get-prefix))
			 (cons prefix t)
		       'stop)))
        (ignore-case t)
        (sorted t)
        (candidates (cons :async 'fsharp-company-candidates))
        (annotation (get-text-property 0 'annotation arg))
        (doc-buffer (company-doc-buffer (fsharp-ac-document arg)))))

(defconst fsharp-ac--ident
  (rx (one-or-more (not (any ".` ,(\t\r\n"))))
  "Regexp for normal identifiers.")

; Note that this regexp is not 100% correct.
; Allowable characters are defined using unicode
; character classes, so this will match some very
; unusual strings composed of rare unicode chars.
(defconst fsharp-ac--rawIdent
  (rx (seq
       "``"
       (one-or-more
        (or
         (not (any "`\n\r\t"))
         (seq "`" (not (any "`\n\r\t")))))
       "``"))
  "Regexp for raw identifiers.")

(defconst fsharp-ac--rawIdResidue
  (rx (seq
       "``"
       (one-or-more
        (or
         (not (any "`\n\r\t"))
         (seq "`" (not (any "`\n\r\t")))))
       string-end))
  "Regexp for residues starting with backticks.")

(defconst fsharp-ac--dottedIdentNormalResidue
  (rx-to-string
   `(seq (zero-or-more
          (seq
           (or (regexp ,fsharp-ac--ident)
               (regexp ,fsharp-ac--rawIdent))
           "."))
         (group (zero-or-more (not (any ":.` ,(\t\r\n"))))
         string-end))
  "Regexp for a dotted ident with a standard residue.")

(defconst fsharp-ac--dottedIdentRawResidue
  (rx-to-string `(seq (zero-or-more
                       (seq
                        (or (regexp ,fsharp-ac--ident)
                            (regexp ,fsharp-ac--rawIdent))
                        "."))
                      (group (regexp ,fsharp-ac--rawIdResidue))))
  "Regexp for a dotted ident with a raw residue.")

(defun fsharp-ac--residue ()
  (let ((line (buffer-substring-no-properties (line-beginning-position) (point))))
           (- (point)
              (cadr
	       (-min-by 'car-less-than-car
			(--map (or (-map 'length (s-match it line)) '(0 0))
			       (list fsharp-ac--dottedIdentRawResidue
				     fsharp-ac--dottedIdentNormalResidue)))))))

(defun fsharp-ac-can-make-request (&optional quiet)
  "Test whether it is possible to make a request with the compiler binding.
The current buffer must be an F# file that exists on disk."
  (let ((file (fsharp-ac--buffer-truename)))
    (cond
     ((null file)
      (unless quiet
        (fsharp-ac-message-safely "Error: buffer not visiting a file."))
      nil)

     ((not (fsharp-ac--process-live-p (fsharp-ac--hostname file)))
      (unless quiet
        (fsharp-ac-message-safely "Error: background intellisense process not running."))
      nil)

     ((and (fsharp-ac--fs-file-p file)
           (not (fsharp-ac--in-project-p file)))

      (unless quiet
        (fsharp-ac-message-safely "Error: %s is not part of the loaded projects." file))
      nil)

     (t
      (and (not (syntax-ppss-context (syntax-ppss)))
           (eq fsharp-ac-status 'idle))))))

(defun fsharp-ac/show-tooltip-at-point ()
  "Display a tooltip for the F# symbol at POINT."
  (interactive)
  (when (fsharp-ac-can-make-request)
    (fsharp-ac-send-pos-request "tooltip"
                                (fsharp-ac--buffer-truename)
                                (line-number-at-pos)
                                (+ 1 (current-column)))))

(defun fsharp-ac/show-typesig-at-point (&optional quiet)
  "Display the type signature for the F# symbol at POINT. Pass
on QUIET to FSHARP-AC-CAN-MAKE-REQUEST. This is a bit of hack to
prevent usage errors being displayed by FSHARP-DOC-MODE."
  (interactive)
  (when (fsharp-ac-can-make-request quiet)
    (fsharp-ac-send-pos-request "typesig"
                                (fsharp-ac--buffer-truename)
                                (line-number-at-pos)
                                (+ 1 (current-column)))))

(defun fsharp-ac/symboluse-at-point ()
  "Find the uses in this file of the symbol at point."
  (interactive)
  (when (fsharp-ac-can-make-request)
    (fsharp-ac-send-pos-request "symboluse"
                                (fsharp-ac--buffer-truename)
                                (line-number-at-pos)
                                (+ 1 (current-column)))))

(defun fsharp-ac/gotodefn-at-point ()
  "Find the point of declaration of the symbol at point and goto it."
  (interactive)
  (when (fsharp-ac-can-make-request)
    (fsharp-ac-send-pos-request "finddecl"
                                (fsharp-ac--buffer-truename)
                                (line-number-at-pos)
                                (+ 1 (current-column)))))

(defun fsharp-ac/pop-gotodefn-stack ()
  "Go back to where point was before jumping to definition."
  (interactive)
  (pop-tag-mark))

(defun fsharp-ac/complete-at-point (&optional quiet)
  (interactive)
  (when (and (fsharp-ac-can-make-request quiet)
             (eq fsharp-ac-status 'idle))
    (fsharp-ac-parse-current-buffer)
    (company-complete)))

(defun fsharp-ac--parse-current-file ()
  (when (and (eq major-mode 'fsharp-mode) (fsharp-ac-can-make-request t))
    (fsharp-ac-parse-current-buffer)))

;;; ----------------------------------------------------------------------------
;;; Errors and Overlays

(defstruct fsharp-error start end face priority text file)
(defstruct fsharp-symbol-use start end face file)

(defun fsharp-ac-line-column-to-pos (line col)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line 1))
    (if (< (point-max) (+ (point) (- col 1)))
        (point-max)
      (forward-char (- col 1))
      (point))))

(defun fsharp-ac--parse-symbol-uses (data)
  "Extract the symbol uses from the given process response DATA."
  (save-match-data
    (--map
     (let ((beg (fsharp-ac-line-column-to-pos (gethash "StartLine" it)
					      (gethash "StartColumn" it)))
	   (end (fsharp-ac-line-column-to-pos (gethash "EndLine" it)
					      (gethash "EndColumn" it)))
	   (face 'fsharp-usage-face)
	   (file (fsharp-ac--tramp-file (gethash "FileName" it))))
       (make-fsharp-symbol-use :start beg
			       :end   end
			       :face  face
			       :file  file))
     data)))

(defun fsharp-ac/show-symbol-use-overlay (use)
  "Draw overlays in the current buffer to represent fsharp-symbol-use USE."
  (let* ((beg  (fsharp-symbol-use-start use))
         (end  (fsharp-symbol-use-end use))
         (face (fsharp-symbol-use-face use))
         (file (fsharp-symbol-use-file use)))
    (when (string= (fsharp-ac--buffer-truename) (file-truename file))
      (-> (make-overlay beg end nil t)
          (overlay-put 'face face)))))

(defun fsharp-ac--clear-symbol-uses ()
  (interactive)
  (remove-overlays nil nil 'face 'fsharp-usage-face))

;;; ----------------------------------------------------------------------------
;;; Error navigation
;;;
;;; These functions hook into Emacs' error navigation API and should not
;;; be called directly by users.

(defun fsharp-ac-message-safely (format-string &rest args)
  "Call MESSAGE with FORMAT-STRING and ARGS only if it is desirable to do so."
  (when (eq major-mode 'fsharp-mode)
    (unless (or (active-minibuffer-window) cursor-in-echo-area)
      (apply 'message format-string args))))

(defun fsharp-ac--has-faces-p (ov &rest faces)
  (let ((face (overlay-get ov 'face)))
    (--first (equal face it) faces)))

(defun fsharp-ac/usage-overlay-at (pos)
  (--first (fsharp-ac--has-faces-p it 'fsharp-usage-face)
          (overlays-at pos)))

;;; ----------------------------------------------------------------------------
;;; Process handling
;;;
;;; Handle output from the completion process.

(defun fsharp-ac--get-msg (proc)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (-when-let (eofloc (search-forward "\n" nil t))
      (when (numberp fsharp-ac-debug)
        (cond
         ((eq fsharp-ac-debug 1)
          (fsharp-ac--log (format "%s ...\n" (buffer-substring (point-min) (min 100 eofloc)))))

         ((>= fsharp-ac-debug 2)
          (fsharp-ac--log (format "%s\n" (buffer-substring (point-min) eofloc))))))

      (let ((json-array-type 'list)
            (json-object-type 'hash-table)
            (json-key-type 'string))
        (condition-case nil
            (prog2
                (goto-char (point-min))
                (json-read)
              (delete-region (point-min) (1+ (point))))
          (error
           (fsharp-ac--log (format "Malformed JSON: %s" (buffer-substring-no-properties (point-min) (point-max))))
           (message "Error: F# completion process produced malformed JSON (%s)."
                    (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun fsharp-ac-filter-output (proc str)
  "Filter STR from the completion process PROC and handle appropriately."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      ;; Remove BOM, if present
      (insert-before-markers (if (string-prefix-p "\ufeff" str)
                 (substring str 1)
                   str))))
  (let (msg)
    (while (setq msg (fsharp-ac--get-msg proc))
      (let ((kind (gethash "Kind" msg))
            (data (gethash "Data" msg)))
        (fsharp-ac--log (format "Received '%s' message of length %d\n"
                                kind
                                (hash-table-size msg)))
        (pcase kind
         ("error" (fsharp-ac-handle-process-error data))
         ("info" (when fsharp-ac-verbose (fsharp-ac-message-safely data)))
         ("completion" (fsharp-ac-handle-completion data))
         ("helptext" (fsharp-ac-handle-doctext data))
         ("lint" (funcall fsharp-ac-handle-lint-function data))
         ("errors" (funcall fsharp-ac-handle-errors-function data))
         ("project" (fsharp-ac-handle-project data))
         ("tooltip" (fsharp-ac-handle-tooltip data))
         ("typesig" (fsharp-ac--handle-typesig data))
         ("finddecl" (fsharp-ac-visit-definition data))
         ("symboluse" (fsharp-ac--handle-symboluse data))
	 (_ (fsharp-ac-message-safely "Error: unrecognised message kind: '%s'" kind)))))))

(defun fsharp-ac-handle-completion (data)
  (setq fsharp-ac-current-candidate data
        fsharp-ac-status 'idle)
  (fsharp-ac-completion-done))

(defun fsharp-ac-handle-doctext (data)
  (puthash (gethash "Name" data)
           (fsharp-ac--format-tooltip (gethash "Overloads" data))
           fsharp-ac-current-helptext))

(defun fsharp-ac-visit-definition (data)
  (let* ((file (fsharp-ac--tramp-file (gethash "File" data)))
         (line (gethash "Line" data))
         (col (gethash "Column" data)))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file file)
    (goto-char (fsharp-ac-line-column-to-pos line col))))

(defun fsharp-ac--format-tooltip-overload (overload)
  "Format a single overload"
  (let ((sig (gethash "Signature" overload))
        (cmt (gethash "Comment" overload)))
    (s-concat sig "\n" cmt (if (s-blank? cmt) "" "\n"))))

(defun fsharp-ac--format-tooltip-overloads (single? overloads)
  "Format a list of overloads"
  (let ((header (if (and single? (> (length overloads) 1)) "Multiple overloads\n" ""))
        (body (s-join "\n" (-map #'fsharp-ac--format-tooltip-overload (-take 10 overloads))))
        (footer (if (> (length overloads) 10) (format "(+%d other overloads)" (length overloads)) "")))
    (s-concat header body footer)))

(defun fsharp-ac--format-tooltip (items)
  "Format a list of items as a tooltip"
  (let ((result (s-join "\n--------------------\n"
                           (--map (fsharp-ac--format-tooltip-overloads (< (length items) 2) it) items))))
      (s-chomp result)))

(defun fsharp-ac--handle-symboluse (data)
  (when (eq major-mode 'fsharp-mode)
    (fsharp-ac--clear-symbol-uses)
    (let ((uses (fsharp-ac--parse-symbol-uses (gethash "Uses" data))))
      (when (> (length uses) 1)
        (mapc 'fsharp-ac/show-symbol-use-overlay uses)))))

(defun fsharp-ac-handle-tooltip (data)
  "Display information from the background process. If the user
has requested a popup tooltip, display a popup."
  ;; Do not display if the current buffer is not an fsharp buffer.
  (when (eq major-mode 'fsharp-mode)
    (unless (or (active-minibuffer-window) cursor-in-echo-area)
      (let ((data (fsharp-ac--format-tooltip data)))
        (progn
          (if fsharp-ac-use-popup
              (fsharp-ac/show-popup data)
            (fsharp-ac/show-info-window data)))))))

(defun fsharp-ac--handle-typesig (data)
  "Display in the minibuffer."
  (fsharp-ac-message-safely "%s" (fsharp-fontify-string data)))

(defun fsharp-ac/show-popup (str)
  (if (display-graphic-p)
      (pos-tip-show str nil nil nil 300)
    ;; Use unoptimized calculation for popup, making it less likely to
    ;; wrap lines.
    (let ((popup-use-optimized-column-computation nil))
      (popup-tip str))))

(defconst fsharp-ac-info-buffer-name "*fsharp info*")

(defun fsharp-ac/show-info-window (str)
  (save-excursion
    (let ((help-window-select t))
      (with-help-window fsharp-ac-info-buffer-name
        (princ str)))))

(defun fsharp-ac-handle-project (data)
  (let* ((project (fsharp-ac--tramp-file (gethash "Project" data)))
         (files (--map (file-truename (fsharp-ac--tramp-file it))
                       (gethash "Files" data)))
         (oldprojdata (gethash project fsharp-ac--project-data)))

    ;; Use the canonicalised filenames
    (puthash "Files" files data)

    ;; Remove any files previously associated with this
    ;; project as if reloading, they may have changed
    (when oldprojdata
      (--each (gethash "Files" oldprojdata)
        (remhash it fsharp-ac--project-files)))

    (puthash project data fsharp-ac--project-data)
    (--each files (puthash it project fsharp-ac--project-files))

    (when (not oldprojdata)
      (fsharp-ac-message-safely "Loaded F# project '%s'" (file-relative-name project)))

    (when (and (numberp fsharp-ac-debug)
               (>= fsharp-ac-debug 2))
      (maphash (lambda (file msg) (fsharp-ac-message-safely "%s:\n%s\n" file msg))
               (gethash "Logs" data)))))

(defun fsharp-ac-handle-process-error (str)
  (fsharp-ac-message-safely str)
  (when (not (eq fsharp-ac-status 'idle))
    (setq fsharp-ac-status 'idle
          fsharp-ac-current-candidate nil)))

(provide 'fsharp-mode-completion)

;;; fsharp-mode-completion.el ends here
