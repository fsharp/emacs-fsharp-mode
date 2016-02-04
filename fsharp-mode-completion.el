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
(require 's)
(require 'dash)
(require 'company)
(require 'json)
(require 'etags)
(require 'fsharp-mode-util)

(autoload 'pos-tip-fill-string "pos-tip")
(autoload 'pos-tip-show "pos-tip")
(autoload 'popup-tip "popup")

(declare-function fsharp-doc/format-for-minibuffer "fsharp-doc.el" (str))
(declare-function fsharp-mode/find-fsproj "fsharp-mode.el" (dir-or-file))

;;; User-configurable variables

(defvar fsharp-ac-executable "fsautocomplete.exe")

(defvar fsharp-ac-complete-command
  (let ((exe (or (executable-find fsharp-ac-executable)
                 (concat (file-name-directory (or load-file-name buffer-file-name))
                         "bin/" fsharp-ac-executable))))
    (if fsharp-ac-using-mono
        (list "mono" exe)
      (list exe))))

(defvar fsharp-ac-use-popup t
  "Display tooltips using a popup at point.
If set to nil, display in a help buffer instead.")

(defvar fsharp-ac-intellisense-enabled t
  "Whether autocompletion is automatically triggered on '.'.")

(defface fsharp-error-face
  '((t :inherit error))
  "Face used for marking an error in F#"
  :group 'fsharp)

(defface fsharp-warning-face
  '((t :inherit warning))
  "Face used for marking a warning in F#"
  :group 'fsharp)

(defface fsharp-usage-face
  '((t :inherit match))
  "Face used for marking a usage of a symbol in F#"
  :group 'fsharp)

;;; Both in seconds. Note that background process uses ms.
(defvar fsharp-ac-blocking-timeout 0.4)
(defvar fsharp-ac-idle-timeout 1)

;;; ----------------------------------------------------------------------------

(defvar fsharp-ac-debug nil)
(defvar fsharp-ac-status 'idle)
(defvar fsharp-ac-completion-process nil)
(defvar fsharp-ac--project-data (make-hash-table :test 'equal)
  "Data returned by fsautocomplete for loaded projects.")
(defvar fsharp-ac--project-files (make-hash-table :test 'equal)
  "Reverse mapping from files to the project that contains them.")
(defvar fsharp-ac-idle-timer nil)
(defvar fsharp-ac-verbose nil)
(defvar fsharp-ac-current-candidate nil)
(defvar fsharp-ac-current-helptext (make-hash-table :test 'equal))
(defvar fsharp-ac-last-parsed-ticks 0
  "BUFFER's tick counter, when the file was parsed.")
(defvar fsharp-ac--last-parsed-buffer nil
  "Last parsed BUFFER, so that we reparse if we switch buffers.")

(defvar-local company-prefix nil)
(defvar-local fsharp-company-callback nil)

(defconst fsharp-ac--log-buf "*fsharp-debug*")
(defconst fsharp-ac--completion-procname "fsharp-complete")
(defconst fsharp-ac--completion-bufname
  (concat "*" fsharp-ac--completion-procname "*"))

(defun fsharp-ac--log (str)
  (when fsharp-ac-debug
    (unless (get-buffer fsharp-ac--log-buf)
      (generate-new-buffer fsharp-ac--log-buf))
    (with-current-buffer fsharp-ac--log-buf
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
  (when (or (/= (buffer-chars-modified-tick) fsharp-ac-last-parsed-ticks)
            (not (eq fsharp-ac--last-parsed-buffer (current-buffer)))
            force-sync)
    (setq fsharp-ac--last-parsed-buffer (current-buffer))
    (save-restriction
      (let ((file (fsharp-ac--buffer-truename)))
        (widen)
        (fsharp-ac--log (format "Parsing \"%s\"\n" file))
        (process-send-string
         fsharp-ac-completion-process
         (format "parse \"%s\" %s\n%s\n<<EOF>>\n"
                 file
                 (if force-sync " sync" "")
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (setq fsharp-ac-last-parsed-ticks (buffer-chars-modified-tick)))))

(defun fsharp-ac-parse-file (file)
  (with-current-buffer (find-file-noselect file)
    (fsharp-ac-parse-current-buffer)))


(defun fsharp-ac--isIdChar (c)
  (let ((gc (get-char-code-property c 'general-category)))
    (or
     (-any? (lambda (x) (string= gc x)) '("Lu" "Ll" "Lt" "Lm" "Lo" "Nl" "Nd" "Pc" "Mn" "Mc"))
     (eq c 39))))

(defun fsharp-ac--isNormalId (s)
  (-all? (lambda (x) x) (mapcar 'fsharp-ac--isIdChar s)))

;;; ----------------------------------------------------------------------------
;;; File Parsing and loading

(defun fsharp-ac--buffer-truename (&optional buf)
  "Get the truename of BUF, or the current buffer by default.
For indirect buffers return the truename of the base buffer."
  (if (buffer-base-buffer buf)
      (file-truename (buffer-file-name (buffer-base-buffer buf)))
    (file-truename (buffer-file-name buf))))

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
    (when (not (fsharp-ac--process-live-p))
      (fsharp-ac/start-process))
    ;; Load given project.
    (when (fsharp-ac--process-live-p)
      (log-psendstr fsharp-ac-completion-process
                    (format "project \"%s\"%s\n"
                            (file-truename file)
                            (if (and (numberp fsharp-ac-debug)
                                     (>= fsharp-ac-debug 2))
                                " verbose"
                              ""))))
    file))

(defun fsharp-ac/load-file (file)
  "Start the compiler binding for an individual F# script FILE."
  (when (fsharp-ac--script-file-p file)
    (if (file-exists-p file)
        (when (not (fsharp-ac--process-live-p))
          (fsharp-ac/start-process))
      (add-hook 'after-save-hook 'fsharp-ac--load-after-save nil 'local))))

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
  (when fsharp-ac-idle-timer
    (cancel-timer fsharp-ac-idle-timer))
  (setq fsharp-ac-status 'idle
        fsharp-ac-idle-timer nil
        fsharp-ac-completion-process nil
        fsharp-ac-current-candidate nil)
  (-each (list fsharp-ac-current-helptext
               fsharp-ac--project-data
               fsharp-ac--project-files) 'clrhash)
  (fsharp-ac-clear-errors))

;;; ----------------------------------------------------------------------------
;;; Display Requests

(defun fsharp-ac-send-pos-request (cmd file line col)
  (log-psendstr fsharp-ac-completion-process
                (format "%s \"%s\" %d %d %d\n" cmd file line col
                        (* 1000 fsharp-ac-blocking-timeout))))

(defun fsharp-ac--process-live-p ()
  "Check whether the background process is live."
  (and fsharp-ac-completion-process
       (process-live-p fsharp-ac-completion-process)))

(defun fsharp-ac/stop-process ()
  (interactive)
  (fsharp-ac-message-safely "Quitting fsharp completion process")
  (when (fsharp-ac--process-live-p)
    (log-psendstr fsharp-ac-completion-process "quit\n")
    (sleep-for 1)
    (when (fsharp-ac--process-live-p)
      (kill-process fsharp-ac-completion-process)))
  (fsharp-ac--reset))

(defun fsharp-ac/start-process ()
  "Launch the F# completion process in the background."
  (interactive)

  (when fsharp-ac-intellisense-enabled
    (when (fsharp-ac--process-live-p)
      (kill-process fsharp-ac-completion-process))

    (condition-case err
        (progn
          (fsharp-ac--reset)
          (setq fsharp-ac-completion-process (fsharp-ac--configure-proc))
          (fsharp-ac--reset-timer))
      (error
       (setq fsharp-ac-intellisense-enabled nil)
       (message "Failed to start fsautocomplete (%s). Disabling intellisense. To reenable, set fsharp-ac-intellisense-enabled to t."
                (error-message-string err))))))

(defun fsharp-ac--process-sentinel (process event)
  "Default sentinel used by `fsharp-ac--configure-proc`."
  (when (memq (process-status process) '(exit signal))
    (when fsharp-ac-idle-timer
      (cancel-timer fsharp-ac-idle-timer))
    (mapc (lambda (buf)
            (with-current-buffer buf
              (when (eq major-mode 'fsharp-mode)
                (setq fsharp-ac-last-parsed-ticks 0)
                (fsharp-ac-clear-errors)
                (fsharp-ac--clear-symbol-uses))))
          (buffer-list))
    (fsharp-ac--reset)))

(defun fsharp-ac--configure-proc ()
  (let ((fsac (car (last fsharp-ac-complete-command))))
    (unless (file-exists-p fsac)
      (error "%s not found" fsac)))
  (let* ((process-environment
          (if (null fsharp-ac-using-mono)
              process-environment
            ;; workaround for Mono >= 4.2.1 thread pool bug
            ;; https://bugzilla.xamarin.com/show_bug.cgi?id=37288
            (let ((x (getenv "MONO_THREADS_PER_CPU")))
              (if (or (null x)
                      (< (string-to-number x) 8))
                  (cons "MONO_THREADS_PER_CPU=8" process-environment)
                process-environment))))
         (proc (let (process-connection-type)
                 (apply 'start-process
                        fsharp-ac--completion-procname
                        fsharp-ac--completion-bufname
                        fsharp-ac-complete-command))))
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
      nil)))

(defun fsharp-ac--reset-timer ()
  (when fsharp-ac-idle-timer
    (cancel-timer fsharp-ac-idle-timer))
  (setq fsharp-ac-idle-timer
        (run-with-idle-timer fsharp-ac-idle-timeout
                             t
                             'fsharp-ac--parse-current-file)))

(defun fsharp-ac-document (item)
  (let* ((ticks (s-match "^``\\(.*\\)``$" item))
         (key (if ticks (cadr ticks) item))
         (prop (gethash key fsharp-ac-current-helptext)))
    (let ((help
           (if prop prop
             (log-psendstr fsharp-ac-completion-process
                           (format "helptext %s\n" key))
             (with-local-quit
               (accept-process-output fsharp-ac-completion-process 0 100))
             (gethash key fsharp-ac-current-helptext
                      "Loading documentation..."))))
             help)))

(defun fsharp-ac-make-completion-request ()
  (interactive)
  (setq fsharp-ac-status 'wait)
  (setq fsharp-ac-current-candidate nil)
  (clrhash fsharp-ac-current-helptext)
  (fsharp-ac-parse-current-buffer)
  (fsharp-ac-send-pos-request
   "completion"
   (fsharp-ac--buffer-truename)
   (line-number-at-pos)
   (+ 1 (current-column))))

(require 'cl-lib)

(defun fsharp-company-filter (prefix candidates)
  (if prefix
    (cl-loop for candidate in candidates
             when (string-prefix-p prefix candidate 't)
             collect candidate)))

(defun fsharp-company-candidates (callback)
  (when (eq company-prefix "")
    ;; discard any pending requests as we
    ;; just pressed '.' or are at the start of a new line
    (setq fsharp-ac-status 'idle))

  (when (and (fsharp-ac-can-make-request 't)
             (eq fsharp-ac-status 'idle))
    (setq fsharp-company-callback callback)
    (fsharp-ac-make-completion-request)))

(defun fsharp-ac-add-annotation-prop (s candidate)
  (propertize s 'annotation (gethash "GlyphChar" candidate)))

(defun fsharp-ac-completion-done ()
  (let ((mapped-completion
    (-map (lambda (candidate)
            (let ((s (gethash "Name" candidate)))
              (if (fsharp-ac--isNormalId s) (fsharp-ac-add-annotation-prop s candidate)
                (s-append "``" (s-prepend "``" (fsharp-ac-add-annotation-prop s candidate))))))
          fsharp-ac-current-candidate)))
    (funcall fsharp-company-callback (fsharp-company-filter company-prefix mapped-completion))))

(defun completion-char-p (c)
  "True if the character before the point is a word char or ."
  (or (= c ?.)
      (= ?w (char-syntax c))))

(defun fsharp-ac-get-prefix ()
  (if (completion-char-p (char-before))
      (buffer-substring-no-properties (fsharp-ac--residue) (point))
    ;; returning nil here causes company mode to not fetch completions
    nil))

(defun fsharp-ac/company-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
        (interactive (company-begin-backend 'fsharp-ac/company-backend))
        (prefix  (fsharp-ac-get-prefix))
        (ignore-case 't)
        (candidates (cons :async 'fsharp-company-candidates))
        (annotation (get-text-property 0 'annotation arg))
        (doc-buffer (company-doc-buffer (fsharp-ac-document arg)))))

(defconst fsharp-ac--ident
  (rx (one-or-more (not (any ".` \t\r\n"))))
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
         (group (zero-or-more (not (any ".` \t\r\n"))))
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
  (let ((result
         (let ((line (buffer-substring-no-properties (line-beginning-position) (point))))
           (- (point)
              (cadr
                (-min-by 'car-less-than-car
                 (-map (lambda (r) (let ((e (-map 'length (s-match r line))))
                                (if e e '(0 0))))
                       (list fsharp-ac--dottedIdentRawResidue
                             fsharp-ac--dottedIdentNormalResidue))))))))
    result))

(defun fsharp-ac-can-make-request (&optional quiet)
  "Test whether it is possible to make a request with the compiler binding.
The current buffer must be an F# file that exists on disk."
  (let ((file (fsharp-ac--buffer-truename)))
    (cond
     ((null file)
      (unless quiet
        (fsharp-ac-message-safely "Error: buffer not visiting a file."))
      nil)

     ((not (fsharp-ac--process-live-p))
      (unless quiet
        (fsharp-ac-message-safely "Error: background intellisense process not running."))
      nil)

     ((and (fsharp-ac--fs-file-p file)
           (not (fsharp-ac--in-project-p file)))

      (unless quiet
        (fsharp-ac-message-safely "Error: this file is not part of the loaded project."))
      nil)

     (t
      (and (not (syntax-ppss-context (syntax-ppss)))
           (eq fsharp-ac-status 'idle))))))

(defvar fsharp-ac-awaiting-tooltip nil)

(defun fsharp-ac/show-tooltip-at-point ()
  "Display a tooltip for the F# symbol at POINT."
  (interactive)
  (setq fsharp-ac-awaiting-tooltip t)
  (fsharp-ac/show-typesig-at-point))

(defun fsharp-ac/show-typesig-at-point (&optional quiet)
  "Display the type signature for the F# symbol at POINT. Pass
on QUIET to FSHARP-AC-CAN-MAKE-REQUEST. This is a bit of hack to
prevent usage errors being displayed by FSHARP-DOC-MODE."
  (interactive)
  (when (fsharp-ac-can-make-request quiet)
     (fsharp-ac-parse-current-buffer)
     (fsharp-ac-send-pos-request "tooltip"
                                 (fsharp-ac--buffer-truename)
                                 (line-number-at-pos)
                                 (+ 1 (current-column)))))

(defun fsharp-ac/symboluse-at-point ()
  "Find the uses in this file of the symbol at point."
  (interactive)
  (when (fsharp-ac-can-make-request)
    (fsharp-ac-parse-current-buffer)
    (fsharp-ac-send-pos-request "symboluse"
                                (fsharp-ac--buffer-truename)
                                (line-number-at-pos)
                                (+ 1 (current-column)))))

(defun fsharp-ac/gotodefn-at-point ()
  "Find the point of declaration of the symbol at point and goto it."
  (interactive)
  (when (fsharp-ac-can-make-request)
    (fsharp-ac-parse-current-buffer)
    (fsharp-ac-send-pos-request "finddecl"
                                (fsharp-ac--buffer-truename)
                                (line-number-at-pos)
                                (+ 1 (current-column)))))

(defun fsharp-ac/pop-gotodefn-stack ()
  "Go back to where point was before jumping to definition."
  (interactive)
  (pop-tag-mark))

(defun fsharp-ac/electric-backspace ()
  (interactive)
  (when (eq (char-before) ?.)
    (ac-stop))
  (delete-char -1))

(defun fsharp-ac/complete-at-point (&optional quiet)
  (interactive)
  (when (and (fsharp-ac-can-make-request quiet)
             (eq fsharp-ac-status 'idle))
    (company-complete)))

(defun fsharp-ac--parse-current-file ()
  (when (and (eq major-mode 'fsharp-mode)
             (fsharp-ac-can-make-request t))
    (fsharp-ac-parse-current-buffer)))

;;; ----------------------------------------------------------------------------
;;; Errors and Overlays

(defstruct fsharp-error start end face priority text file)
(defstruct fsharp-symbol-use start end face file)

(defvar fsharp-ac-errors)

(defun fsharp-ac-line-column-to-pos (line col)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line 1))
    (if (< (point-max) (+ (point) (- col 1)))
        (point-max)
      (forward-char (- col 1))
      (point))))

(defun fsharp-ac-parse-errors (data)
  "Extract the errors from the given process response DATA. Return a list of fsharp-error."
  (save-match-data
    (let (parsed)
      (dolist (err data parsed)
        (let ((beg (fsharp-ac-line-column-to-pos (gethash "StartLine" err)
                                                 (gethash "StartColumn" err)))
              (end (fsharp-ac-line-column-to-pos (gethash "EndLine" err)
                                                 (gethash "EndColumn" err)))
              (face (if (string= "Error" (gethash "Severity" err))
                        'fsharp-error-face
                      'fsharp-warning-face))
              (priority (if (string= "Error" (gethash "Severity" err))
                            1
                          0))
              (msg (gethash "Message" err))
              (file (gethash "FileName" err)))
          (add-to-list 'parsed (make-fsharp-error :start beg
                                                  :end   end
                                                  :face  face
                                                  :priority priority
                                                  :text  msg
                                                  :file  file)))))))


(defun fsharp-ac--parse-symbol-uses (data)
  "Extract the symbol uses from the given process response DATA."
  (save-match-data
    (let (parsed)
      (dolist (use data parsed)
        (let ((beg (fsharp-ac-line-column-to-pos (gethash "StartLine" use)
                                                 (gethash "StartColumn" use)))
              (end (fsharp-ac-line-column-to-pos (gethash "EndLine" use)
                                                 (gethash "EndColumn" use)))
              (face 'fsharp-usage-face)
              (file (gethash "FileName" use)))
          (add-to-list 'parsed (make-fsharp-symbol-use :start beg
                                                       :end   end
                                                       :face  face
                                                       :file  file)))))))

(defun fsharp-ac/show-error-overlay (err)
  "Draw overlays in the current buffer to represent fsharp-error ERR."
  (let* ((beg  (fsharp-error-start err))
         (end  (fsharp-error-end err))
         (face (fsharp-error-face err))
         (priority (fsharp-error-priority err))
         (txt  (fsharp-error-text err))
         (file (fsharp-error-file err)))
    (unless (or (not (string= (fsharp-ac--buffer-truename)
                              (file-truename file)))
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'face face)
        (overlay-put ov 'help-echo txt)
        (overlay-put ov 'priority priority))))))

(defun fsharp-ac/show-symbol-use-overlay (use)
  "Draw overlays in the current buffer to represent fsharp-symbol-use USE."
  (let* ((beg  (fsharp-symbol-use-start use))
         (end  (fsharp-symbol-use-end use))
         (face (fsharp-symbol-use-face use))
         (file (fsharp-symbol-use-file use)))
    (unless (or (not (string= (fsharp-ac--buffer-truename)
                              (file-truename file)))
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'face face))))))

(defun fsharp-ac-clear-errors ()
  (interactive)
  (remove-overlays nil nil 'face 'fsharp-error-face)
  (remove-overlays nil nil 'face 'fsharp-warning-face)
  (setq fsharp-ac-errors nil))

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

(defun fsharp-ac-error-position (n-steps errs)
  "Calculate the position of the next error to move to."
  (let* ((xs (->> (sort (-map 'fsharp-error-start errs) '<)
               (--remove (= (point) it))
               (--split-with (>= (point) it))))
         (before (nreverse (car xs)))
         (after  (cadr xs))
         (errs   (if (< n-steps 0) before after))
         (step   (- (abs n-steps) 1))
         )
    (nth step errs)))

(defun fsharp-ac/next-error (n-steps reset)
  "Move forward N-STEPS number of errors, possibly wrapping
around to the start of the buffer."
  (when reset
    (goto-char (point-min)))

  (let ((pos (fsharp-ac-error-position n-steps fsharp-ac-errors)))
    (if pos
        (goto-char pos)
      (error "No more F# errors"))))

(defun fsharp-ac--has-faces-p (ov &rest faces)
  (let ((face (overlay-get ov 'face)))
    (--first (equal face it) faces)))

(defun fsharp-ac/error-overlay-at (pos)
  (-first (lambda (ov) (fsharp-ac--has-faces-p ov 'fsharp-error-face 'fsharp-warning-face))
          (overlays-at pos)))

(defun fsharp-ac/usage-overlay-at (pos)
  (-first (lambda (ov) (fsharp-ac--has-faces-p ov 'fsharp-usage-face))
          (overlays-at pos)))

;;; HACK: show-error-at point checks last position of point to prevent
;;; corner-case interaction issues, e.g. when running `describe-key`
(defvar fsharp-ac-last-point nil)

(defun fsharp-ac/show-error-at-point ()
  (let ((ov (fsharp-ac/error-overlay-at (point)))
        (changed-pos (not (equal (point) fsharp-ac-last-point))))
    (setq fsharp-ac-last-point (point))

    (when (and ov changed-pos)
      (fsharp-ac-message-safely (overlay-get ov 'help-echo)))))

;;; ----------------------------------------------------------------------------
;;; Process handling
;;;
;;; Handle output from the completion process.

(defun fsharp-ac--get-msg (proc)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (let ((eofloc (search-forward "\n" nil t)))
      (when eofloc
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
              (progn
                (goto-char (point-min))
                (let ((msg (json-read)))
                  (delete-region (point-min) (+ (point) 1))
                  msg))
            (error
             (fsharp-ac--log (format "Malformed JSON: %s" (buffer-substring-no-properties (point-min) (point-max))))
             (message "Error: F# completion process produced malformed JSON (%s)."
                      (buffer-substring-no-properties (point-min) (point-max))))))))))

(defun fsharp-ac-filter-output (proc str)
  "Filter STR from the completion process PROC and handle appropriately."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      ;; Remove BOM, if present
      (insert-before-markers (if (string-prefix-p "\ufeff" str)
                 (substring str 1)
                   str))))
  (let ((msg (fsharp-ac--get-msg proc)))
    (while msg
      (let ((kind (gethash "Kind" msg))
            (data (gethash "Data" msg)))
        (fsharp-ac--log (format "Received '%s' message of length %d\n"
                                kind
                                (hash-table-size msg)))
        (cond
         ((s-equals? "error" kind) (fsharp-ac-handle-process-error data))
         ((s-equals? "info" kind) (when fsharp-ac-verbose (fsharp-ac-message-safely data)))
         ((s-equals? "completion" kind) (fsharp-ac-handle-completion data))
         ((s-equals? "helptext" kind) (fsharp-ac-handle-doctext data))
         ((s-equals? "errors" kind) (fsharp-ac-handle-errors data))
         ((s-equals? "project" kind) (fsharp-ac-handle-project data))
         ((s-equals? "tooltip" kind) (fsharp-ac-handle-tooltip data))
         ((s-equals? "finddecl" kind) (fsharp-ac-visit-definition data))
         ((s-equals? "symboluse" kind) (fsharp-ac--handle-symboluse data))
       (t
        (fsharp-ac-message-safely "Error: unrecognised message kind: '%s'" kind))))

    (setq msg (fsharp-ac--get-msg proc)))))

(defun fsharp-ac-handle-completion (data)
  (setq fsharp-ac-current-candidate data
        fsharp-ac-status 'idle)
  (fsharp-ac-completion-done))

(defun fsharp-ac-handle-doctext (data)
  (puthash (gethash "Name" data)
           (fsharp-ac--format-tooltip (gethash "Overloads" data))
           fsharp-ac-current-helptext))

(defun fsharp-ac-visit-definition (data)
  (let* ((file (gethash "File" data))
         (line (gethash "Line" data))
         (col (gethash "Column" data)))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file file)
    (goto-char (fsharp-ac-line-column-to-pos line col))))

(defun fsharp-ac-handle-errors (data)
  "Display error overlays and set buffer-local error variables for error navigation."
  (when (eq major-mode 'fsharp-mode)
    (unless (or (active-minibuffer-window) cursor-in-echo-area)
      (fsharp-ac-clear-errors)
      (let ((errs (fsharp-ac-parse-errors data)))
        (setq fsharp-ac-errors errs)
        (mapc 'fsharp-ac/show-error-overlay errs)))))

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
                           (-map (lambda (i) (fsharp-ac--format-tooltip-overloads (< (length items) 2) i)) items))))
      (s-chomp result)))

(defun fsharp-ac--handle-symboluse (data)
  (when (eq major-mode 'fsharp-mode)
    (fsharp-ac--clear-symbol-uses)
    (let ((uses (fsharp-ac--parse-symbol-uses (gethash "Uses" data))))
      (when (> (length uses) 1)
        (mapc 'fsharp-ac/show-symbol-use-overlay uses)))))

(defun fsharp-ac-handle-tooltip (data)
  "Display information from the background process. If the user
has requested a popup tooltip, display a popup. Otherwise,
display a short summary in the minibuffer."
  ;; Do not display if the current buffer is not an fsharp buffer.
  (when (eq major-mode 'fsharp-mode)
    (unless (or (active-minibuffer-window) cursor-in-echo-area)
      (let ((data (fsharp-ac--format-tooltip data)))
        (if fsharp-ac-awaiting-tooltip
            (progn
              (setq fsharp-ac-awaiting-tooltip nil)
              (if fsharp-ac-use-popup
                  (fsharp-ac/show-popup data)
                (fsharp-ac/show-info-window data)))
          (fsharp-ac-message-safely "%s" (fsharp-doc/format-for-minibuffer data)))))))

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
  (let* ((project (gethash "Project" data))
         (files (-map 'file-truename (gethash "Files" data)))
         (oldprojdata (gethash project fsharp-ac--project-data)))

    ;; Use the canonicalised filenames
    (puthash "Files" files data)

    ;; Remove any files previously associated with this
    ;; project as if reloading, they may have changed
    (when oldprojdata
      (-each (gethash "Files" oldprojdata)
        (lambda (f) (remhash f fsharp-ac--project-files))))

    (puthash project data fsharp-ac--project-data)
    (-map (lambda (f) (puthash f project fsharp-ac--project-files)) files)

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
