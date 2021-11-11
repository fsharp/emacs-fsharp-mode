;;; inf-fsharp-mode.el --- Support for F# interactive

;; Copyright (C) 1997 INRIA

;; Author: 1993-1997 Xavier Leroy, Jacques Garrigue
;;         2010-2011 Laurent Le Brun <laurent@le-brun.eu>
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

(require 'tramp)
(require 'comint)
(require 'fsharp-mode-util)

(require 'cl-lib)

;; User modifiable variables

;; Whether you want the output buffer to be diplayed when you send a phrase

(defvar fsharp-display-when-eval t
  "*If true, display the inferior fsharp buffer when evaluating expressions.")

(defvar inferior-fsharp-program
  (if fsharp-ac-using-mono
      "fsharpi"
    (concat "\"" (fsharp-mode--executable-find "fsi.exe") "\" --fsi-server-input-codepage:65001"))
  "*Program name for invoking an inferior fsharp from Emacs.")

;; End of User modifiable variables


(defvar inferior-fsharp-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map [M-return] 'fsharp-comint-send)
    (define-key map (kbd "<tab>") 'inferior-fsharp-get-completion)
    map))

;; Augment fsharp mode, so you can process fsharp code in the source files.

(define-derived-mode inferior-fsharp-mode comint-mode "Inferior fsharp"
  "Major mode for interacting with an inferior fsharp process.
Runs a fsharp toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in fsharp mode.

\\{inferior-fsharp-mode-map}"
  (setq comint-prompt-regexp "^> ?")
  (setq comint-prompt-read-only t)

  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "(*")
  (set (make-local-variable 'comment-end) "*)")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "(\\*+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'comint-process-echoes) nil)
  (run-hooks 'inferior-fsharp-mode-hooks)

  ;; use compilation mode to parse errors, but RET and C-cC-c should still be from comint-mode
  (compilation-minor-mode)
  (make-local-variable 'minor-mode-map-alist)
  (setq minor-mode-map-alist (assq-delete-all 'compilation-minor-mode (copy-seq minor-mode-map-alist))))

(defconst inferior-fsharp-buffer-subname "inferior-fsharp")
(defconst inferior-fsharp-buffer-name
  (concat "*" inferior-fsharp-buffer-subname "*"))

(defun fsharp--localname (file)
  "Return localname of a Tramp filename.
If FILE is not a Tramp filename return FILENAME"
  (if (tramp-tramp-file-p file)
      (with-parsed-tramp-file-name file nil
	localname)
    file))

(defun fsharp-run-process-if-needed (&optional cmd)
  "Launch fsi if needed, using CMD if supplied."
  (unless (comint-check-proc inferior-fsharp-buffer-name)
    (setq inferior-fsharp-program
          (or cmd (read-from-minibuffer "fsharp toplevel to run: "
                                        inferior-fsharp-program)))
    (let ((cmdlist (inferior-fsharp-args-to-list inferior-fsharp-program))
          ;; fsi (correctly) disables any sort of console interaction if it
          ;; thinks we're a dumb terminal, and `comint-term-environment'
          ;; (correctly) defaults to setting TERM=dumb on systems using
          ;; terminfo, which is basically every modern system.
          ;;
          ;; we want to make use of fsi's tab completion, so tell comint
          ;; to set TERM=emacs for our inferior fsharp process.
          (comint-terminfo-terminal "emacs"))
      (with-current-buffer (apply (function make-comint)
                                  inferior-fsharp-buffer-subname
                                  (car cmdlist) nil
                                  (cdr cmdlist))
        (when (eq system-type 'windows-nt)
          (set-process-coding-system (get-buffer-process (current-buffer))
                                     'utf-8 'utf-8))
        (inferior-fsharp-mode))
      (display-buffer inferior-fsharp-buffer-name))))


;; the first value returned from our inferior f# process that appears to be a
;; completion. our filters can end up receiving multiple results that would match
;; any reasonable regexps, doing this prevents clobbering our match with
;; confusing-looking values.
;;
;; this will hopefully go away as we figure out how to get full completion results
;; from fsi without the sort of awkward automation we're doing here, but on the
;; chance that it doesn't we might consider making this buffer-local in the case
;; that people want to use multiple inferior fsharp buffers in the future.
(defvar inf-fsharp-completion-match nil)

;; the completion functions below are almost directly ripped from `comint', in
;; particular the `comint-redirect' functions. since `comint' pretty much
;; assumes we're line- based, and we cant easily (as far as i know) extend fsi
;; at runtime to let us retrieve full completion info the way that the
;; `python-shell-completion-native' functions do, we need to do some extra stuff
;; to send <tab> and handle deleting input that comint doesn't know has already
;; been sent to fsi

(defun inf-fsharp-redirect-filter (process input-string)
  (with-current-buffer (process-buffer process)
    (unless inf-fsharp-completion-match
      ;; this if-cascasde doesn't work if we convert it to a cond clause ??
      (if (and input-string
               (string-match comint-redirect-finished-regexp input-string))
          (setq inf-fsharp-completion-match input-string)

        ;; for some reason, we appear to get the results from fsi fontified
        ;; already in `comint-redirect-previous-input-string' without having
        ;; them pass through this function as `input-string' even though this
        ;; function (or comint-redirect-filter when we were using that directly)
        ;; is the only place we've been able to find that modifies the variable.
        ;;
        ;; looks like a race-condition or multithreading issue but not sure.
        ;; either way, we need to check here to make sure we don't miss our match
        (if (and comint-redirect-previous-input-string
                 (string-match comint-redirect-finished-regexp
                               (concat comint-redirect-previous-input-string input-string)))
            (setq inf-fsharp-completion-match
                  (concat comint-redirect-previous-input-string input-string)))))

    (setq comint-redirect-previous-input-string input-string)

    (if inf-fsharp-completion-match
        (let ((del-string (make-string (length inf-fsharp-completion-match) ?\b)))
          ;; fsi thinks we should have completed string that hasn't been sent in
          ;; the input buffer, but we will actually send later after inserting
          ;; the fsi-completed string into our repl buffer, so we need to delete
          ;; the match from fsi's input buffer to avoid sending nonsense strings.
          (process-send-string process del-string)

          ;; we need to make sure our deletion command goes through before we
          ;; exit this func and remove our current output-filter otherwise we'll
          ;; end up with the output from fsi confirming our backspaces in our
          ;; repl buffer, i.e, if we had "string" we'd see "strin stri str st s
          ;; ". we'd like to find a better way than sleeping to do this, but
          ;; there's not really a way for emacs to know that a process is done
          ;; sending it input as opposed to just not sending it yet....
          (sleep-for 1)

          (save-excursion
            (set-buffer comint-redirect-output-buffer)
            (erase-buffer)
            (goto-char (point-min))
            (insert (ansi-color-filter-apply inf-fsharp-completion-match)))

          (comint-redirect-cleanup)
          (run-hooks 'comint-redirect-hook)))))

(defun inf-fsharp-redirect-get-completion-from-process (input output-buffer process)
  (let* ((process-buffer (if (processp process)
                            (process-buffer process)
                           process))
         (proc (get-buffer-process process-buffer)))

    (with-current-buffer process-buffer
      (comint-redirect-setup
       output-buffer
       (current-buffer)
       (concat input "\\(.+\\)")
       nil)

      (set-process-filter proc #'inf-fsharp-redirect-filter)
      (process-send-string (current-buffer) (concat input "\t")))))

(defun inf-fsharp-get-completion-from-process (process to-complete)
  (let ((output-buffer " *inf-fsharp-completion*"))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (inf-fsharp-redirect-get-completion-from-process to-complete output-buffer process)

      (set-buffer (process-buffer process))
      (while (and (null comint-redirect-completed)
                  (accept-process-output process)))

      (set-buffer output-buffer)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun inferior-fsharp-get-completion ()
  (interactive)
  (let* ((inf-proc (get-process inferior-fsharp-buffer-subname))
        (orig-filter (process-filter inf-proc)))

    ;; reset our global completion match marker every time we start a completion
    ;; search so we don't accidentally use old complete data.
    (setq inf-fsharp-completion-match nil)

    (with-current-buffer (process-buffer inf-proc)
      (let* ((pos (marker-position (cdr comint-last-prompt)))
             (input (buffer-substring-no-properties pos (point))))

        ;; we get the whole of our input back from fsi in the response to our
        ;; <tab> completion request, so remove the initial repl input here and
        ;; replace it with that response.
        (delete-backward-char (length input))

        (insert (inf-fsharp-get-completion-from-process inf-proc input))))

    ;; we'd prefer to reset this filter closer in the file to where we replace
    ;; it, but we ran into some issues with setting it too early. try to fix
    ;; this up when we figure out a nicer way of doing this completion stuff
    ;; overall.
    (set-process-filter inf-proc orig-filter)))


;;;###autoload
(defun run-fsharp (&optional cmd)
  "Run an inferior fsharp process.
Input and output via buffer `*inferior-fsharp*'."
  (interactive
   (list (if (not (comint-check-proc inferior-fsharp-buffer-name))
             (read-from-minibuffer "fsharp toplevel to run: "
                                   inferior-fsharp-program))))
  (fsharp-run-process-if-needed cmd)
  (switch-to-buffer-other-window inferior-fsharp-buffer-name))

;; split the command line (e.g. "mono fsi" -> ("mono" "fsi"))
;; we double the \ before unquoting, so that the user doesn't have to
(defun inferior-fsharp-args-to-list (string)
  (split-string-and-unquote (replace-regexp-in-string "\\\\" "\\\\\\\\" string)))

(defun inferior-fsharp-show-subshell ()
  (interactive)
  (fsharp-run-process-if-needed)
  (display-buffer inferior-fsharp-buffer-name)

  (let ((buf (current-buffer))
        (fsharp-buf  (get-buffer inferior-fsharp-buffer-name))
        (count 0))
    (while
        (and (< count 10)
             (not (equal (buffer-name (current-buffer))
                         inferior-fsharp-buffer-name)))
      (next-multiframe-window)
      (setq count (+ count 1)))
    (if  (equal (buffer-name (current-buffer))
                inferior-fsharp-buffer-name)
        (goto-char (point-max)))
    (while
        (> count 0)
      (previous-multiframe-window)
      (setq count (- count 1)))))

(defun inferior-fsharp-eval-region (start end)
  "Send the current region to the inferior fsharp process."
  (interactive "r")
  (fsharp-run-process-if-needed)
  ;; send location to fsi
  (let* ((name (file-truename (buffer-file-name (current-buffer))))
         (dir (fsharp--localname (file-name-directory name)))
         (line (number-to-string (line-number-at-pos start)))
         (loc (concat "# " line " \"" name "\"\n"))
         (movedir (concat "#silentCd @\"" dir "\";;\n")))
    (comint-send-string inferior-fsharp-buffer-name movedir)
    (comint-send-string inferior-fsharp-buffer-name loc))
  (save-excursion
    (goto-char end)
    (comint-send-region inferior-fsharp-buffer-name start (point))
    ;; normally, ";;" are part of the region
    (if (and (>= (point) 2)
             (prog2 (backward-char 2) (looking-at ";;")))
        (comint-send-string inferior-fsharp-buffer-name "\n")
      (comint-send-string inferior-fsharp-buffer-name "\n;;\n"))
    ;; the user may not want to see the output buffer
    (if fsharp-display-when-eval
        (display-buffer inferior-fsharp-buffer-name t))))

(defvar fsharp-previous-output nil
  "tells the beginning of output in the shell-output buffer, so that the
output can be retreived later, asynchronously.")

;; To insert the last output from fsharp at point
(defun fsharp-insert-last-output ()
  "Insert the result of the evaluation of previous phrase"
  (interactive)
  (let ((pos (process-mark (get-buffer-process inferior-fsharp-buffer-name))))
    (insert-buffer-substring inferior-fsharp-buffer-name
                             fsharp-previous-output (- pos 2))))


(defun fsharp-simple-send (proc string)
  (comint-simple-send proc (concat string ";;")))

(defun fsharp-comint-send ()
  (interactive)
  (let ((comint-input-sender 'fsharp-simple-send))
    (comint-send-input)))

(provide 'inf-fsharp-mode)

;;; inf-sharp-mode.el ends here
