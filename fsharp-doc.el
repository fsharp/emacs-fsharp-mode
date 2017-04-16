;;; fsharp-doc.el -- show information for F# symbol at point.
;;
;; Filename: fsharp-doc.el
;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Maintainer: Chris Barrett <chris.d.barrett@me.com>
;; Keywords: fsharp, languages
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; This is part of fsharp-mode for Emacs. It communicates with the F#
;; completion process to provide information for the symbol at point.
;;
;; This should be loaded automatically by fsharp-mode. Otherwise, add
;; this file to your load path, then call
;;
;;   (autoload 'turn-on-fsharp-doc-mode "fsharp-doc.el")
;;   (add-hook 'fsharp-mode-hook 'turn-on-fsharp-doc-mode)
;;
;;; Code:

(with-no-warnings (require 'cl))
(require 'fsharp-mode-completion)
(require 'flycheck)

(declare-function fsharp-mode "fsharp-mode.el")
(defvar fsharp-doc-idle-delay 0.5
  "The number of seconds to wait for input idle before showing a tooltip.")

(define-minor-mode fsharp-doc-mode
  "Display F# documentation in the minibuffer."
  nil
  ""
  nil
  ;; Body
  (fsharp-doc-reset-timer)
  (cond
   (fsharp-doc-mode
    (add-hook 'post-command-hook 'fsharp-doc-request-info-soon nil t)
    (run-hooks 'fsharp-doc-mode-hook))
   (t
    (remove-hook 'post-command-hook 'fsharp-doc-request-info-soon t)))
  fsharp-doc-mode)

(defun turn-on-fsharp-doc-mode ()
  (fsharp-doc-mode t))

(defun turn-off-fsharp-doc-mode ()
  (fsharp-doc-mode nil))

;;; -----------------------------------------------------------------------------

(defvar-local fsharp-doc-timer nil)

(defun fsharp-doc-request-info-soon ()
  (fsharp-doc-reset-timer)
  (when fsharp-doc-mode
    (setq fsharp-doc-timer
          (run-at-time fsharp-doc-idle-delay nil
                       'fsharp-doc--request-info))))

(defun fsharp-doc-reset-timer ()
  (when fsharp-doc-timer
    (cancel-timer fsharp-doc-timer)
    (setq fsharp-doc-timer nil)))

(defvar fsharp-doc-buffer-name "* fsharp-doc-buffer *")
(defun fsharp-get-fontification-buffer ()
  (let ((buffer (get-buffer fsharp-doc-buffer-name)))
    (if (buffer-live-p buffer)
        buffer
      (with-current-buffer (generate-new-buffer fsharp-doc-buffer-name)
        (ignore-errors
          (let ((fsharp-mode-hook nil))
            (fsharp-mode)))
        (current-buffer)))))

(defun fsharp-fontify-string (str)
  (with-current-buffer (fsharp-get-fontification-buffer)
    (delete-region (point-min) (point-max))
    (font-lock-fontify-region (point) (progn (insert str ";") (point)))
    (buffer-substring (point-min) (1- (point-max)))))

;;; ----------------------------------------------------------------------------

(defvar fsharp-doc-prevpoint nil)

(defun fsharp-doc--request-info ()
  "Send a request for tooltip and usage information unless at an error."
  (with-demoted-errors "F# doc display error: %s"
    (fsharp-doc-reset-timer)
    (let ((in-usage-overlay (fsharp-ac/usage-overlay-at (point))))
      (unless in-usage-overlay
        (fsharp-ac--clear-symbol-uses))
      (when (and fsharp-doc-mode
                 (thing-at-point 'symbol)
                 (not (eq (char-after) ? )))
        (unless (or (equal (point) fsharp-doc-prevpoint)
                    (not (eq fsharp-ac-status 'idle))
                    executing-kbd-macro
                    (flycheck-overlay-errors-at (point))
                    (active-minibuffer-window)
                    cursor-in-echo-area)
          (setq fsharp-doc-prevpoint (point))
          (fsharp-ac/show-typesig-at-point t)
          (unless in-usage-overlay
            (fsharp-ac/symboluse-at-point)))))))

(provide 'fsharp-doc)

;;; fsharp-doc.el ends here
