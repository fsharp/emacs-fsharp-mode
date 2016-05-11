(require 'ert)

(defmacro wait-for-condition (&rest body)
  "Wait up to 10 seconds for BODY to evaluate non-nil"
  `(with-timeout (10)
    (while (not (progn
		  ,@body))
      (sleep-for 1))))

(defun fsharp-mode-wrapper (bufs fun)
  "Load fsharp-mode and make sure any completion process is killed after test"
  (unwind-protect
      ; Run the actual test
      (funcall fun)

    ; Clean up below

    ; Close any buffer requested by the test
    (dolist (buf bufs)
      (when (get-buffer buf)
        (switch-to-buffer buf)
        (when (file-exists-p buffer-file-name)
          (revert-buffer t t))
        (kill-buffer buf)))

    ; Close any buffer associated with the loaded project
    (mapc (lambda (buf)
            (when (gethash (buffer-file-name buf) fsharp-ac--project-files)
              (switch-to-buffer buf)
              (revert-buffer t t)
              (kill-buffer buf)))
          (buffer-list))

    ; Stop the fsautocomplete process and close its buffer
    (fsharp-ac/stop-process)
    (wait-for-condition (not (fsharp-ac--process-live-p nil)))
    (when (fsharp-ac--process-live-p nil)
      (kill-process fsharp-ac-completion-process)
      (wait-for-condition (not (fsharp-ac--process-live-p nil))))
    (when (get-buffer "*fsharp-complete*")
      (kill-buffer "*fsharp-complete*"))

    ; Kill the FSI process and buffer, if it was used
    (let ((inf-fsharp-process (get-process inferior-fsharp-buffer-subname)))
      (when inf-fsharp-process
        (when (process-live-p inf-fsharp-process)
          (kill-process inf-fsharp-process)
          (wait-for-condition (not (process-live-p
				    inf-fsharp-process))))))))

(defun find-file-and-wait-for-project-load (file)
  (find-file file)
  (wait-for-condition (and (gethash (fsharp-ac--buffer-truename) fsharp-ac--project-files)
			   (/= 0 (hash-table-count fsharp-ac--project-data)))))

(ert-deftest check-project-files ()
  "Check the program files are set correctly"
  (fsharp-mode-wrapper '("Program.fs")
   (lambda ()
     (find-file-and-wait-for-project-load "test/Test1/Program.fs")
     (let ((project (gethash (fsharp-ac--buffer-truename) fsharp-ac--project-files))
           (projectfiles))
       (maphash (lambda (k _) (add-to-list 'projectfiles k)) fsharp-ac--project-files)
       (should-match "Test1/Program.fs" (s-join "" projectfiles))
       (should-match "Test1/FileTwo.fs" (s-join "" projectfiles))
       (should-match (regexp-quote (convert-standard-filename "Test1/bin/Debug/Test1.exe"))
                     (gethash "Output" (gethash project fsharp-ac--project-data)))))))

(ert-deftest check-completion ()
  "Check completion-at-point works"
  (fsharp-mode-wrapper '("Program.fs")
   (lambda ()
     (find-file-and-wait-for-project-load "test/Test1/Program.fs")
     (search-forward "X.func")
     (delete-char -3)
     (let ((company-async-timeout 5)) (company-complete))
     (beginning-of-line)
     (should (search-forward "X.func")))))

(ert-deftest check-completion-no-project ()
  "Check completion-at-point if File is not part of a Project."
  (fsharp-mode-wrapper '("NoProject.fs")
   (lambda ()
     (find-file-and-wait-for-project-load "test/Test1/NoProject.fs")
     (search-forward "open System.Collectio")
     (company-complete)
     (beginning-of-line)
     (should (re-search-forward "open System\\.Collection$" nil t)))))

(ert-deftest check-gotodefn ()
  "Check jump to (and back from) definition works"
  (fsharp-mode-wrapper '("Program.fs")
   (lambda ()
     (find-file-and-wait-for-project-load "test/Test1/Program.fs")
     (search-forward "X.func")
     (backward-char 2)
     (fsharp-ac-parse-current-buffer t)
     (fsharp-ac/gotodefn-at-point)
     (wait-for-condition (/= (point) 88))
     (should= (point) 18)
     (fsharp-ac/pop-gotodefn-stack)
     (should= (point) 88)
     ;; across files
     (goto-char (point-min))
     (search-forward "NewObjectType()")
     (backward-char 7)
     (fsharp-ac/gotodefn-at-point)
     (wait-for-condition
      (progn
	;; Command loop doesn't get executed so the buffer change
	;; in the filter function doesn't take effect. Prod it manually.
	(set-buffer (window-buffer (selected-window)))
	(and (/= (point) 64)
	     (equal (buffer-name) "FileTwo.fs"))))
     (should= (buffer-name) "FileTwo.fs")
     (should= (point) 97)
     (fsharp-ac/pop-gotodefn-stack)
     (should= (buffer-name) "Program.fs")
     (should= (point) 64))))

(ert-deftest check-tooltip ()
  "Check tooltip request works"
  (fsharp-mode-wrapper '("Program.fs")
   (lambda ()
     (let ((tiptext)
           (fsharp-ac-use-popup t))
       (noflet ((fsharp-ac/show-popup (s) (setq tiptext s)))
         (find-file-and-wait-for-project-load "test/Test1/Program.fs")
         (search-forward "X.func")
         (backward-char 2)
         (fsharp-ac-parse-current-buffer t)
         (fsharp-ac/show-tooltip-at-point)
         (wait-for-condition tiptext)
         (should-match "val func : x:int -> int\n\nFull name: Program.X.func"
                       tiptext))))))

(ert-deftest check-errors ()
  "Check error underlining works"
  (fsharp-mode-wrapper '("Program.fs")
   (lambda ()
     (find-file-and-wait-for-project-load "test/Test1/Program.fs")
     (search-forward "X.func")
     (delete-char -1)
     (backward-char)
     (fsharp-ac-parse-current-buffer t)
     (wait-for-condition (> (length (overlays-at (point))) 0))
     (should= (overlay-get (car (overlays-at (point))) 'face)
              'fsharp-error-face)
     (should= (overlay-get (car (overlays-at (point))) 'help-echo)
              "Unexpected keyword 'fun' in binding. Expected incomplete structured construct at or before this point or other token."))))

(ert-deftest check-script-tooltip ()
  "Check we can request a tooltip from a script"
  (fsharp-mode-wrapper '("Script.fsx")
   (lambda ()
     (let ((tiptext)
           (fsharp-ac-use-popup t))
       (noflet ((fsharp-ac/show-popup (s) (setq tiptext s)))
         (find-file-and-wait-for-project-load "test/Test1/Script.fsx")
         (fsharp-ac-parse-current-buffer t)
         (search-forward "XA.fun")
         (fsharp-ac/show-tooltip-at-point)
         (wait-for-condition tiptext)
         (should-match "val funky : x:int -> int\n\nFull name: Script.XA.funky"
                       tiptext))))))

(ert-deftest check-script-tooltip ()
  "Check the symbol highlighting works"
  (fsharp-mode-wrapper '("Script.fsx")
   (lambda ()
     (find-file-and-wait-for-project-load "test/Test1/Script.fsx")
     (search-forward "funky")
     (wait-for-condition (> (length (overlays-at (point))) 0))
     (let ((ovs (overlays-in (point-min) (point-max))))
       (mapc (lambda (ov) (should= (overlay-get ov 'face) 'fsharp-usage-face)) ovs)))))

(ert-deftest check-inf-fsharp ()
  "Check that FSI can be used to evaluate"
  (fsharp-mode-wrapper '("tmp.fsx")
   (lambda ()
     (fsharp-run-process-if-needed inferior-fsharp-program)
     (wait-for-condition (get-buffer inferior-fsharp-buffer-name))
     (find-file "tmp.fsx")
     (goto-char (point-max))
     (insert "let myvariable = 123 + 456")
     (fsharp-eval-phrase)
     (switch-to-buffer inferior-fsharp-buffer-name)
     (wait-for-condition (search-backward "579" nil t))
     (should-match "579" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest check-multi-project ()
  "Check that we can get intellisense for multiple projects at once"
  (fsharp-mode-wrapper '("FileTwo.fs" "Main.fs")
   (lambda ()
     (find-file-and-wait-for-project-load "test/Test1/FileTwo.fs")
     (find-file-and-wait-for-project-load "../Test2/Main.fs")

     (switch-to-buffer "FileTwo.fs")
     (search-forward "   y")
     (fsharp-ac-parse-current-buffer t)
     (fsharp-ac/gotodefn-at-point)
     (wait-for-condition (/= (point) 159))
     (should= (point) 137)

     (switch-to-buffer "Main.fs")
     (search-forward "\" val2")
     (fsharp-ac-parse-current-buffer t)
     (fsharp-ac/gotodefn-at-point)
     (wait-for-condition (/= (point) 113))
     (should= (point) 24))))
