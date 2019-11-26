set HOME=%~dp0\tmp

REM emacs -l test\test-common.el --batch --eval "(progn (set-default-coding-systems 'utf-8) (load-packages))"

REM emacs -l test\test-common.el --batch --eval "(progn (set-default-coding-systems 'utf-8) (run-fsharp-unit-tests))"

