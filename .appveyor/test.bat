set HOME=%~dp0\tmp

emacs -l test\test-common.el --batch --eval "(progn (set-default-coding-systems 'utf-8) (load-packages))"

emacs -l test\test-common.el --batch --eval "(progn (set-default-coding-systems 'utf-8) (run-fsharp-unit-tests))"
