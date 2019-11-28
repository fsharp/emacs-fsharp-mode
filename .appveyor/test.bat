set HOME=%~dp0\tmp

REM emacs -l test\test-common.el --batch --eval "(progn (set-default-coding-systems 'utf-8) (load-packages))"

REM emacs -l test\test-common.el --batch --eval "(progn (set-default-coding-systems 'utf-8) (run-fsharp-unit-tests))"

emacs --version build %~dp0\..\test\eglot-tests.el %~dp0\..\test\Test1\project.assets.json
%userprofile%\.cask\bin\cask install
%userprofile%\.cask\bin\cask exec buttercup -L . -L %~dp0\..\test
