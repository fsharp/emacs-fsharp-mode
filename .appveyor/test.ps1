$env:HOME='$PSScriptRoot\tmp'

emacs --version

Invoke-Request -Uri https://raw.githubusercontent.com/joaotavora/eglot/master/eglot-tests.el
dotnet restore $PSScriptRoot\..\test\Test1

$location- Join-Path ($env:USERPROFILE) '\.cask\bin'
$env.Path += ";$location"
cask install | Out-String
cask update | Out-String
cask exec buttercup -L . -L $PSScriptRoot\..\test | Out-String
