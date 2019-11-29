$env:HOME=(Join-Path $PSScriptRoot '\tmp')

emacs --version

Invoke-WebRequest -Uri https://raw.githubusercontent.com/joaotavora/eglot/master/eglot-tests.el -Outfile (Join-Path (Split-Path $PSScriptRoot -parent) "eglot-tests.el")
dotnet restore (Join-Path (Split-Path $PSScriptRoot -parent) "test\Test1")

$location= Join-Path ($env:USERPROFILE) '\.cask\bin'
$env:Path += ";$location"
cask install | Out-String
cask update | Out-String

cask exec buttercup -L . -L (Join-Path (Split-Path $PSScriptRoot -parent) "test") | Out-String
