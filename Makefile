all: build

export EMACS ?= emacs

EMACSFLAGS = -L .

EASK = eask

VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')

PKG = fsharp-mode

ci: build compile test

build:
	$(EASK) package
	$(EASK) install --dev

compile:
	$(EASK) compile

test/eglot-tests.el:
	curl -o eglot-tests.el https://raw.githubusercontent.com/joaotavora/eglot/master/eglot-tests.el

test/Test1/restored:
	dotnet restore test/Test1
	touch test/Test1/restored

test: test/eglot-tests.el test/Test1/restored
	$(EASK) buttercup --allow-error --verbose 4

checkdoc:
	$(EASK) checkdoc

lint:
	$(EASK) lint

clean:
	rm -f .depend $(OBJECTS) $(PKG)-autoloads.el
	$(EASK) clean-elc

elpaclean: clean
	$(EASK) clean # Clean packages installed for development

run-$(PKG):
	cask exec $(EMACS) -Q -L . --eval "(require '$(PKG))"
