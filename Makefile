# -*- Makefile -*-

EMACS ?= emacs
EASK ?= eask

# TODO: add lint?
ci: build compile checkdoc test

build:
	$(EASK) package
	$(EASK) install

compile: 
	$(EASK) compile

clean:
	$(EASK) clean-all

test:
	$(EASK) install-deps --dev
	$(EASK) ert ./tests/*.el

checkdoc:
	$(EASK) checkdoc

# package-lint
lint:
	$(EASK) lint
