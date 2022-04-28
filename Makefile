# -*- Makefile -*-

EMACS ?= emacs
EASK ?= eask

ci: build compile checkdoc lint test

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
