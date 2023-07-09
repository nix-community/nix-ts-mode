.PHONY: test

EMACS ?= emacs

lisp:
	EMACS=$(EMACS) cask install \
	EMACS=$(EMACS) cask build

test: lisp
	EMACS=$(EMACS) cask exec ert-runner --reporter ert