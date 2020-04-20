.PHONY: test clean

EMACS ?= emacs
LOADPATH = -L .

test:
	emacs -Q -batch $(LOADPATH) \
		-l test/test-quickrun2.el \
		-f ert-run-tests-batch-and-exit

clean:
	@rm -f *.elc
