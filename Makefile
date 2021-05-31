all: compile test

compile:
	@emacs -batch -L . -L themes/ -f batch-byte-compile *.el themes/*.el

test:
	@emacs -batch -L . -L themes/ -l test/test-helper.el test/*-test.el

clean:
	@rm -vf *.elc themes/*.elc *-autoloads.el *~

.PHONY: test
