all:
	@cask

test:
	@cask exec ert-runner -l test/test-helper.el

clean:
	@rm -rf .cask
	@rm -f *.elc test/*.elc

.PHONY: test
