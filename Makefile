all:
	@cask

test:
	@cask exec ert-runner

clean:
	@rm -rf .cask
	@rm -f *.elc test/*.elc

.PHONY: test
