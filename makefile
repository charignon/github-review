.PHONY: ci local
ci:
	cask upgrade-cask
	cask install
	cask exec buttercup -L test/github-review-test.el

local:
	TRAVIS=true ${HOME}/.cask/bin/cask exec buttercup -L test/github-review-test.el # TRAVIS=true to force coverage to be computed locally
