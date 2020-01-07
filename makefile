.PHONY: ci local test
ci:
	cask upgrade-cask
	cask install
	cask exec buttercup -L test/github-review-test.el

test:
	cask exec buttercup -L test/github-review-test.el

# Run the tests locally
# Before running this:
# 1) Follow steps to install cask https://github.com/cask/cask
# 2) Call `cask` to install all the dependencies
local:
	TRAVIS=true ${HOME}/.cask/bin/cask exec buttercup -L test/github-review-test.el # TRAVIS=true to force coverage to be computed locally
