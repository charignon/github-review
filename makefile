all:
	TRAVIS=true ${HOME}/.cask/bin/cask exec buttercup -L test/test-helper.el # TRAVIS=true to force coverage to be computed locally
