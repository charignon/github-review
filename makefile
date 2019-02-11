all:
  # TRAVIS=true is to force computation of coverage locally
	TRAVIS=true ${HOME}/.cask/bin/cask exec buttercup -L test/test-helper.el
