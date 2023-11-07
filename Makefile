build: configure
	@cabal build

configure:
	@cabal configure --enable-tests

clean:
	@cabal clean

test: build
	@cabal run eclair-haskell-test

.PHONY: configure build clean test
