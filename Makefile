configure:
	@cabal configure --enable-tests

build: configure
	@cabal build

clean:
	@cabal clean

test:
	@cabal run eclair-haskell-test

.PHONY: configure build clean test
