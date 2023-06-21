.PHONY: build build2 run

CABAL ?= /home/b/src/haskell/cabal/dist-newstyle/build/x86_64-linux/ghc-9.2.7/cabal-install-3.10.1.0/x/cabal/build/cabal/cabal
GHC ?= /home/b/Projects/usort/usort-web/ghc/_build/stage1/bin/javascript-unknown-ghcjs-ghc

build:
	$(CABAL) build -v -w $(GHC) --with-hc-pkg=$(GHC)-pkg --allow-newer

run:
	$(CABAL) run -w $(GHC) --with-hc-pkg=$(GHC)-pkg --allow-newer
