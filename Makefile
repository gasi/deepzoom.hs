build:
	@ghc deepzoom.hs -XDeriveGeneric

install:
	cabal install --extra-lib-dirs=/usr/X11/lib/ \
		--extra-include-dirs=/usr/X11/include/ gd

run: build
	@./deepzoom test.jpg test.dzi

clean:
	@-rm -f *.hi
	@-rm -f *.o
	@-rm -f deepzoom
	@-rm -rf *_files
	@-rm -f *.dzi

.PHONY: build install run clean
