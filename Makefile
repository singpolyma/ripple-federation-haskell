GHCFLAGS=-Wall -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHSripple-federation-$(VERSION).a dist/ripple-federation-$(VERSION).tar.gz

install: dist/build/libHSripple-federation-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Ripple/Federation.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/ripple-federation/index.html README

README: ripple-federation.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/ripple-federation/index.html: dist/setup-config Ripple/Federation.hs
	cabal haddock --hyperlink-source

dist/setup-config: ripple-federation.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHSripple-federation-$(VERSION).a: dist/setup-config Ripple/Federation.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/ripple-federation-$(VERSION).tar.gz: README dist/setup-config Ripple/Federation.hs
	cabal check
	cabal sdist
