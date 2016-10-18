.PHONY: all clean dist docs

nixsh = nix-shell --pure --command


all: dist/setup-config Setup default.nix shell.nix
	$(nixsh) "./Setup build"

clean:
	rm -rf dist
	rm -f Setup Setup.hi Setup.o shell.nix

dist: dist/setup-config Setup default.nix shell.nix
	$(nixsh) "./Setup sdist"

docs: hpass   = $(shell grep -Pio "(?<=^password:).*" ~/.cabal/config | tr -d " ")
docs: huser   = $(shell grep -Pio "(?<=^username:).*" ~/.cabal/config | tr -d " ")
docs: pkgname = $(shell grep -Pio "(?<=^name:).*" *.cabal | tr -d " ")
docs: pkgver  = $(shell grep -Pio "(?<=^version:).*" *.cabal | tr -d " ")
docs: tgz     = $(pkgname)-$(pkgver)-docs
docs: dist/setup-config Setup default.nix shell.nix
	$(nixsh) "\
	 	./Setup build && \
	 	./Setup haddock \
	 		--contents-location='/package/$$pkg-$$version' \
			--hoogle \
			--html \
	 		--html-location='/package/\$$pkg-\$$version/docs' \
	 		--hyperlink-source"
	rm -rf dist/doc/$(tgz){,.tar.gz}
	cp -av dist/doc/html/$(pkgname) dist/doc/$(tgz)
	tar -C dist/doc -cvz --format=ustar -f dist/doc/$(tgz).tar.gz $(tgz)
	@echo Uploading.
	@curl \
		--data-binary "@dist/doc/$(tgz).tar.gz" \
		--fail \
		--header "content-type: application/x-tar" \
		--header "content-encoding: gzip" \
		--request PUT \
		--user "$(huser):$(hpass)" \
		"https://hackage.haskell.org/package/$(pkgname)-$(pkgver)/docs"


Setup: Setup.lhs shell.nix
	$(nixsh) "ghc -O -o $@ $<"
	@touch Setup

default.nix: $(wildcard *.cabal)
	cabal2nix ./. > $@

shell.nix: $(wildcard *.cabal)
	cabal2nix -fexamples --shell ./. > $@

dist/setup-config: $(wildcard *.cabal) Setup shell.nix
	$(nixsh) "./Setup configure -fexamples"
