.PHONY: all clean

nixsh = nix-shell --pure --command


all: dist/setup-config Setup default.nix shell.nix
	$(nixsh) "./Setup build"

clean:
	rm -rf dist
	rm -f Setup default.nix


Setup: Setup.lhs default.nix shell.nix
	$(nixsh) "ghc -O -o $@ $<"
	rm $@.hi $@.o

default.nix: $(wildcard *.cabal)
	cabal2nix ./. > $@

dist/setup-config: $(wildcard *.cabal) Setup default.nix shell.nix
	$(nixsh) "./Setup configure -ftestprogram"
