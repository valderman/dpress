DPRESS_HSFILES=DissociatedPress/Text/ByteString.hs DissociatedPress/Text/String.hs DissociatedPress/Core.hs DissociatedPress/NGram.hs DissociatedPress/Storage.hs DissociatedPress.hs
HSFLAGS=-O2 --make

all: compile test server merge tget

compile: compile.hs $(DPRESS_HSFILES)
	ghc $(HSFLAGS) compile.hs
	strip -s compile

test: test.hs $(DPRESS_HSFILES)
	ghc $(HSFLAGS) test.hs
	strip -s test

server: server.hs $(DPRESS_HSFILES)
	ghc $(HSFLAGS) server.hs
	strip -s server

tget: tget.hs $(DPRESS_HSFILES)
	ghc $(HSFLAGS) tget.hs
	strip -s tget

merge: merge.hs $(DPRESS_HSFILES)
	ghc $(HSFLAGS) merge.hs
	strip -s merge

clean:
	find . -name '*.hi' -exec rm \{\} \;
	find . -name '*.o' -exec rm \{\} \;
	find . -name '*.hi' -exec rm \{\} \;

distclean: clean
	find . -name '*.hs~' -exec rm \{\} \;
	rm tget compile merge server test
