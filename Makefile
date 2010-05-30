DPRESS_HSFILES=DissociatedPress/Text/ByteString.hs DissociatedPress/Text/String.hs DissociatedPress/Core.hs DissociatedPress/NGram.hs DissociatedPress/Storage.hs DissociatedPress.hs

all: compile test server

compile: compile.hs $(DPRESS_HSFILES)
	ghc -O2 --make compile.hs
	strip -s compile

test: test.hs $(DPRESS_HSFILES)
	ghc -O2 --make test.hs
	strip -s compile

server: server.hs $(DPRESS_HSFILES)
	ghc -O2 --make server.hs
	strip -s compile

clean:
	find . -name '*.hi' -exec rm \{\} \;
	find . -name '*.o' -exec rm \{\} \;
	find . -name '*.hi' -exec rm \{\} \;

distclean: clean
	find . -name '*.hs~' -exec rm \{\} \;
