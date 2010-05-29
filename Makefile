DPRESS_HSFILES=DissociatedPress/Text/ByteString.hs DissociatedPress/Text/String.hs DissociatedPress/Core.hs DissociatedPress/NGram.hs DissociatedPress/Storage.hs DissociatedPress.hs

all: compile test

compile: compile.hs $(DPRESS_HSFILES)
	ghc -O2 --make compile.hs

test: test.hs $(DPRESS_HSFILES)
	ghc -O2 --make test.hs

clean:
	find . -name '*.hi' -exec rm \{\} \;
	find . -name '*.o' -exec rm \{\} \;
	find . -name '*.hi' -exec rm \{\} \;

distclean: clean
	find . -name '*.hs~' -exec rm \{\} \;
