HS_FILES := $(wildcard *.hs)
all: Reactive

Reactive: Reactive.hs
	ghc -O2 Reactive.hs

Test: Test.hs
	ghc -O2 Test.hs

clean:
	rm -f *.o *.hi Reactive Test
