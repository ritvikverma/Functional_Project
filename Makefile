kodable: clean
	-stack install ansi-terminal
	-cabal install --lib ansi-terminal
	-stack Kodable.hs
	-./Kodable

clean:
	-rm -rf Kodable
	-rm -rf Kodable.hi
	-rm -rf Kodable.o
	-rm -rf KodableData.hi
	-rm -rf KodableData.o
	-rm -rf Parser.hi
	-rm -rf Parser.o
	-rm -rf KodableUtils.hi
	-rm -rf KodableUtils.o