.PHONY: run
run: markov
	./markov alice.txt +RTS -sstderr -K20000000

markov: markov.hs
	rm -f *.o *.hi
	ghc -O2 -Wall --make markov.hs
