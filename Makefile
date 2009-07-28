.PHONY: runcmakrov
runcmarkov: cmarkov
	./cmarkov morals.txt

.PHONY: runmarkov
runmarkov: markov
	./markov morals.txt +RTS -sstderr -K20000000

.PHONY: clean
clean:
	rm -f *.o *.hi *~

markov: markov.hs
	ghc -O2 -Wall --make markov.hs

cmarkov: cmarkov.c
	gcc -Wall -O2 cmarkov.c -o cmarkov
