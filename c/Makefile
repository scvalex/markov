all: build

.PHONY: clean build

build: markov.c
	gcc -Wall -O2 -o markov markov.c

clean:
	rm -f markov

p:
	permamake.sh *.c Makefile
