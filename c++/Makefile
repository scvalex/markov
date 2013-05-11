all: build

.PHONY: clean build

build: markov.cpp
	g++ -std=gnu++11 -Wall -O2 -o markov markov.cpp

clean:
	rm -f markov

p:
	permamake.sh *.cpp Makefile
