all: build

.PHONY: all build clean

build:
	go build

clean:
	go clean

p:
	permamake.sh *.go Makefile
