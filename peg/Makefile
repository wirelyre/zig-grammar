
all: build build/parser

clean:
	rm -r build

build:
	mkdir build

build/parser.c: parser.y
	peg $^ > $@

build/full.c: main.c build/parser.c
	cat $^ > $@

build/parser: build/full.c
	gcc -O3 -o $@ $^
