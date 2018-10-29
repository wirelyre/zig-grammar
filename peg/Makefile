
all: build build/parser

clean:
	rm -r build

build:
	mkdir build

build/parser.c: parser.y
	leg $^ > $@

build/parser: build/parser.c
	gcc -o $@ $^
