
all: build build/parser

clean:
	rm -r build

build:
	mkdir build

build/parser.c: parser.y
	bison -o $@ $^

build/lexer.c: lexer.l
	flex -o $@ $^

build/parser: build/parser.c build/lexer.c
	gcc -o $@ $^
