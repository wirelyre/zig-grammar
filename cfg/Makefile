
all: build build/parser build/parser.html

clean:
	rm -r build

build:
	mkdir build

build/parser.c build/parser.xml: parser.y
	bison -x -o $@ $^

build/lexer.c: lexer.l
	flex -o $@ $^

build/parser: build/parser.c build/lexer.c
	gcc -o $@ $^

build/parser.html: build/parser.xml
	xsltproc $(shell bison --print-datadir)/xslt/xml2xhtml.xsl $^ > $@
