#
# g0 Makefile
#
YACC=yacc
LEX=flex
CC=cc
CFLAGS=-g -Wall

all: g0

FILES=g0.o g0gram.o g0lex.o tree.o symt.o

g0: ${FILES}
	cc -o g0 ${FILES}

g0gram.c g0gram.h: g0gram.y
	$(YACC) -dtv --verbose g0gram.y
	mv -f y.tab.c g0gram.c
	mv -f y.tab.h g0gram.h

g0lex.c: g0lex.l y.tab.h tree.h
	$(LEX) -t g0lex.l >g0lex.c

g0lex.o: g0gram.h

symt.o: symt.h

type.o: type.h

tree.o: tree.h token.h y.tab.h

y.tab.h: g0gram.y
	$(YACC) -d g0gram.y

g0.o: main.c
	$(CC) -c $(CFLAGS) main.c -o g0.o

.c.o:
	$(CC) -c $(CFLAGS) $<

test: g0
	perl check.pl

clean:
	rm -f g0 *.o
	rm -f g0lex.c g0gram.c g0gram.h

submitfiles = main.c token.h tokenlist.h tokenlist.c g0.l Makefile
submit: $(submitfiles)
	mkdir hw2
	cp -T hw2/ $(submitfiles)
	gzip hw2/