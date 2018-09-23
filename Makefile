#
# g0 Makefile
#
YACC=yacc
LEX=flex
CC=cc
CFLAGS=-g -Wall

all: g0++

.c.o:
	$(CC) -c $(CFLAGS) $<

g0: g0.o g0gram.o g0lex.o tree.o
	cc -o g0 g0.o g0gram.o g0lex.o tree.o

g0gram.c g0gram.h: g0gram.y
	$(YACC) -dt --verbose g0gram.y
	mv -f y.tab.c g0gram.c
	mv -f y.tab.h g0gram.h

g0lex.c: g0lex.l
	$(LEX) -t g0lex.l >g0lex.c

g0lex.o: g0gram.h

symt.o: symt.h

type.o: type.h

tree.o: tree.h

clean:
	rm -f g0 *.o
	rm -f g0lex.c g0gram.c g0gram.h

submitfiles = main.c token.h tokenlist.h tokenlist.c g0.l Makefile
submit: $(submitfiles)
	mkdir hw2
	cp -T hw2/ $(submitfiles)
	gzip hw2/