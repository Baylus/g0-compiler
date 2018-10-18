#
# g0 Makefile
#
YACC=yacc
LEX=flex
CC=cc
CFLAGS= -Wall -g

all: g0

FILES=g0.o g0gram.o g0lex.o tree.o symt.o processTree.o scope.o

g0: ${FILES}
	cc -o g0 ${FILES}

g0gram.c g0gram.h: g0gram.y
	$(YACC) -dtv --verbose g0gram.y
	mv -f y.tab.c g0gram.c
	mv -f y.tab.h g0gram.h

g0lex.c: g0lex.l g0gram.h tree.h
	$(LEX) -t g0lex.l >g0lex.c

g0lex.o: g0gram.h

symt.o: symt.h

type.o: type.h

tree.o: tree.h token.h g0gram.h

scope.o: symt.h type.h token.h

g0.o: main.c
	$(CC) -c $(CFLAGS) main.c -o g0.o

.c.o:
	$(CC) -c $(CFLAGS) $<

test: g0
	perl check.pl Examples/

clean:
	rm -f g0 *.o
	rm -f g0lex.c g0gram.c g0gram.h
	# rm -fr hw2
	rm -fr testSubmit

submitfiles = main.c token.h g0lex.l g0gram.y symt.c symt.h tree.c tree.h Makefile type.h scope.c scope.h processTree.h processTree.c
submit: $(submitfiles)
	zip hw3.zip $(submitfiles)
	mkdir testSubmit
	cp hw3.zip testSubmit/
	cd testSubmit && unzip hw3.zip && make