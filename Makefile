cc = gcc
ccopts = -ly
lex = lex
lexopts =
lexgens = lex.yy.c
yacc = yacc
yaccopts = -d
yaccgens = y.tab.c y.tab.h
prj = g0

g0: main.o tokenlist.o lex.yy.o
	gcc -o g0 main.o tokenlist.o lex.yy.o
	
main.o: token.h tokenlist.h ytab.h
	gcc -c -g main.c
	
tokenlist.o: token.h tokenlist.h ytab.h
	gcc -c -g tokenlist.c

lex.yy.o: lex.yy.c ytab.h token.h
	gcc -c -g lex.yy.c
	
lex.yy.c: g0.l
	flex g0.l

submitfiles = main.c token.h tokenlist.h tokenlist.c g0.l makefile
submit: $(submitfiles)
	mkdir hw1
	cp -T hw1/ $(submitfiles)
	gzip hw1/
	
clean:
	rm main.o tokenlist.o lex.yy.o lex.yy.c
	rm -R hw1/
	
# complex: $(lexgens) $(yaccgens)
	# $(cc) $(lexgens) $(yaccgens) $(ccopts) -o $(prj)

# clean:
	# rm $(lexgens) $(yaccgens) $(prj)

# $(yaccgens): $(prj).y sym.h
	# $(yacc) $(yaccopts) $(prj).y

# $(lexgens): $(prj).l $(yaccgens) sym.h
	# $(lex) $(lexopts) $(prj).l
