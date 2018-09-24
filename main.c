/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #1: Lexical Analyzer
"main.c"
	Main compiler source file. 
	
	Depends on:
		token.h
			struct token,
		g0.l
			yytoken, yyin, yylex, 
*/

// #include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>	// strlen(),

#include "token.h"
#include "tokenlist.h"

#include "tree.h"

extern int yyparse();	// g0gram.y
extern tree* yytree;		// g0gram.y

extern int yylex();		// g0.l
extern struct token* yytoken;	// g0.l
extern FILE* yyin;		// g0.l

char* filename;
char* addExtension( char* f ) {
	char* n = calloc(4000, sizeof(char));
	if (n == NULL) {
		perror("Couldnt allocate memory for filename");
		exit(-1);
	}
	int needsExtension = 1;
	char* p = f;
	int i = 0;
	while (p[i] != '\0') {
		if (p[i] == '.') needsExtension = 0;
		n[i] = p[i++];
	}
	if (needsExtension) {
		n[i++] = '.';
		n[i++] = 'g';
		n[i++] = '0';
	}
	n[i] = '\0';	// Null terminate string.
	n = realloc(n, strlen(n));
	if (n == NULL) {
		perror("Couldnt reallocate memory for filename");
		exit(-1);
	}
	return n;
}

int main(int argc, char** argv)
{
	char* filenames[argc - 1]; // allocate memory for the file names.
	
	if (argc < 2) {
		printf("Error: No files to scan provided.\n");
		exit(-1);
	}
	// int files = argc - 1;
	int i = 1;
	for ( ; i < argc ; ++i) {
		filename = addExtension( argv[i] );
		filenames[i - 1] = filename;
		
		// yyin = fopen(filename.c_str(), "r");
		yyin = fopen(filename, "r");
		if ( yyin == NULL ) {
			printf("Error: Failed to open file %s", filename);
			exit(-1);
		}
		
		int toknum = 0;
		// while ( (toknum = yylex()) != 0 ){
		// 	addToken(yytoken);
		// }
		// printList();
		// deleteList();
		
		if ( yyparse() == 0 )
		{
			printList( yytree, 0 );
			postTraversal( yytree, 0, deleteTree );
		}

		fclose(yyin);
		yyin = NULL;
		// ++i;	// move onto next
	}
	
	return 0;
}