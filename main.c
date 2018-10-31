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

#include "tree.h"
#include "symt.h"

#include "processTree.h"	// semanticCheck(),

extern int yyparse();	// g0gram.y
extern tree* yytree;		// g0gram.y
extern int yydebug;		// g0gram.y

extern int yylex();		// g0.l
extern struct token* yytoken;	// g0.l
extern FILE* yyin;		// g0.l
extern char* yyfilename;	//g0lex.l

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
		if (p[i] == '.') 
			needsExtension = 0;
		else if ( p[i] == '/' )
		{
			// if path is "../Examples/calc", then the dots dont denote extensions.
			// 	Account for this fact by resetting the boolean when '/' encountered
			needsExtension = 1;
		}
		n[i] = p[i];
		++i;
	}
	if (needsExtension) {
		n[i++] = '.';
		n[i++] = 'g';
		n[i++] = '0';
	}
	n[i] = '\0';	// Null terminate string.
	n = realloc(n, i + 1);
	if (n == NULL) {
		perror("Couldnt reallocate memory for filename");
		exit(-1);
	}
	return n;
}

int checkCommandOptions(char* arg) {
	/*  Checks the command argument given to see if
	 *		it is a command option.
	 *  Currently recognized options
	 * 	"-t", "-T", or "--tree": Prints out the syntax tree generated for the given files
	 * 	"-s", "-S", or "--symbol": Prints out the different symbol tables generated.
	 * 
	 * 
	 */

	if ((strcmp("-t", arg) == 0) || (strcmp("-T", arg) == 0) || (strcmp("--tree", arg) == 0))
	{
		// tree requested.
		return 1;
	}
	if ((strcmp("-s", arg) == 0) || (strcmp("-S", arg) == 0) || (strcmp("--symbol", arg) == 0))
	{
		// symbol requested.
		return 2;
	}
	if ((strcmp("-d", arg) == 0) || (strcmp("-D", arg) == 0) || (strcmp("--debug", arg) == 0))
	{
		// symbol requested.
		return 3;
	}
	return 0;
}


int main(int argc, char** argv)
{
	char* filenames[argc - 1]; // allocate memory for the file names.
	int boolPrintTree = 0;
	int boolPrintSymbol = 1;

	if (argc < 2) {
		printf("Error: No files to scan provided.\n");
		exit(-1);
	}

	// int files = argc - 1;
	int i = 1;
	for (i = 1; i < argc; ++i)
	{
		switch( checkCommandOptions(argv[i]) ){
			case 1:
				boolPrintTree = 1;
				break;
			case 2:
				boolPrintSymbol = 1;
				break;
			case 3:
				yydebug = 1;
				break;
		}
	}

	for (i = 1; i < argc; ++i)
	{
		if ( argv[i][0] == '-' )
			continue;
		yyfilename = addExtension(argv[i]);
		filenames[i - 1] = yyfilename;
		printf("\nParsing file: %s\n", yyfilename);

		// yyin = fopen(filename.c_str(), "r");
		yyin = fopen(yyfilename, "r");
		if (yyin == NULL)
		{
			printf("Error: Failed to open file %s\n", yyfilename);
			perror("Error with opening file\n");
			exit(-1);
		}

		// int toknum = 0;

		if (yyparse() == 0)
		{
			if (boolPrintTree){
				treeprint(yytree, 0);
			}
			// postTraversal( yytree, 0, deleteTree );
		}
		// destroyTables();

		if (boolPrintSymbol)
			semanticCheck(yytree, boolPrintSymbol);

		fclose(yyin);
		yyin = NULL;
	}



	for (i = 1; i < argc; ++i)
	{
		free(filenames[i - 1]);
		filenames[i - 1] = NULL;
	}

	return 0;
}