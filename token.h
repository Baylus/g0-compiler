/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #1: Lexical Analyzer
"token.h"
*/

#ifndef TOKEN
#define TOKEN

typedef struct Symbol sym_t;

// #include "symt.h"

typedef struct token {
	int category;   /* the integer code returned by yylex */
	char *text;     /* the actual string (lexeme) matched */
	int lineno;     /* the line number on which the token occurs */
	char *filename; /* the source file in which the token occurs */
	union {
	   int ival;       /* for integer constants, store binary value here */
	   double dval;	   /* for real constants, store binary value here */
	   char *sval;     /* for string constants, malloc space, de-escape, store */
					   /*    the string (less quotes and after escapes) here */
		sym_t *tval; /* The information about the identifier */
		// struct Symbol *tval; /* The information about the identifier */
	};
} tok_t;
#endif