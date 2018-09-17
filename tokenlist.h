/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #1: Lexical Analyzer
"tokenlist.h"
*/

#ifndef TOKENLIST
#define TOKENLIST

#include "token.h"

struct tokenlist {
	struct token* tok;
	struct tokenlist* next;
};

int addToken(struct token* t);
void printToken(struct token* t);
void printList();
void deleteList();

#endif