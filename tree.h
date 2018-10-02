/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #2: Syntax Analysis
"tree.h"
	Tree Header
	Is used for creating the AST ( Abstract Syntax Tree )
*/

#ifndef TREE_H
#define TREE_H

#include "token.h"

typedef struct treeNode {
	char* label;
	int code;		/* terminal or nonterminal symbol */
	int nkids;
	union {
	   // struct token { ...  } leaf; // or: struct token *leaf;
	   struct token* token; // or: struct token *leaf;
	   struct treeNode **kids;
   };
} tree ;

tree* alctree( char* label, int code, int kids, ... );
void treeprint( tree *t, int depth );
tree* addLeaf( int code, struct token* t );
void postTraversal(tree *t, int depth, void (*f)(tree *));
void deleteTree(tree *);
void printToken(struct token *t);

#endif

