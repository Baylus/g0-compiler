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
	int code;		/* terminal or nonterminal symbol */
	int nkids;
	union {
	   // struct token { ...  } leaf; // or: struct token *leaf;
	   struct token* leaf; // or: struct token *leaf;
	   struct treeNode *kids[9];
   } u;
} tree ;

tree* alctree( struct token* t, tree* parent, ... );
int treeprint( tree *t, int depth )

#endif

