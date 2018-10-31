/*
Baylus Tunnicliff
10/3/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #3: Semantic Analysis
"processTree.h"
	Processing the tree's output
*/

#ifndef PROCESS_TREE_H
#define PROCESS_TREE_H


#include "tree.h"
// #include "symt.h"
#include "scope.h"

int generateSymbolTables( tree* t, int bool_print );
int semanticCheck( tree* t, int print );
int checkUndeclaredSymbols( tree* t, int print );


#endif