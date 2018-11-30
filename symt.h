/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #2: Syntax Analysis
"symt.h"
	Symbol Table Header
*/
#define HASH_TABLE_SIZE 41

#ifndef SYM_T_H
#define SYM_T_H

typedef struct Scope scope_t;

#include <stdlib.h>
#include <stdint.h> // uint32_t
#include "token.h"
// #include "scope.h"	// scope_t
#include "type.h"	// type_t


typedef struct Symbol
{
	char* label;
	type_t* type;
	// declaration info
	int lineno;
	scope_t* parenscope;

	union {
		// Supporting information for semantic analysis
		struct auxflags
		{
			// auxillary flags like "isconst"
			int isconst;
		} f;
		// if symbol has a symboltable entry it owns.
		scope_t* myScope;
	} s;
	// Create a linked list to store local variables.
} sym_t;

typedef struct SymbolListNode {
	sym_t* info;
	struct SymbolListNode* next;
} listNode;

typedef struct SymbolLinkedList {
	listNode* head;
	listNode* tail;
	int size;
} symList_t;

void initTables( symList_t* );
void destroyTables( symList_t* );

sym_t *addSym(symList_t *, char *name, int lineno);
sym_t *lookUp(symList_t *, char *name);
uint32_t hash(char *n);

// Linked List functions.
sym_t* addIdent( symList_t* l, sym_t* i );
sym_t *searchList(symList_t l, char *name);

void printTable( symList_t* , int depth);

#endif