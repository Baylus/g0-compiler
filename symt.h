/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #2: Syntax Analysis
"symt.h"
	Symbol Table Header
*/

#ifndef SYM_T_H
#define SYM_T_H

#define HASH_TABLE_SIZE 41

#include <stdlib.h>

typedef struct Identifier
{
	char* name;
	int typeCode;	// Code of identifier type.
	int numInstances;
	// declaration info
	int lineno;
	// Create a linked list to store local variables.
} ident;

typedef struct IdentListNode {
	ident* info;
	struct IdentListNode* next;
} listNode;

typedef struct IdentifierLinkedList {
	listNode* head;
	listNode* tail;
	int size;
} identList;

identList hashTable[HASH_TABLE_SIZE] = {};

ident* addIdentifier( char* name, int code, int lineno);
ident* lookUp( char* name);
long hash( char* n );

// Linked List functions.
ident* addIdent( identList l, ident* i );
ident *searchList(identList l, char *name);

#endif