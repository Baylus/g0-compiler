/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #2: Syntax Analysis
"symt.c"
	Symbol Table Definition
*/

#include <stdlib.h>	// NULL
#include <stdio.h>	//fprintf
#include <string.h>	// stdup
#include <stdint.h>	// uint32_t

#include "symt.h"

identList hashTable[HASH_TABLE_SIZE] = {};

void initTables()
{
	int i;
	for ( i = 0; i < HASH_TABLE_SIZE; ++i )
	{
		hashTable[i].head = NULL;
		hashTable[i].tail = NULL;
		hashTable[i].size = 0;
	}
}

void freeNode( listNode* x )
{
	if (x != NULL)
	{
		free(x->info->name);
		free(x->info);
		free(x);
	}
}

void destroyTables()
{
	int i;
	for (i = 0; i < HASH_TABLE_SIZE; ++i)
	{
		listNode *p = hashTable[i].head;
		while (p != hashTable[i].tail)
		{
			hashTable[i].head = hashTable[i].head->next;
			freeNode(p);
			p = hashTable[i].head;
		}
		freeNode(p);
	}
}

ident *addIdentifier(char *name, int code, int lineno)
{
	/*
	 *
	 * Uses:
	 * 	name: str, name of identifier
	 * 	code: int, integer code of type of identifier.
	 */
	ident* i = malloc( sizeof(ident) );
	if (i == NULL)
	{
		fprintf(stderr, "symt.c: addIdent wasnt given ident to add.\n");
		exit(1);
	}
	i->name = strdup(name);
	i->numInstances = 1;
	i->typeCode = code;
	i->lineno = lineno;

	return addIdent( &hashTable[ hash(name) % HASH_TABLE_SIZE ], i );
}

ident *lookUp(char *name)
{
	/*
	 *
	 * 
	 * 
	 */
	ident* p = NULL;
	p = searchList( hashTable[ hash(name) % HASH_TABLE_SIZE ], name );
	return p;
}

uint32_t hash(char *n)
{
	/*
	 *
	 * 
	 * 
	 */
	uint32_t hashAddress = 0;
	int counter;
	for (counter = 0; n[counter] != '\0'; counter++)
	{
		hashAddress = n[counter] + (hashAddress << 6) + (hashAddress << 16) - hashAddress;
	}
	return hashAddress;
}


// Linked List functions.
ident *addIdent(identList* l, ident *i)
{
	/*
	 *
	 * 
	 * 
	 */
	if ( i == NULL )
	{
		fprintf(stderr, "symt.c: addIdent wasnt given ident to add.\n");
		exit(1);
	}
	listNode* n = malloc ( sizeof(listNode));
	if (n == NULL) {
		fprintf(stderr, "symt.c: addIdent out of memory\n");
		exit(1);
	}
	n->info = i;
	n->next = NULL;

	if ( l->head == NULL ) {
		// Add to empty list
		l->head = n;
		l->tail = n;
		l->size = 1;
	}
	else 
	{
		l->tail->next = n;
		l->tail = l->tail->next;
		l->size++;
	}
	return i;
}

ident *searchList(identList l, char *name)
{
	/*
	 *
	 * 
	 * 
	 */
	if ( l.head == NULL) return NULL;
	listNode* p = l.head;
	while ( p != l.tail ) 
	{
		if ( strcmp(p->info->name, name) == 0 ) {
			p->info->numInstances += 1;
			return p->info;
		}
		p = p->next;
	}
	// Check last item.
	if (strcmp(p->info->name, name) == 0)
	{
		p->info->numInstances += 1;
		return p->info;
	}

	return NULL;
}

