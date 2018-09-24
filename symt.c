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

#include "symt.h"



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

	return addIdent( hashTable[ hash(name) % HASH_TABLE_SIZE ], i );
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

long hash(char *n)
{
	/*
	 *
	 * 
	 * 
	 */
	long hashAddress = 0;
	int counter;
	for (counter = 0; n[counter] != '\0'; counter++)
	{
		hashAddress = n[counter] + (hashAddress << 6) + (hashAddress << 16) - hashAddress;
	}
	return hashAddress;
}


// Linked List functions.
ident *addIdent(identList l, ident *i)
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

	if ( l.head == NULL ) {
		// Add to empty list
		l.head = n;
		l.tail = n;
	}
	else 
	{
		l.tail->next = n;
		l.tail = l.tail->next;
	}
	return n;
}

ident *searchList(identList l, char *name)
{
	/*
	 *
	 * 
	 * 
	 */
	listNode* p = l.head;
	while ( p != l.tail ) 
	{
		if ( strcmp(p->info->name, name) == 0 ) {
			p->info->numInstances += 1;
			return p;
		}
		p = p->next;
	}
	// Check last item.
	if (strcmp(p->info->name, name) == 0)
	{
		p->info->numInstances += 1;
		return p;
	}

	return NULL;
}

