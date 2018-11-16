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
#include "token.h"

// extern struct scope_t;
extern void print_Scope(scope_t *s, int depth);

symList_t hashTable[HASH_TABLE_SIZE] = {};

void initTables(symList_t n[HASH_TABLE_SIZE])
{
	int i;
	for ( i = 0; i < HASH_TABLE_SIZE; ++i )
	{
		n[i].head = NULL;
		n[i].tail = NULL;
		n[i].size = 0;
	}
}

void freeNode( listNode* x )
{
	if (x != NULL)
	{
		free(x->info->label);
		free(x->info);
		free(x);
	}
}

void destroyTables(symList_t n[HASH_TABLE_SIZE])
{
	int i;
	for (i = 0; i < HASH_TABLE_SIZE; ++i)
	{
		listNode *p = n[i].head;
		while (p != n[i].tail)
		{
			n[i].head = n[i].head->next;
			freeNode(p);
			p = n[i].head;
		}
		freeNode(p);
	}
}



sym_t *addSym(symList_t n[HASH_TABLE_SIZE] , char *name, int lineno)
{
	/* Adds given symbol into the list
	 *
	 * Uses:
	 * 	name: str, name of identifier
	 * 	code: int, integer code of type of identifier.
	 * 	lineno: int, line # symbol ocurred on. ( later might be offset instead, but for now, its line number )
	 * 	s: scope_t, parent scope of the symbol
	 */
	sym_t* i = malloc( sizeof(sym_t) );
	if (i == NULL)
	{
		fprintf(stderr, "symt.c: addIdent wasnt given sym_t to add.\n");
		exit(1);
	}
	i->label = strdup(name);
	// i->numInstances = 1;
	// i->typeCode = code;
	// i->type = t;
	i->lineno = lineno;
	// i->parenscope = s;

	return addIdent( &n[ hash(name) % HASH_TABLE_SIZE ], i );
}

sym_t *lookUp(symList_t n[HASH_TABLE_SIZE], char *name)
{
	/*
	 *
	 * 
	 * 
	 */
	sym_t* p = NULL;
	p = searchList( n[ hash(name) % HASH_TABLE_SIZE ], name );
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
sym_t *addIdent(symList_t* l, sym_t *i)
{
	/*
	 *
	 * 
	 * 
	 */
	if ( i == NULL )
	{
		fprintf(stderr, "symt.c: addIdent wasnt given sym_t to add.\n");
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

sym_t *searchList(symList_t l, char *name)
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
		if ( strcmp(p->info->label, name) == 0 ) {
			// p->info->numInstances += 1;
			return p->info;
		}
		p = p->next;
	}
	// Check last item.
	if (strcmp(p->info->label, name) == 0)
	{
		// p->info->numInstances += 1;
		return p->info;
	}

	return NULL;
}

void printTable(symList_t *L, int depth)
{
	if (L == NULL) return;
	listNode* q = L->head;
	while (q != NULL)
	{
		printf("%*s", depth * 4, " ");
		printf( "\"%s\" : type %d\n", q->info->label, q->info->type->base_type);
		if ( q->info->s.myScope != NULL )
			print_Scope( q->info->s.myScope, depth );
		
		q = q->next;
	}
}