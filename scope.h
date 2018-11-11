/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #3: Semantic Analysis
"scope.h"
	Scope header
*/

#ifndef SCOPE_T_H
#define SCOPE_T_H

typedef struct Symbol sym_t;
typedef struct SymbolLinkedList symList_t;

#include "symt.h"
#include "type.h"
#include "token.h"

typedef struct Scope
{
   char *scopeName;
   symList_t SymbolTable[HASH_TABLE_SIZE];
   
   // Declaration info
   struct Scope *parentScope;
   
} scope_t;

scope_t *create_scope(char *name, scope_t *parent);

sym_t *scope_check(scope_t *s, char *name);

sym_t* findSymbol(scope_t* s, char* name);

sym_t* scope_addSymbol(scope_t *s, struct token *t, type_t*);

void delete_Scope( scope_t* s );

#endif