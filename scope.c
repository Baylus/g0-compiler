/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #3: Semantic Analysis
"scope.c"
	Scope source
*/

#include "scope.h"
#include "symt.h"
#include "type.h"
#include "token.h"
#include <stdio.h>

scope_t *create_scope(char *name, scope_t *parent)
{
   scope_t *n = calloc(1, sizeof(scope_t));

   n->scopeName = name;
   n->parentScope = parent;
   initTables(n->SymbolTable);

   return n;
}

sym_t *scope_check(scope_t *s, char *name)
{
   /*  Checks the scope_t provided to see if the symbol name was found already declared.
	 *
	 * 
	 * 
	 * 
	 */

   return lookUp(s->SymbolTable, name);
}

sym_t *findSymbol(scope_t *s, char *name)
{
   /* 
      Checks every scope visible to this one to find the given symbol name.
    */
   sym_t* t = NULL;
   scope_t* p = s;

   while ( p != NULL )
   {
      if ( (t = lookUp(p->SymbolTable, name) ) != NULL ) break;
      // Look in next higher up scope.
      p = p->parentScope;
   }

   return t;
}

sym_t* scope_addSymbol(scope_t *s, struct token *t, type_t* r )
{
   sym_t* p = addSym( s->SymbolTable, t->text, t->lineno );
   if (p == NULL)
   {
      fprintf(stderr, "couldn't add %s to symbol table", t->text);
      perror("Couldn't add symbol to table.");
      exit(-3);
   }
   p->parenscope = s;
   p->type = r;

   return p;
}
