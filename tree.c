/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #2: Syntax Analysis
"tree.c"
	Tree definition.
	Is used for creating the AST ( Abstract Syntax Tree )
*/

#include "tree.h"
#include "token.h"
#include "y.tab.h"
#include <stdarg.h> // va_start()
#include <stdlib.h> // size_t
#include <stdio.h>  // stderr

void printToken(struct token *t)
{
	/*
	 *
	 *
	 *
	 *
	 *
	 */
	if (t == NULL) return;

	printf("%d  ", t->category);
	switch (t->category)
	{
		case INTLITERAL:
			printf("%d", t->ival);
			break;
		case FLOATLITERAL:
			printf("%f", t->dval);
			break;
		case STRINGLITERAL:
			printf("%s", t->sval);
			break;
			// default:
			// perror("ERROR: tokenlist.c, func: printToken(): unknown category\n\n");
			// exit(-1);
	}
	printf("\n");
}

tree *alctree( char* label, int code, int nkids, ... )
{ 
   int i;
   va_list ap;
//    size_t treeSize = sizeof(tree);
//    if (nkids > 0)
//    {
//        treeSize += sizeof(tree *) * ( nkids );
//    }
   tree *ptr = malloc(sizeof(tree));
   if (ptr == NULL) {fprintf(stderr, "alctree out of memory\n"); exit(1); }
	ptr->kids = malloc( sizeof(tree *) * (nkids) );
	if (ptr->kids == NULL)
	{
		fprintf(stderr, "alctree out of memory\n");
		exit(1);
	}
	ptr->label = label;
   ptr->code = code;
   ptr->nkids = nkids;
   va_start(ap, nkids);
   for(i=0; i < nkids; ++i)
      ptr->kids[i] = va_arg(ap, tree *);
   va_end(ap);
   return ptr;
}

void treeprint(tree *t, int depth)
{
  int i;
  printf("%*s", depth * 2, " "); 

	if (t->nkids == 0)
	{
		printToken(t->token);
		return;
	}

  printf(" %s: %d\n", t->label, t->nkids);
  
  for(i=0; i < t->nkids; ++i)
	{
		if (t->kids[i] != NULL){
    		treeprint(t->kids[i], depth+1);
		}
	}

}

tree *addLeaf(int code, struct token *t)
{
	tree* ptr = malloc(sizeof(tree));
	if (ptr == NULL)
	{
		fprintf(stderr, "alctree out of memory\n");
		exit(1);
	}
	ptr->token = t;
	ptr->code = code;
	ptr->label = NULL;
	ptr->nkids = 0;

	return ptr;
}

void deleteTree( tree* t )
{
	if (t->nkids > 0){
		free(t->kids);
	}
	else 
	{
		if (t->token != NULL)
		{
			free(t->token->text);
			if (t->token->category)
				free(t->token->sval);
			free(t->token);
		}
	}
	free(t);
}

void postTraversal(tree *t, int depth, void (*f)(tree *))
{
	int i = 0;

	for (i = 0; i < t->nkids; ++i)
	{
		if (t->kids[i] != NULL)
		{
			postTraversal(t->kids[i], depth + 1, (*f));
		}
	}
	(*f)(t);
}
