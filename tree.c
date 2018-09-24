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
#include <stdarg.h> // va_start()
#include <stdlib.h> // size_t
#include <stdio.h>  // stderr


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

int treeprint(tree *t, int depth)
{
  int i;

  printf("%*s %s: %d\n", depth*2, " ", t->label, t->nkids);
  
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
}

void deleteTree( tree* t )
{
	free(t->kids);
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
