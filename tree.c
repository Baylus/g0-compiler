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

tree *alctree(int label, int nkids, ... )
{ 
   int i;
   va_list ap;
   tree *ptr = malloc(sizeof(tree) +
                             (nkids-1)*sizeof(tree *));
   if (ptr == NULL) {fprintf(stderr, "alctree out of memory\n"); exit(1); }
   ptr->label = label;
   ptr->nkids = nkids;
   va_start(ap, nkids);
   for(i=0; i < nkids; ++i)
      ptr->child[i] = va_arg(ap, tree *);
   va_end(ap);
   return ptr;
}

int treeprint(tree *t, int depth)
{
  int i;

  printf("%*s %s: %d\n", depth*2, " ", humanreadable(t->prodrule), t->nkids);
  
  for(i=0; i < t->nkids; ++i)
    treeprint(t->kids[i], depth+1);

}