/*
Baylus Tunnicliff
11/16/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #5: Intermediate Code Generation
"codegen.c"
      Begins intermediate code generation.


Key:
"FIX" - these are placed throughout the code where i believe improvement or errors might occur,
   but arent crucial to fix at the moment.
"LOOK" - these are places where i might need to take another look at them later, when i have time,
   as they might just be bad form, improper handling, etc.
*/

#include <stdlib.h>

#include "tree.h"


char *newlabel()
{
   /* 
   Generates new string for the label of a new code region address
    */
   static int i = 0;
   char *s = calloc(1, sizeof(int) * 8 + 1); // According to itoa page, it must be this size to guarante big enough string
   s = itoa(i++, s, 10);
   return s;
}






#define DR_J_STUFF 1


////////////////////// DR J's Stuff
#ifdef DR_J_STUFF

#include <stdio.h>
#include "tree.h"

void codegen(struct tree *t)
{
   int i, j;
   if (t == NULL)
      return;

   /*
    * this is a post-order traversal, so visit children first
    */
   for (i = 0; i < t->nkids; i++)
      codegen(t->child[i]);

   /*
    * back from children, consider what we have to do with
    * this node. The main thing we have to do, one way or
    * another, is assign t->code
    */
   switch (t->label)
   {
   case O_ADD:
   {
      struct instr *g;
      t->code = concat(t->child[0]->code, t->child[1]->code);
      g = gen(O_ADD, t->address,
              t->child[0]->address, t->child[1]->address);
      t->code = concat(t->code, g);
      break;
   }
   /*
    * ... really, a bazillion cases, up to one for each
    * production rule (in the worst case)
    */
   default:
      /* default is: concatenate our children's code */
      t->code = NULL;
      for (i = 0; i < t->nkids; i++)
         t->code = concat(t->code, t->child[i]->code);
   }
}


///////////////////////////////
void codegen(struct tree *t)
{
   // pre-order stuff, e.g. label generation
   switch (t->prodrule)
   {
      ... case ITERATION_STMT : // inherited attributes for while loop
                                // push an inherited attribute to child before visiting them
                                t->child[2]
                                    ->true = newlabel();
      break;
      ...
   }
   // visit children
   for (i = 0; i < t->nkids; i++)
      codegen(t->child[0]);

   // post-order stuff, e.g. code generation
   switch (t->prodrule)
   {
      ... case CONDEXPR_2 : // synthesized attribs for CondExpr: Expr < Expr
                            t->code = concat(
          t->child[0]->code,
          t->child[2]->code,
          gen(BLT, t->child[0]->place, t->child[2]->place, t->true),
          gen(GOTO, t->false));
      break;
   case ITERATION_STMT: // synthesized attributes for while loop
      t->code = concat(
          gen(LABEL, t->child[2]->first),
          t->child[2]->code,
          gen(LABEL, t->child[2]->true),
          t->child[4]->code,
          gen(GOTO, t->child[2]->first));
      break;
   }

   void codegen(nodeptr t)
   {
      int i, j;
      if (t == NULL)
         return;

      /*
    * this is a post-order traversal, so visit children first
    */
      for (i = 0; i < t->nkids; i++)
         codegen(t->child[i]);

      /*
    * back from children, consider what we have to do with
    * this node. The main thing we have to do, one way or
    * another, is assign t->code
    */
      switch (t->label)
      {
      case PLUS:
      {
         t->code = concat(t->child[0].code, t->child[1].code);
         g = gen(PLUS, t->address,
                 t->child[0].address, t->child[1].address);
         t->code = concat(t->code, g);
         break;
      }
      /*
    * ... really, a bazillion cases, up to one for each
    * production rule (in the worst case)
    */
      default:
         /* default is: concatenate our children's code */
         t->code = NULL;
         for (i = 0; i < t->nkids; i++)
            t->code = concat(t->code, t->child[i].code);
      }
   }
#endif