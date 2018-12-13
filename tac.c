/*
Baylus Tunnicliff
11/16/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #5: Intermediate Code Generation
"tac.c"
      Three Address Code


Key:
"FIX" - these are placed throughout the code where i believe improvement or errors might occur,
   but arent crucial to fix at the moment.
"LOOK" - these are places where i might need to take another look at them later, when i have time,
   as they might just be bad form, improper handling, etc.
*/

/*
 * Three Address Code - skeleton for CS 445
 */
#include <stdio.h>
#include <stdlib.h>
#include "tac.h"

struct instr *gen(int op, struct addr a1, struct addr a2, struct addr a3)
{
  struct instr *rv = calloc(1, sizeof (struct instr));
  if (rv == NULL) {
     fprintf(stderr, "out of memory\n");
     exit(4);
     }
  rv->opcode = op;
  rv->dest = a1;
  rv->src1 = a2;
  rv->src2 = a3;
  rv->next = NULL;
  return rv;
}

struct instr *copylist(struct instr *l)
{
   if (l == NULL) return NULL;
   struct instr *lcopy = gen(l->opcode, l->dest, l->src1, l->src2);
   lcopy->next = copylist(l->next);
   return lcopy;
}

struct instr *append(struct instr *l1, struct instr *l2)
{
   if (l1 == NULL) return l2;
   struct instr *ltmp = l1;
   while(ltmp->next != NULL) ltmp = ltmp->next;
   ltmp->next = l2;
   return l1;
}

struct instr *concat(struct instr *l1, struct instr *l2)
{
   return append(copylist(l1), l2);
}
