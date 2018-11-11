/*
Author of Code 
   Clint Jeffery

Baylus Tunnicliff
10/3/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #3: Semantic Analysis
"type.h"
	Defining types of variables.

   Requires scope.h
*/

#ifndef TYPE_H
#define TYPE_H

struct Scope;

// "Prototyping" struct fields
struct field;
/* 
base_type codes:
-1 void
1 int
2 double
3 string
4 bool
5 list
6 table
7 func
8 class_object    i.e. pet dog;  pet == class_type , dog == class_object
9 class_type
10 
11
12
13
14
15
16
 */
typedef struct type {
   /*
    * Integer code that says what kind of type this is.
    * Includes all primitive types: 1 = int, 2=float,
    * Also includes codes for compound types that then also
    * hold type information in a supporting union...
    * 7 = array, 8 = struct, 9 = pointer etc. */
   int base_type;
   union {
      struct list {
         int size; /* allow for missing size, e.g. -1 */
	      struct type *elemtype; /* pointer to type for elements in array,
	 				follow it to find its base type, etc.*/
      } l;
      struct table {
         // Table type stuff.
         int size;
         struct type* index;
         struct type* elemtype;
      } t;
      struct struc {		/* structs/classes */
         char *label;
         int nfields;
         // struct field **f; // This makes more sense if it was a scope*
         struct Scope* scope;
      } s;
      struct func {
         struct type* retType;
         int nargs;
         struct type** argtype;
      } f;
      struct type *p;   /* pointer type, points at another type : used for pointing at class types */
   } u;
} type_t;

struct field
{ /* members (fields) of structs */
   char *name;
   struct type *elemtype;
};

#endif

