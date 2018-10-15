/*
Baylus Tunnicliff
10/3/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #3: Semantic Analysis
"processTree.c"
	Processing the tree's output
*/

#include "processTree.h"
#include "tree.h"
#include "symt.h"
#include "scope.h"   // scope_t
#include "token.h"   //struct token
#include "type.h" //struct type, struct field

#include "g0gram.h"  // Token integer codes.

#include <stdio.h>
#include <string.h>

// Consider making these examples of the basic types if it helps later.
// Doing this would make clean up severely more troublesome.
type_t I_Type, D_Type, S_Type, B_Type, L_Type, T_Type;

extern scope_t* yyscope;   // g0lex.l

void error(tree* t, char* s) {
   /* 
      Semantic analysis default error function.
   */
   tok_t* n = getToken(t);
   int line = -1;
   char* tname = NULL;
   if ( n != NULL )
   {
      line = n->lineno;
      tname = n->text;
      fprintf(stderr, "Semantic Error:\n %s, on line %d\nnear token %s\n ", s, line, tname);
   }

   fprintf(stderr, "Semantic Error:\n %s\n", s);

   exit(3);
}


int isLeaf( tree* t )
{
   /* 
      Returns a boolean int depending on if the tree node is a 
         leaf or not.

      Details:
         tree should exist, if it doesnt, bogus return value is given.
    */
   if ( t == NULL ) return -1;
   if ( t->nkids < 1 && t->token != NULL )
   {
      return 1;
   }
   
   return 0;
}

struct token* getToken( tree* t )
{
   /* 
      Only use on terminal symbol trees.
      or
      Some specified non-terminal rules.
         Type, 
         These work because they grab the token of either a non-terminal rule that has
         boiled down to a terminal production, or they have been accounted for and will return
         the desired token that is needed, defined by whatever is necessary when created.
      Type will return the simple integer code of the type of the symbol.
         WARNING: This must be replaced or enhanced once typechecking is needed, because this
         will not provided the information of complex types like lists and tables.

    */
   if ( t == NULL ) return NULL;

   switch( t->code ) {
      case 125:   // ListType
      case 130:   // TableType
      case 131:   // TableType
         return getToken( t->kids[0] );
         break;
      // case:

      //    break;
      // case:

      //    break;
      // case:

      //    break;
      // case:

      //    break;
   }

   // If wasn't handled up above, just grab left-most token you can find.
   if ( isLeaf(t)) return t->token;
   else
   {
      return getToken( t->kids[0] );
   }

   return NULL;
}

type_t* getType( tree* t )
{
   /* 
      Gets the type from type_t from Type rules
         and constructs a type_t variable for them.
    */
   if (t == NULL) return NULL;

   type_t* p = NULL;

   if ( isLeaf(t) ) {
      p = calloc( 1, sizeof(type_t) );
      // Type is a primitive type.
      switch( t->token->category )
      {
         case INT:
            p->base_type = 1;
            break;
         case DOUBLE:
            p->base_type = 2;
            break;
         case STRING:
            p->base_type = 3;
            break;
         case BOOL:
            p->base_type = 4;
            break;
         case LIST:
            p->base_type = 5;
            p->u->l->size = -1;
            p->u->l->elemtype = calloc( 1, sizeof(type_t) );
            p->u->l->elemtype->base_type = 1;
            break;
         case TABLE:
            p->base_type = 6;
            p->u->t->index = calloc( 1, sizeof(type_t) );
            p->u->t->index->base_type = 3;      // Default index type is string
            p->u->t->elemtype = calloc( 1, sizeof(type_t) );
            p->u->t->elemtype->base_type = 1;   // Default element type is int
            break;
         case CLASS_NAME:
            p->base_type = 8;
            sym_t* class = findSymbol( yyscope, t->token->text );
            if ( class == NULL ) {
               error(t, "Class type couldn't be found from the current scope!\n It has to exist though...");
            } 
            else
            {
               p->u->p = class->type;
            }
            break;
         case VOID:
            p->base_type = -1;
            break;
      }
      // successfully found the type.
   }
   else
   {
      // Not a primitive type or a basic list, table, or class object.
      p = calloc(1, sizeof(type_t));
      // p = getType( t->kids[0] );
      switch (t->code)
      {
         case 125:   // ListType: LIST < Type > 
            p->base_type = 5;
            p->u->l->size = -1;
            p->u->l->elemtype = getType( t->kids[1] );
            break;
         case 130:   // TableType: TABLE < Type >
            p->base_type = 6;
            p->u->t->index = calloc(1, sizeof(type_t));
            p->u->t->index->base_type = 3;   // Default index type == string
            p->u->t->elemtype = getType( t->kids[1] );
            break;
         case 131: // TableType: TABLE < Type , Type >
            p->base_type = 6;
            p->u->t->index = getType(t->kids[1]);
            p->u->t->elemtype = getType(t->kids[2]);
            break;
         case 158: // ClassHeader: CLASS CLASS_NAME
            p->base_type = 9;
            p->u->s->label = strdup(t->kids[1]->token->text);
            break;
         case 192: // MethodHeader: Type MethodDeclarator
         case 193: // MethodHeader: VOID MethodDeclarator
            p->base_type = 9;
            p->u->s->label = strdup(t->kids[1]->token->text);
            break;
         default:
            error(t, "Unrecognized non-terminal rule type.");
            break;
      }
   }

   if ( p == NULL ) error(t, "Couldn't get type from function getType()");
   return p;
}

int processVariableDeclarations( tree* t, int bool_print )
{
   /* 
      Processes variable declarations, regardless of their location in the program.
         or what type they are.

      Details:
         This makes one type for every declarator list. So when freeing the memory, 
            it is important to find a way to only free the one type that was made for each
            symbol. This could be done by having a list of all the types made,
            and simply freeing those at the end.
         If this causes too many issues, a fix can be made to simply make copies of the 
            type and give one to each symbol.
    */

   // For each possible symbol, add it to proper symbol table.
   // Get type of variable declarations.
   // token* type = getToken( t->kids[0] );
   type_t* varType = getType( t->kids[0] );
   int count = 0;
   tree* p = t->kids[1];   // Should be the variable list
   while ( ! isLeaf(p) )   // While there is a list to process.
   {
      switch( p->code )
      {
         case 113:   // VariableDeclaratorList: VariableDeclaratorList , VariableDeclaratorId
         case 168:   // ClassVariableList: ClassVariableList , IDENT
            ++count; 
            tok_t* T = getToken(p->kids[1]);
            sym_t* s = scope_addSymbol( yyscope, T, varType );
            if ( s == NULL ) error( p, "Failed to add symbol while processing var declarator list.");
            break;
         // default:
         //    // do nothing
      }
      // We know there is a list to process.
      p = p->kids[0];
   }
   // Now there should be one token left to add.
   tok_t* T = getToken( p );
   ++count;
   if ( scope_addSymbol( yyscope, T, varType ) == NULL)
   {
      error(p, "Failed to add symbol while processing var declarator list.");
   }
   return count;
}

int generateSymbolTables(tree *t, int bool_print)
{
   /*  Generates Symbol tables for the provided tree.
    * 
    * 
    * 
    * 
    * 
    */
   if (t == NULL) {
      return 0;
   }
   if (yyscope == NULL)
   {
      // Global scope hasn't been created yet.
      yyscope = create_scope( "Global Scope", NULL );
   }

   if ( isLeaf(t) )
      return -1;

   /* 
   Process states that need to do something.
      Different things to do.
         Process variable declarations.
            check redeclaration.
            add symbol.
         change scopes.
            create new scopes.
            print symbol info if print turned on.
   */
   switch ( t->code )
   {
      case 108:   // GlobalVariable
      case 172:   // ClassVariable
      case 525:   // LocalVariable
         // For each possible symbol, add it to proper symbol table.
         // Get type of variable declarations.
         processVariableDeclarations( t, bool_print );
         break;
      /* Scope Changes */
      case 154:   // ClassDeclaration: ClassHeader ClassBlock
         tree* p = t->kids[0];   // p = ClassHeader: CLASS CLASS_NAME
         tok_t* T = p->kids[1]->token;
         type_t* cType = getType( p );
         sym_t* sym = scope_addSymbol( yyscope, T, cType );
         char* str = calloc ( strlen("Class Scope: ") + strlen(T->text) + 1, sizeof(char) )
         sym->s->myScope = create_scope( sprintf("Class Scope: %s", T->text), yyscope );
         // change scope to new scope.
         yyscope = sym->s->myScope;
         // Process rest of class.
         generateSymbolTables( t->kids[1], bool_print );
         // Revert to previous scope.
         yyscope = yyscope->parentScope;
         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      case :

         break;
      default:
         // Process further trees
         int i;
         for (i = 0; i < nkids; ++i)
         {
            generateSymbolTables( t->kids[i], s, bool_print );
         }
   }

}
