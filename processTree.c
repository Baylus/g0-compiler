/*
Baylus Tunnicliff
10/3/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #3: Semantic Analysis
"processTree.c"
	Processing the tree's output


Key:
"FIX" - these are placed throughout the code where i believe improvement or errors might occur,
   but arent crucial to fix at the moment.
"LOOK" - these are places where i might need to take another look at them later, when i have time,
   as they might just be bad form, improper handling, etc.
*/

#include "processTree.h"
#include "tree.h"
#include "symt.h"
#include "scope.h"   // scope_t
#include "token.h"   //struct token
#include "type.h" //struct type, struct field

#include "g0gram.h"  // Token integer codes.

#include <stdio.h>
#include <string.h>  //memcpy, strcmp

int isLeaf(tree *t);

// Consider making these examples of the basic types if it helps later.
// Doing this would make clean up severely more troublesome.
type_t *V_Type, *I_Type, *D_Type, *S_Type, *B_Type, *L_Type, *T_Type;

extern scope_t* yyscope;   // g0lex.l

extern int yydebug;     // g0gram.y/g0gram.h

int print = 0;


/* 
How these are going to work is that they will be initialized to hold 5 pointers
initally, they will be used to store the pointers that still need to be free'd,

so it will go like this, put the pointer into the array, increment the current counter for how many
pointers are being held, if unfreed % 5 == 0, resize the array to a size of 5 bigger.
*/
type_t** unfreedPointers = NULL;
int unfreed = 0;

void store( type_t* p )
{
   /* 
   Takes a pointer and stores it in the unfreed pointers list.
   */
   unfreedPointers[unfreed++] = p;
   if ( (unfreed % 5) == 0 )
   {
      unfreedPointers = realloc( unfreedPointers, sizeof(type_t) * (unfreed + 5) );
   }
}

type_t** reverseTypeList( type_t** l, int size )
{
   /* 
   Reverses the order of types of argument lists.

   Details:
      This is because the way the arguments are processed, the list is
      generated in reverse. So this function is intended to put them in the right order.
    */
   if ( l == NULL || size < 1 ) { fprintf(stderr, "Unknown error occurred\n\tReverse lists\n"); exit(-1); }

   type_t* temp = NULL;
   int i = 0, j = size - 1;
   for( ; i < j; ++i, --j )
   {
      temp = l[i];
      l[i] = l[j];
      l[j] = temp;
   }
   return l;
}

int compareTypes( type_t* a, type_t* b )
{
   /* 
   Compares two types 

   Details:
      a is considered to be the base_type, i.e. it is the type we are comparing b to.
      so if the types are not the same, we are considering b to be the faulty type, since
      a is considered the "key".

      Specific type comparing details:
         Functions:
            If b does not have the same return type as a, that does not mean that the type check
            failed, necessarily. There are some cases where this is to be expected, since we 
            cannot generate the return value of a function call, we can only get that from the 
            function's symbol. So this means that this error code must be considered when in
            situations where b's return type cannot be generated.


   Returns:
      0 for no error
      1 for not the same
      > 1: special cases

      Function errors:
      10: not a function
      11: incorrect number of arguments.
      12: unexpected arg type ( differing argument types )
      13: different return types (Not sure if this will ever be useful.)
   */
   if ( a == NULL ) return -1;
   if ( b == NULL ) return 1;
   if ( a->base_type == 2 && b->base_type == 1)
   {
      // This is my solution for promotion. This may not work, and i may have to do the whole
      // adding a new node thing to represent the type conversion, but id rather not do that if i can help it.
      return 0;   // I am hoping by allowing it to be type checked, it will be easy later to simply promote it to a double.
   }

   if ( a->base_type != b->base_type ) return 1;

   switch( a->base_type )
   {
      case -1:  /* void */
      case 1:   /* int */
      case 2:   /* double */
      case 3:   /* string */
      case 4:   /* bool */
         return 0;   // Since we know a->base_type == b->base_type
         break;
      case 5:   /* List */
         if ( !compareTypes( a->u.l.elemtype, b->u.l.elemtype ) ) return 0;
         break;
      case 6:   /* Table */
         if ( !compareTypes(a->u.t.elemtype, b->u.t.elemtype) && !compareTypes(a->u.t.index, b->u.t.index))
            return 0;
         break;
      case 7:  /* function */
        {
            void* af = &(a->u.f);
            void* bf = &(b->u.f);

            if ( b->base_type != 7 )
            {
               return 10;
            }

            if ( af->nargs != bf->nargs )
               return 11;

            // check types of arguments
            int i = 0;
            while ( i < af->nargs )
            {
               /* 
               Reason we can safely just check the return value of compare types here on
               given arguments, without checking the return 13 possibility is because
               g0 prohibits functions from being passed in as arguments to functions.
               */
               if ( compareTypes( af->argtype[i], bf->argtype[i] ) )
                  return 12;
               ++i;
            }

            if ( compareTypes( a->u.f.retType, b->u.f.retType ) )
               return 13;
         }
         break;
      case 8:  /* class_object */
      // This is the same check as asking, "do both objects have the same class type?"
         return checkTypes( a->u.p, b->u.p )
         break;
      case 9:  /* class_type */
         if ( strcmp(a->u.s.label, b->u.s.label) == 0 )
            return 0;
         break;
      default:
         if ( print )
            fprintf(stderr, "Default case found in compareTypes...\n");
         // return 1;
   }
   return 1;
}

int check_operator( int operator, type_t* x, type_t* y )
{
   /* 
      This is going to check whether an operation is allowed for two given types and an operator
   
      Returns:
         0 if allowed,
         1 if not,
         2 if types dont match
         > 9 : special cases
   */

   /* FIX : better error message */

   // Allow lists, tables, and class_objects to be assigned 'null' ( Currently, 'null' is being treated as the void type )
   switch( x->base_type )
   {
      case 5:
      case 6:
      case 8:
         if ( operator == ASN && y->base_type == -1 )
            return 0;
   }
   if ( compareTypes(x,y) ) return 2;
   if ( operator == ASN || operator == SWAP) return 0; // Allow all ASN/SWAP operations

   switch( x->base_type )
   {
      switch (s1->base_type)
      {
         case 2: /* double */
            if ( operator == MOD ) return 1;
            if ( operator == DROLL ) return 1;
         case 1: /* int */
            switch( operator )
            {
               case OROR:
               case ANDAND:
               case BANG:
                  return 1;
               default:
                  return 0;
            }
            break;
         case 3: /* string */
            switch( operator )
            {
               case ASN: 
               case EQ: 
               case NE:
                  return 0; 
               default:
                  return 1;
            }
         case 4: /* bool */
            switch( operator )
            {
               case ASN: 
               case EQ:
               case NE:
               case OROR:
               case ANDAND:
                  return 0; 
               default:
                  return 1;
            }
            break;
         case 5: /* list */
            if ( operator == PLASN ) return 0;
            else return 1;
            break;
         case 6: /* table */
            if ( operator == MIASN) return 0;
            else return 1;
            break;
         case 8: /* class_object */
            // Only allow ASN
            return 1;
            break;
         default:
            error(t, "invalid type assignment, unknown root of error");
      }
   }
}

void deleteType( type_t* t )
{
   if ( t )
   {
      // If t != NULL
      switch( t->base_type )
      {
         case 5:  // list
            deleteType( t->u.l.elemtype );   // Delete children first.
            // free(t)
            break;
         case 6:  // table
            deleteType( t->u.t.elemtype );   // Delete children first.
            deleteType( t->u.t.index );   // Delete children first.

            break;
         case 7:  // function
            deleteType( t->u.f.retType );
            {
               int i;
               for ( i = 0; i < t->u.f.nargs; ++i )
               {
                  deleteType( t->u.f.argtype[i] );
               }
               free( t->u.f.argtype ); // free array.
            }
            break;
         case 8:  // class_object
            free( t->u.s.label );   // Free name.
            break;
         case 9:  // class_type

            break;
         default: // void, int, double, string, bool
            // Dont delete these, they will be cleaned up later.
            return;
      }
      free(t); // Free t, if it isnt a default case.
   }
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
      case 630:   // ListType
      case 635:   // TableType
      case 636:   // TableType
         return getToken( t->kids[0] );
         break;
      case 659:   // ClassDeclaration: ClassHeader ClassBlock
         return t->kids[0]->kids[0]->token;
         break;
      case 741:   // FunctionDefinition:  Type IDENT ( [FormalParameterList] ) FunctionBody
      case 742:   // 
      case 743:   // 
      case 744:   // 
         return t->kids[1]->token;
         break;
      // case :   // Add PrimaryNoNewArray

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
      return getToken( t->kids[0] );	// Is causing segfaults on several files including "funcexample"
   }

   return NULL;
}

// HelperFunctions
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
   else
   {
      fprintf(stderr, "Semantic Error:\n %s\n", s);
   }


   exit(3);
}

sym_t* checkSym( scope_t* s, tok_t* t )
{
   /* 
      Reports an error if symbol does not exist in the current context.
    */
   sym_t* sym = findSymbol( s, t->text );
   if ( sym == NULL ) {
      /* Symbol undefined */
      fprintf(stderr, "Semantic Error:\n Undeclared symbol %s, on line %d\n ", s, t->text, t->lineno);
      exit(3);
   }
   return sym;
}

sym_t* check_addSym( scope_t* s, tok_t* t, type_t* y ) {
   /* 
      Checks the current scope to determine if the symbol already exists
      in the current context.

      Details:
         Will only check the most immediate scope, and report an error
            iff there already exists a symbol in that scope. Otherwise
            will add the symbol to the current scope.
   */
   if ( s == NULL ) error( NULL, " DEBUG ERROR in func processTree.c:check_addSym()");
   
   sym_t* sym = NULL;
   if ( (sym = scope_check( s, t->text )) != NULL )
   {
      /* Symbol already exists in this scope */
      fprintf(stderr, "Error: symbol redeclaration of symbol %s on line %d\n\tOriginal declaration on line %d\n", t->text, t->lineno, sym->lineno);
      error(NULL, "");
   }
   else
   {
      /* Symbol not in current scope */
      sym = scope_addSymbol( s, t, y );
   }
   return sym;
}

scope_t* newScope( tree* t, char* name, scope_t* parent )
{
   /* 
      Creates a new scope and prints out debugging information if yydebug is defined.

      Uses:
         yydebug - int, defined in g0gram.h (by g0gram.y). 
    */
   if ( yydebug ) {
      printf( "Creating new scope for tree\n" );
      treeprint( t, 0 );
   }
   return create_scope( name, parent );
}

scope_t* getSymbolScope( scope_t* p, sym_t* s )
{
   /* 
      Returns the scope owned by the symbol. 

      Details:
         If symbol does not contain a scope, raise an error.
    */
   if ( s == NULL ) return NULL;
   scope_t* t = NULL;
   sym_t* classSym = s; /* By default, make class sym be the symbol given. */
   switch( s->type->base_type )
   {
      case 7:  /* function */
         t = s->s.myScope;
         break;
      case 8:  /* class object */
         {
            type_t* classType = s->type->u.p;
            classSym = findSymbol( p, classType->u.s.label );
            if ( classSym == NULL )
            {
               /* This might make sense in some cases where user enters incorrect class type name for an object.
                  Might have to change how this is handled in the future. */
               fprintf( stderr, "Couldn't find class symbol %s from scope while getting scope of %s\n", classType->u.s.label, s->label );
               error(NULL, "POSSIBLE DEBUG: in func processTree.c:getSymbolScope()");
            }
         }
      case 9:  /* class type */
         /* Class symbol found! */
         t = classSym->s.myScope;
         break;
   }
   return t;
}



scope_t* enterScope( scope_t* s, char* sName )
{
   /* 
      Returns the scope contained by the given symbol name.
    */
   sym_t* sym = findSymbol( s, sName );
   if ( sym == NULL )
      return NULL;
   scope_t* tempScope = getSymbolScope( s, sym );
   if ( s == NULL )
   {
      /* Symbol did not have a scope. */
      fprintf(stderr, "Symbol %s\n", sName);
      error(NULL, "Symbol provided did not have a scope to enter!");
   }
   return tempScope;
}

scope_t *enterObjectScope(scope_t *s, type_t *t)
{
   /*
      Same as enter scope, but uses type instead of name, and only works on objects ( classes )

   Details:
      Works with class object types only... cant think of any other time that you would need
      to qualify something.
    */

   if (t->base_type != 8)
   {
      error(t, "Type does not match class object for qualified name resolution\n");
   }
   char *classname = t->u.p->u.s.label;         // gives the class name that t is an object of.
   scope_t *n = enterScope(yyscope, classname); // Enters the scope of the given class name visible to scope n
   return n;
}

int isLeaf( tree* t )
{
   /* 
      Returns a boolean int depending on if the tree node is a 
         leaf or not.

      Details:
         tree should exist, if it doesnt, bogus return value is given.
    */
   if ( t == NULL ) return -1;	// Might have to be changed...

   if ( t->nkids < 1 && t->token != NULL )
   {
      return 1;
   }
   
   return 0;
}

type_t* getIdentType( tree *t, scope_t* s )
{
   /* 
   Similar to getType, but only for identifiers, and works with different scope than yyscope.
    */
   if (t == NULL) return NULL;
   if ( isLeaf(t) && t->code == IDENT )
   {
      sym_t* symbol = findSymbol( s, t->token->text );
      return symbol->type;
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
   if ( yydebug )
   {
      // Print some information about current token
      fprintf(stderr, "Getting type of node type: %d\n", t->code);
      fprintf(stderr, "Getting type of token on line %d\n", getToken(t)->lineno);
   }
   type_t* p = NULL;

   if ( isLeaf(t) ) {
      // Type is a primitive type.
      switch( t->token->category )
      {
         case INT:
            p = I_Type;
            break;
         case DOUBLE:
            p = D_Type;
            break;
         case STRING:
            p = S_Type;
            break;
         case BOOL:
            p = B_Type;
            break;
         case LIST:
            p = calloc(1, sizeof(type_t));
            p = memcpy(p, L_Type, sizeof(type_t));
            break;
         case TABLE:
            p = calloc(1, sizeof(type_t));
            p = memcpy(p, T_Type, sizeof(type_t));
            break;
         case CLASS_NAME:
            p = calloc( 1, sizeof(type_t) );
            p->base_type = 8;
            sym_t* class = findSymbol( yyscope, t->token->text );
            if ( class == NULL ) {
               error(t, "Class type couldn't be found from the current scope!\n It has to exist though...");
            } 
            else
            {
               p->u.p = class->type;
            }
            break;
         case VOID:
            p = V_Type;
            break;
         case IDENT:
            p = checkSym(yyscope, t->token)->type;
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
         case 630:   // ListType: LIST < Type > 
            free(p);
            p = getType( t->kids[0] );
            // p->base_type = 5;
            // p->u.l.size = -1;
            p->u.l.elemtype = getType( t->kids[1] );
            break;
         case 635:   // TableType: TABLE < Type >
            p = getType(t->kids[0]);
            // p->base_type = 6;
            // p->u.t.size = -1;
            // p->u.t.index = calloc(1, sizeof(type_t));
            // p->u.t.index->base_type = 3;   // Default index type == string
            p->u.t.elemtype = getType( t->kids[1] );
            break;
         case 636: // TableType: TABLE < Type , Type >
            p = getType(t->kids[0]);
            // p->base_type = 6;
            // p->u.t.size = -1;
            p->u.t.index = getType(t->kids[1]);
            p->u.t.elemtype = getType(t->kids[2]);
            break;
         case 663: // ClassHeader: CLASS CLASS_NAME
            p->base_type = 9;
            p->u.s.label = strdup(t->kids[0]->token->text);
            break;
         case 697: // MethodHeader: Type MethodDeclarator
         case 698: // MethodHeader: VOID MethodDeclarator
            {
               tree* q = t->kids[1];   // q = MethodDeclarator: IDENT ( [FormalParameterList] )
               if ( q->nkids > 1 )
               {
                  free( p );
                  p = getType( q->kids[1] );
               }
               else
               {
                  p->base_type = 7;
                  p->u.f.nargs = 0;
               }
               // p->u.s.label = strdup(t->kids[1]->token->text);
               p->u.f.retType = getType( t->kids[0] );
            }
            break;
         case 715:   // ConstructorDeclarator: CLASS_NAME ( [FormalParamterList] )
         case 716: 
            // p->base_type = 7;    // symbol type function.
            if ( t->nkids > 1 )
            {
               // We have type list.
               free( p );
               p = getType( t->kids[1] );
            }
            else
            {
               p->base_type = 7;
            }
            p->u.f.retType = getType( t->kids[0] );
            break;
         case 729:   // FunctionPrototype: Type IDENT ( [TypeList] )
         case 730:   
         case 731:   
         case 732:   
            if ( t->nkids > 2 )
            {
               // We have type list.
               free( p );
               p = getType( t->kids[2] );
            }
            else
            {
               p->base_type = 7;
            }
            p->u.f.retType = getType( t->kids[0] );
            break;         
         case 741:   // FunctionDefinition: Type IDENT ( [FormalParameterList] ) FunctionBody
         case 742:   
         case 743:   
         case 744:   
            if ( t->nkids > 3 )
            {
               // We have type list.
               free( p );
               p = getType( t->kids[2] );
            }
            else
            {
               p->base_type = 7;
            }
            p->u.f.retType = getType( t->kids[0] );
            break;
         /* 
         Special cases - These dont process and return things that make intuitive sense.
            They return these kinds of things because they are used by other functions to 
            generate the proper types.
         */
         case 737:  // TypeList: TypeList , Type
         // case 879:  // ArgumentList: ArgumentList CM Expression
         case 1033: // FormalParameterList: FormalParameterList , FormalParameter
            // p = calloc(1, sizeof(type_t));
            p->base_type = 7; // Function type
            p->u.f.nargs = 0;
            int max = 5;
            p->u.f.argtype = calloc(max, sizeof(type_t*));
            tree *q = t;
            /* 
            This generates parameter lists that are backwards. Remember this when processing.
             */
            while (q->code == 1033 || q->code == 737)
            { // While we are parsing FormalParameterList
               p->u.f.argtype[(p->u.f.nargs)++] = getType(q->kids[1]);

               if (p->u.f.nargs == max)
               {
                  max += 5;
                  p->u.f.argtype = realloc(p->u.f.argtype, sizeof(type_t*) * max);
               }
               q = q->kids[0];
            }
            p->u.f.argtype[(p->u.f.nargs)++] = getType(q);
            if (p->u.f.nargs < max)
            {
               p->u.f.argtype = realloc(p->u.f.argtype, sizeof(type_t*) * (p->u.f.nargs));
            }
            p->u.f.argtype = reverseTypeList( p->u.f.argtype, p->u.f.nargs );
            // return p;
            break;
         case 1037: // FormalParameter: Type VariableDeclaratorId
            free(p);

            return getType(t->kids[0]);
            break;
         default:
            error(t, "Unrecognized non-terminal rule type.");
            break;
      }
   }

   if ( p == NULL ) error(t, "Couldn't get type from function getType()");
   return p;
}

void addParameterSymbols( tree*t, int bool_print )
{
   switch ( t->code ) {
      case 1033:   // FormalParamterList: FormalParameterList , FormalParameter
         addParameterSymbols( t->kids[0], bool_print );
         addParameterSymbols( t->kids[1], bool_print );
         break;
      case 1037:   // FormalParamter: Type VariableDeclaratorId
         {
            type_t* symType = getType( t->kids[0] );
            tok_t* T = getToken( t->kids[1] );
            sym_t* sym = check_addSym( yyscope, T, symType );
            if (sym == NULL)
               error(t, "Failed to add symbol while adding parameters to local symbol table.");
         }
         break;
   }
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
         case 618:   // VariableDeclaratorList: VariableDeclaratorList , VariableDeclaratorId
         case 673:   // ClassVariableList: ClassVariableList , IDENT
            ++count; 
            tok_t* T = getToken(p->kids[1]);
            // sym_t *s = scope_addSymbol(yyscope, T, varType);
            sym_t *s = check_addSym(yyscope, T, varType);
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
   // if ( scope_addSymbol( yyscope, T, varType ) == NULL)
   if ( check_addSym( yyscope, T, varType ) == NULL)
   {
      error(p, "Failed to add symbol while processing var declarator list.");
   }
   return count;
}



scope_t* checkQualified( tree* t )
{
   /* 
      Processes qualified names, requires first call to be a qualified name.
    */
   if ( t == NULL ) return NULL;
   if ( isLeaf(t) )
   {
      /* At the left most node in qualified names */
      return enterScope( yyscope, t->token->text );
   }
   else
   {
      /* This is so much garbage, i cant put into words less than a novel what this is trying to do */
      /* 
      Essentially, I am trying to grab the return values of functions called in a qualified name-style
         If the return value/member ( of the expression attempting to be qualified ) is not a class ( i.e. is not a qualifiable member ), return NULL from this function. If NULL is returned,
         and there is more "qualifying" to do. Then it will be caught and raise an error.
      This function returns NULL if it couldnt qualify the result of the function.
       */
      scope_t* n = checkQualified( t->kids[0]);
      if ( n == NULL ) 
      {
         // It's alright to be here, as long as we are at the bottom. i.e. theres an IDENT in kids[0].
         // What does this mean if there is? maybe instead of a class object, its a method invoc. or array access.
         if (t->kids[0]->code == IDENT)
         {
            // We need to check if we have a good situation
            // switch ( t->code )
            // {
            //    case :

            //       break;
            //    case :

            //       break;
            //    case :

            //       break;
            //    case :

            //       break;
            //    case :

            //       break;
            //    case :

            //       break;

            // }
         }
         error(t, "Failed to qualify name");
      }
      sym_t* f = findSymbol( n, t->kids[1]->token->text );
      /* Maybe add more debug info for this. Or just info at all. */
      if ( f == NULL ) 
      {
         fprintf(stderr, "Failed to find symbol %s on line %d\n", t->kids[1]->token->text, t->kids[1]->token->lineno);
         error(NULL, "");
      }
      type_t* r = f->type;

      switch ( t->code )
      {
         case 913:   /* ArrayAccess: PrimaryNoNewArray [ Expression ] */
         case 907:   /* MethodInvocation: Primary . IDENT ( [ArgumentList] ) */
         case 908:   /* MethodInvocation: Primary . IDENT ( [ArgumentList] ) */
            /* Find return value scope, and pass backwards. */
            r = f->type->u.f.retType;
         case 901:   /* FieldAccess: Primary . IDENT */
         case 1059:   /* QualifiedName: Name . IDENT */
            if ( r->base_type != 8 )
            {
               /* If return type is not a class object ( The only qualifiable type ),
                  return null. */
               return NULL;
            }
            /* WARNING WARNING WARNING: This part might be all sorts of crazy */
            /* Need to convert class objects into the names of the classes they represent */
            char* classname = r->u.p->u.s.label;   // gives the class name that r is an object of.
            return enterScope( n,  classname);  // Enters the scope of the given class name visible to scope n
            break;
         default:
            error(t, "DEBUG::: func processTree.c:checkQualified(): Unknown qualified code");
      }
   }
   return NULL;   // SHOULDNT EVER GET HERE.
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
      // yyscope = create_scope( "Global Scope", NULL );
      perror("Went up one too many scope levels.\nWent to parent scope of global...\n");
   }

   if ( isLeaf(t) )
   {
      if ( t->token->category == IDENT )
      {
         /* Check if IDENT has been declared */
         checkSym( yyscope, t->token );
      }
   }
   else
   {
      sym_t* sym = NULL;
      type_t* itype = NULL;
      tok_t* T = NULL;
      int maxlen = 0;
      char* str = NULL;
      tree* p = NULL;
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
         case 613:   // GlobalVariable
         case 677:   // ClassVariable
         case 1041:   // LocalVariable
            // For each possible symbol, add it to proper symbol table.
            // Get type of variable declarations.
            processVariableDeclarations( t, bool_print );
            break;
         /* Scope Changes */
         /* Class Declaration */
         case 659:   // ClassDeclaration: ClassHeader ClassBlock
            p = t->kids[0];   // p = ClassHeader: CLASS CLASS_NAME
            T = p->kids[1]->token;
            // Grab the token of tree CLASS
            itype = getType( p ); 
            /* Add symbol to the current scope */
            sym = scope_addSymbol( yyscope, T, itype );
            if ( sym == NULL ) 
               error(t, "Failed to add symbol to scope.");
            
            /* Make new scope */
            maxlen = strlen("Class Scope: ") + strlen(T->text) + 1;
            str = calloc ( maxlen, sizeof(char) );
            // sym->s.myScope = create_scope( sprintf("Class Scope: %s", T->text), yyscope );
            snprintf( str, maxlen, "Class Scope: %s", T->text);
            sym->s.myScope = newScope( t, str, yyscope );

            // change scope to new scope.
            yyscope = sym->s.myScope;
            // add scope to symbol type.
            sym->type->u.s.scope = sym->s.myScope;
            // Process rest of class.
            generateSymbolTables( t->kids[1], bool_print );
            // Revert to previous scope.
            yyscope = yyscope->parentScope;
            break;
         /* Method Declaration */
         case 693:   // MethodDeclaration: MethodHeader MethodBody
            itype = getType( t->kids[0] );
            T = getToken( t->kids[0]->kids[1] );
            // sym_t *sym = scope_addSymbol(yyscope, T, mType);
            sym = check_addSym(yyscope, T, itype);
            if ( sym == NULL ) 
               error(t, "Failed to add symbol to scope.");
            sym->s.myScope = create_scope( T->text, yyscope );
            yyscope = sym->s.myScope;
            // Add function parameters to local scope.
            /* 
            MethodHeader: Type MethodDeclarator
            MethodDeclarator: IDENT ( [FormalParameterList] )
            */
            tree* q = t->kids[0]->kids[1];
            if ( q->nkids > 1 )
            {
               addParameterSymbols( q->kids[1], bool_print );
            }
            // Generate Rest of symbols.
            generateSymbolTables( t->kids[1], bool_print );
            // Revert to previous scope.
            yyscope = yyscope->parentScope;
            break;
         /* Constructor Declaration */
         case 711:   //    ConstructorDeclaration: ConstructorDeclarator ConstructorBody
            // Does this symbol have to be defined in the class's scope?
            // We will go off of the assumption that it is in the class scope
            itype = getType( t->kids[0] );
            T = getToken( t->kids[0] );
            sym = check_addSym(yyscope, T, itype);

            // create new scope for class constructor.
            maxlen = strlen(T->text) + strlen(" constructor") + 1;
            str = calloc( maxlen, sizeof(char) );
            snprintf( str, maxlen, "%s constructor", T->text);
            sym->s.myScope = newScope( t, str, yyscope );

            /* change scope */
            yyscope = sym->s.myScope;
            /* Add parameters if they exist */
            if ( t->kids[0]->nkids > 1 )
            {
               addParameterSymbols( t->kids[0]->kids[1], bool_print );
            }
            /* Process rest of constructor body */
            generateSymbolTables( t->kids[1], bool_print );
            /* Revert to previous scope */
            yyscope = yyscope->parentScope;
            break;
         /* Function Prototypes. */
         case 729:
         case 730:
         case 731:
         case 732:
            itype = getType( t );
            T = getToken( t->kids[1] );
            sym = scope_addSymbol( yyscope, T, itype );
            break;
         /* Function Definition */
         case 741:   /* FunctionDefinition: Type IDENT LP [FormalParameterList] RP FunctionBody */
         case 742:
         case 743:
         case 744:
            // Add function symbol to parent scope
            // Check whether function was prototyped before.
            T = getToken(t->kids[1]);
            sym = findSymbol( yyscope, T->text );
            itype = getType(t);
            if ( sym != NULL )
            {
               // Another symbol was found, check if prototype.
               if ( sym->s.myScope != NULL )
               {
                  // not prototype
                  // Attempted second declaration
                  fprintf(stderr, "redefinition of symbol %s, first defined on line %d\n", T->text, sym->lineno);
                  error(t, "");
               }
               else
               {
                  // Symbol is prototype.
                  // Type check prototype types with definition types.
               }
            }
            else
            {
               // No prototype found.
               sym = scope_addSymbol(yyscope, T, itype);
            }

            // Add scope to symbol
            sym->s.myScope = newScope(t, T->text, yyscope);
            // Switch to new scope.
            yyscope = sym->s.myScope;
            // Add function params to new scope
            if ( t->nkids > 3 )
            {
               addParameterSymbols( t->kids[2], bool_print );
            }
            // Process function body in new scope.
            generateSymbolTables( t->kids[ t->nkids - 1 ], bool_print );   // Process last symbol in rule. i.e. "BODY"
            // Revert to previous scope.
            yyscope = yyscope->parentScope;
            break;
         /* Undeclared variable checking */
         case 893:   /* Primary: SHARP PrimaryNoNewArray */
            sym = NULL;
            if ( ( sym = findSymbol( yyscope, t->kids[0]->token->text ) ) == NULL )
            {
               // Undeclared variable usage.
               T = getToken( t->kids[0] );
               fprintf(stderr, "Undeclared variable usage.\n variable name %s was used on line %d before declaration was given.\n", T->text, T->lineno);
               error(t, "Undeclared variable.");
            }
            break;
         case 901:   /* FieldAccess: PrimaryNoNewArray . IDENT */
         case 907:   /* MethodInvocation: PrimaryNoNewArray . IDENT ( [ArgumentList] ) */
         case 908:   /* MethodInvocation: PrimaryNoNewArray . IDENT ( [ArgumentList] ) */
         case 1059:   /* QualifiedName: Name . IDENT */
            checkQualified(t);
            break;
         default:
            {
               // Process further trees
               int i = 0;
               for (i = 0; i < t->nkids; ++i)
               {
                  generateSymbolTables( t->kids[i], bool_print );
               }
            }
      }
   }
   return 0;
}

//////////////////////////////////////////// Type Checking Stuff ////////////////////////////////////
/* This stuff is very scary and needs to be managed well, since it can create memory leaks. */
/* Only use functions in this section from other functions in this section */

type_t *getReturnType(tree *t)
{
   /* 
   Very similar to the checkQualified, but returns the type instead.
    */
   if (t == NULL)
      return NULL;

   if (isLeaf(t))
   {
      // Do something, we "have to" check this anyway to be safe.
      return getType(t);
   }
   else
   {
      type_t *r = getReturnType(t->kids[0]);
      type_t *p = NULL;
      switch (t->code)
      {
      case 913: /* ArrayAccess: PrimaryNoNewArray [ Expression ] */
         // Check whether list, string, or table.
         switch (r->base_type)
         {
         case 3: // string
            // I guess the return value of this is a substring of length 1 (?)
            // Remember to check Expression for valid type.
            //    Valid types would be only int-castable types.
            r = S_Type;
            break;
         case 5: // list
            // Validate Expression is vaild int-castable thing.
            r = r->u.l.elemtype;
            break;
         case 6: // table
            // Validate expression is of type r->u.t.index type
            r = r->u.t.elemtype;
            break;
         default:
            error(t, "Type does not match list, string, or table for array access");
         }
         break;
      case 907: /* MethodInvocation: PrimaryNoNewArray . IDENT ( [ArgumentList] ) */
      case 908: /* MethodInvocation: PrimaryNoNewArray . IDENT ( [ArgumentList] ) */
         /* Find return value scope, and pass backwards. */
         // r = f->type->u.f.retType;

         // char *classname = r->u.p->u.s.label; // gives the class name that r is an object of.
         // return enterScope(n, classname);     // Enters the scope of the given class name visible to scope n

         scope_t *n = enterObjectScope(yyscope, r); // Enters the scope of the given class name visible to scope n
         if (n == NULL)
         {
            error(t, "Failed to find class scope from local scope... i must be confused with how this works\n");
         }
         sym_t *tmp = checkSym(n, getToken(t->kids[1]));

         if ( t->nkids > 2 )
         {
            // check argument lists

         }

         r = tmp->type->u.f.retType; // make type the returned type from function.
         // r = tmp->type;
      case 901:  /* FieldAccess: PrimaryNoNewArray . IDENT */
      case 1059: /* QualifiedName: Name . IDENT */
         scope_t *n = enterObjectScope(yyscope, r); // Enters the scope of the given class name visible to scope n
         if (n == NULL)
         {
            error(t, "Failed to find class scope from local scope... i must be confused with how this works\n");
         }
         sym_t *tmp = checkSym(n, getToken(t->kids[1]));
         // switch( t->code )
         // {
         //    case 907: /* MethodInvocation: PrimaryNoNewArray . IDENT ( [ArgumentList] ) */
         //    case 908: /* MethodInvocation: PrimaryNoNewArray . IDENT ( [ArgumentList] ) */
         //       r = tmp->type->u.f.retType; // make type the returned type from function.
         //       break;
         //    case 901:  /* FieldAccess: PrimaryNoNewArray . IDENT */
         //    case 1059: /* QualifiedName: Name . IDENT */
         //       r = tmp->type;
         //       break;
         // }
         r = tmp->type;
         // r = r->u.p; // Class type
         break;
      default:
         error(t, "DEBUG::: func processTree.c:checkQualified(): Unknown qualified code");
      }
      return r; // Final return value should be r.
      // Reason for this is because some expressions want to return the value retrieved from getReturnType()
      // call that originally initializes r.
      // Some examples, 893
   }
}

type_t* copyType(type_t* t)
{
   /* 
   Makes a copy of the given type and returns it.
    */
   type_t* temp = calloc( 1, sizeof(type_t) );
   temp = memcpy( temp, t, sizeof(type_t) );
   return temp;
}

type_t* checkTypes(tree *t)
{
   /* 
   Handles type checking for every single node in the tree. Doesn't generate symbols

   Details:
      This is where qualified checks come from and all that jazz.
      This way we separate that part from the generate symbol tables.

      The importance of this function is to separate out the generation of types to be stored,
      which is handled by getType() ( with very dangerous exceptions ), and to mainly create temporary
      type variables to compare to one another, return type for given tree node, and free all generated 
      types that are not needed for further checking.

      This is critical for ensuring we are cleaning up everything necessary.

      All calls to getType() should be done sparingly, with thorough investigation, and freeing of
      any generated memory.

      Things that need to be managed carefully whenever deleting them:
         class objects: we cant just call deleteType() on them, that will destroy the
            whole class it points to, the class object pointers must be free'd whenever 
            another type is being returned, or the return value will be ignored.
   */

   /* r is return value, p is temp type */
   type_t* r = NULL, *p = NULL;
   type_t* s1 = NULL, *s2 = NULL;

   switch( t->code )
   {
      /* Things that switch the scope */
      case 659:   // ClassDeclaration: ClassHeader ClassBlock
      case 741:   // FunctionDefinition:  Type IDENT ( [FormalParameterList] ) FunctionBody
      case 742:   // 
      case 743:   // 
      case 744:   // 
         {
            // Get symbol name
            char* n = getToken( t )->text;
            // Find symbol
            sym_t* s = findSymbol( yyscope, n );
            if ( s == NULL ) error(" Strange error, couldnt find reference to class.\n ");
            // store old scope, and find new scope
            scope_t* oldscope = yyscope, *newscope = getSymbolScope( yyscope, s );

            // Change to new scope
            yyscope = newscope;
            // check types of Block
            checkTypes( t->kids[ t->nkids - 1 ] );

            yyscope = oldscope;
         }
         break;
/* Process tree nodes and return their types */
/////////////////// Primary / PrimaryNoNewArray Handling ////////////////////////////
      case 893:   /* Primary: SHARP PrimaryNoNewArray */
         r = checkTypes( t->kids[0] );
         switch( r->base_type )
         {
            case 3:   // string
            case 5:   // list
            case 6:   // table
               // Size of operator should work, return integer type.
               return copyType(I_Type);
               break;
            default:
               fprintf( stderr, "semantic error on line %d:\n\tstatement did not return a type string, list, or table.\n", getToken(t)->lineno );
         }
         break;
      /* Literals taken care of below */
      case 893:   /* PrimaryNoNewArray: ( Expression ) */
         return checkTypes( t->kids[0] );
         break;
/////////////////// MethodInvocation + a few other things from Assignable /////////////////////
      case 879:  // ArgumentList: ArgumentList CM Expression
         {
            type_t* p = calloc(1, sizeof(type_t));
            p->base_type = 7; // Function type
            p->u.f.nargs = 0;
            int max = 5;
            p->u.f.argtype = calloc(max, sizeof(type_t*));
            tree *q = t;
            /* 
            This generates parameter lists that are backwards. Remember this when processing.
               */
            while (q->code == 879)
            { // While we are parsing FormalParameterList
               p->u.f.argtype[(p->u.f.nargs)++] = checkTypes(q->kids[1]);

               if (p->u.f.nargs == max)
               {
                  max += 5;
                  p->u.f.argtype = realloc(p->u.f.argtype, sizeof(type_t*) * max);
               }
               q = q->kids[0];
            }
            p->u.f.argtype[(p->u.f.nargs)++] = checkTypes(q);
            if (p->u.f.nargs < max)
            {
               p->u.f.argtype = realloc(p->u.f.argtype, sizeof(type_t*) * (p->u.f.nargs));
            }
            p->u.f.argtype = reverseTypeList(p->u.f.argtype, p->u.f.nargs);
            return p;
            break;
         }
      case 909:   /* MethodInvocation: CLASS_NAME ( ) */
         {
            // scope_t* s = enterScope( yyscope, getToken( t->kids[0] ) );
            r = getType( t->kids[0] );
            // idk anymore... just roll with it.

            // Since this created a type that points to an existing class type,
            // if we ever want to free r, we have to preserve the class type that
            // it is pointing to.
            // type_t* temp = calloc( 1, sizeof(type_t) );
            // temp = memcpy( temp, r->u.p, sizeof(type_t) );
            // r->u.p = temp;
            // Now r should be complete
            return r;
         }
         break;
      case 905:   /* MethodInvocation: Name ( ArgumentList ) */
      case 906:   /* MethodInvocation: Name (  ) */
         fprintf( stderr, "Turns out name is used instead of name.ident..." );
         exit( -2 );
         break;
      case 901:   /* FieldAccess: PrimaryNoNewArray . IDENT */
      case 1059:  /* QualifiedName: Name . IDENT */
      case 907:   /* MethodInvocation: PrimaryNoNewArray . IDENT ( [ArgumentList] ) */
      case 908:   /* MethodInvocation: PrimaryNoNewArray . IDENT (  ) */
         {
            type_t* newType = NULL; // Type to return at end of case statement.
            tree* first = t->kids[0];
            r = checkTypes( first );
            if ( r->base_type != 8 )
            {
               // return type is not a class object
               // mention last kid of t's first child
               error( first->kids[ first->nkids - 1 ], "cannot qualify non-class object types" );
            }
            scope_t* s = r->u.s.scope;
            sym_t *symbol = scope_check(s, t->kids[1]->token->text);
            if ( symbol == NULL )
            {
               error(t->kids[1], "Failed to find symbol in class scope");
            }

            // At end of this switch, free( r )
            switch( t->code )
            {
               case 901:   /* FieldAccess: PrimaryNoNewArray . IDENT */
               case 1059:  /* QualifiedName: Name . IDENT */
                  // free stuff and return type
                  // free( r );  // safe to free as long as checkTypes always returns copies of types.
                  newType = copyType( symbol->type );

                  break;
               case 907: /* MethodInvocation: PrimaryNoNewArray . IDENT ( [ArgumentList] ) */
               case 908: /* MethodInvocation: PrimaryNoNewArray . IDENT (  ) */
                  if ( t->nkids > 2 )
                  {
                     p = checkTypes( t->kids[2] );
                  }
                  else
                  {
                     // make standard function with no args or return
                     p = calloc( 1, sizeof(type_t) );
                     p->base_type = 7;
                  }

                  // Compare the types.
                  // free(p) after switch
                  /* 
                  returns of compareTypes()
                  10: not a function
                  11: incorrect number of arguments.
                  12: unexpected arg type ( differing argument types )
                   */
                  switch (compareTypes( symbol->type , p))
                  {
                     case 0: /* Same function calls */
                     case 13: /* Improper return types, this is fine because p cannot have return type */
                        newType = copyType(symbol->type->u.f.retType);
                        
                        break;
                     ///////////// SEMANTIC ERROR ///////////////////////
                     case 1: /* Unsimilar types */
                        // symbol is not a function type
                        error( t->kids[1], "symbol is not of type function" );
                        break;
                     case 11: /* incorrect arg numbers */
                        fprintf( stderr, "semantic error: invalid arg # for function call, 
                              on line %d\n\tfunction %s expects %d arguments, %d were given.\n", 
                              t->kids[1]->token->lineno, t->kids[1]->token->text, symbol->type->u.f.nargs, p->u.f.nargs);
                        exit(3);
                        break;
                     case 12: /* incorrect arg types */
                        error(t->kids[1], "unexpected arg type");
                        /* FIX */
                        /* add detailed information on what type was expected */
                        break;
                     //////////// ERROR CONDITION //////////////
                     case 10: /* p is not a function, but symbol->type is */
                        /* 
                        This should really never happen, and if it does, something very strange occurred.
                        */
                     default:
                        fprintf( stderr, "FATAL ERROR: THIS SHOULD NEVER HAPPEN\nprocessTree.c, checkTypes(): case 908\n" );
                        exit( -5 );
                        break;
                  }
                  free( p );
                  break;
               default:
                  error(NULL, "this isnt possible");
                  exit(-1);
            }
            
            free(r); // free copy of previous type checked
            return newType;
         }
         break;
/////////////////// Assignable //////////////////////////
      case 913: /* ArrayAccess: PrimaryNoNewArray [ Expression ] */
         {
            r = checkTypes( t->kids[0] );
            type_t* newType = NULL;
            // Check whether list, string, or table.
            switch (r->base_type)
            {
               case 3: // string
                  // LOOK
                  // I guess the return value of this is a substring of length 1 (?)
                  // Remember to check Expression for valid type.
                  //    Valid types would be only int-castable types.
                  
                  // Need to make copy of type instead.
                  newType = copyType(S_Type);
                  break;
               case 5: // list
                  // Validate Expression is vaild int-castable thing.
                  newType = copyType(r->u.l.elemtype);
                  break;
               case 6: // table
                  // Validate expression is of type r->u.t.index type
                  newType = copyType(r->u.t.elemtype);
                  break;
               default:
                  error(t, "Type does not match list, string, or table for array access");
            }
            // validate expression is of proper type
            p = checkTypes( t->kids[1] );
            if ( r->base_type == 6 && compareTypes( r->u.t.index, p ) )
            {  // if r is a table, and expression is not the same type as r's index type
               /* FIX : consider adding more detail to error message */
               fprintf(stderr, "semantic error: index does not match table index type, on line %d\n", t->kids[0]->token->lineno);
               exit(3);
            }
            else if ( compareTypes( I_Type, p ) )
            {  // or, if expression is not a integer for indexing
               error(t->kids[1], "failed to perform index with non-integer type");
            }

            free(r);
            free(p);
            return newType;
         }
         break;
      case 917:   /* DefaultTableMapping: PrimaryNoNewArray [ ] */
         r = checkTypes( t->kids[0] );
         
         if ( r->base_type != 6 )
         {
            error( t, "no default mapping for non-table type" );
         }
         p = copyType(r->u.t.elemtype);

         free(r);
         return p;
////////////////// Final parts of PrimaryNoNewArray ////////////////////////////////////
      case 897:   /* PrimaryNoNewArray: PrimaryNoNewArray [ Expression : Expression ] */
         /* List/string substring */
         {
            r = checkTypes( t->kids[0] );

            if ( r->base_type != 3 && r->base_type != 5 )
            {  // r is not string or list
               error(t, "attempting to substring non-string, non-list type");
            }
            type_t* e1 = checkTypes(t->kids[1]), *e2 = checkTypes(t->kids[2]);

            if ( e1->base_type != I_type )
               error(t->kids[1], "expression 1 of substring statement is not of valid index type");
            if (e2->base_type != I_type)
               error(t->kids[2], "expression 2 of substring statement is not of valid index type");

            p = ( r->base_type == 3 ) ? copyType( S_Type ) : copyType( r->u.l.elemtype );

            free(r);
            free(e1);
            free(e2);
            return p;
         }
         break;

/////////////////////////// OPERATORS THAT RETURN OPERAND TYPES /////////////////////////////////////////
      case 998:   /* Assignment: Assignable AssignmentOperator AssignmentExpression */
      case 938:   /* MultplicativeExpressions: MUL */
      case 939:   /* DIV */
      case 940:   /* MOD */
      case 941:   /* DROLL */
      case 964:   /* AdditiveExpression: PLUS */
      case 965:   /* MINUS */
      case 984:   /* ANDAND */   // These get to be here because we are placing the restriction on these operators to require boolean values.
      case 989:   /* OROR */
         {
            /* 
            Operators that are explicitly defined:
            strings:    ==    !=
            lists:      +=    
            tables:     -=

            NOTE: lists, tables, and classes must be allowed to be assigned 'null'
             */
            s1 = checkTypes(t->kids[0]);
            s2 = checkTypes(t->kids[2]);
            int o = t->kids[1]->code;

            if ( check_operator( o, s1, s2 ) )
            {
               error(t, "operation not defined for given type");
            }

            free(s2);
            return s1;
         }
         break;
////// Unary operators  //////////////
      case 926:   /* UNARY MINUS */
         s1 = checkTypes(t->kids[0]);
         if ( s1->base_type != 1 && s2->base_type != 2 ) error(t, "Unary minus requires a numeric type");   /* FIX : better error message */
         return s1;
      case 933:   /* Unary DROLL */
         s1 = checkTypes(t->kids[0]);
         if ( s1->base_type != 1 ) error(t, "Unary Dice roll requires integer expression");
         return s1;
//////////////////////////////// OPERATORS THAT RETURN BOOLEAN VALUES ///////////////////////////////////
      case 932:   /* BANG */
      case 970:   /* LT */
      case 971:   /* LE */
      case 972:   /* GT */
      case 973:   /* GE */
      case 978:   /* EQ */
      case 979:   /* NE */
         s1 = checkTypes(t->kids[0]);
         s2 = checkTypes(t->kids[2]);

         if (check_operator(o, s1, s2))
         {
            error(t, "operation not defined for given type");  /* FIX : better error message */
         }

         free(s2);
         return s1;
      /* SwapExpression */
      // case 998:    /* Assignment: Assignable AssignmentOperator AssignmentExpression */
      case 1004:   /* SwapExpression: Assignable SWAP Assignable */
      case 1005:   /* SwapExpression: Assignable SWAP Assignment */
      case 1006:   /* SwapExpression: Assignable SWAP SwapExpression */
         {
            s1 = checkTypes(t->kids[0]);
            s2 = checkTypes(t->kids[1]);

            if ( compareTypes( s1, s2 ) )
            {
               /* FIX : Add more detailed information like the types that werent matched */
               error(t, "invalid swap on incompatible types");
            }

            free(s2);
            return s1;
         }
         break;
/////////////////////////// UNCATEGORIZED THINGS ///////////////////////////////////
      case 888:   /* ListInializer: ListInializer CM PrimaryNoNewArray */
         /* We want the first item in the list to determine the type for the whole list,
            So we want the rule to be left-recursive, not right. This is because we go
            all the way down on the first node, then compare the results with the right node*/
         {
            type_t *s1 = checkTypes(t->kids[0]), *s2 = checkTypes(t->kids[1]);

            if ( compareTypes( s1, s2 ) )
            {
               /* FIX : add more detailed error reporting */
               fprintf( stderr, "semantic error: list literal contains multiple non-comparable types, on line %d\n", getToken(t)->lineno );
               exit(3);
               // char* t1 = NULL, t2 = NULL;
               // switch (s1)
               // fprintf( stderr,  )
            }

            // free(s1);
            free(s2);
            return s1;
         }
         // r = checkTypes( t->kids[0] );
         break;
      case 1572:  /* ListLiteral: [ ListInitializer ] */
         {
            /* 
            EXTREME CAUTION: 
            Because checkTypes returns a pointer to a block of memory not referenced elsewhere,
            we are always assuming we have to free the piece of memory we return, this causes a lot of problems when we
            have to get back the type of the list elements, because we need to get the pointer back and only have to free
            the whole list type when we return from this function, that will leave the list element type unfreed, and w/o
            a reference to free it.

            So, we made the list of unfreed pointers to circumvent the issue, and track 
             */
            r = copyType(L_Type);
            p = checkTypes(t->kids[0]);
            r->u.l.elemtype = p;

            // Store the pointer in unfreedPointers for free'ing later.
            store(p);

            return r;   // return the list
         }
         break;
      case :

         break;
///////////// BASIC TYPES ////////////////////
      case INTLITERAL:
         return copyType( I_Type );
         break;
      case FLOATLITERAL:
         return copyType( D_Type );
         break;
      case BoolLiteral:
         return copyType( B_Type );
         break;
      case STRINGLITERAL:
      case CHARLITERAL:       /* LOOK: Not even sure if this is supposed to work */
         return copyType( S_Type );
         break;
      case NULLLITERAL:
         return copyType( V_Type ); /* LOOK : Going to be using void type to represent null... not sure how this is gonna work. */
         break;
      case IDENT:
         r = getType( t );
         // get type gives us reference to the type, but this function should only return
         // copies of references to types, so that we can safely free any memory we make
         // in this function
         // make p a copy of r
         p = copyType( r );
         // return the copy
         return p;
         break;
      default:
         // This means that the rest of the tree should be processed, but we dont care about
         // comparing or returning their types.
      {
         int i;
         if (yydebug)
         {
            printf(" checkTypes, default rule reached for tree code %d, with name %s ", t->code, t->label);
         }
         for ( i = 0; i < t->nkids; ++i )
         {
            checkTypes( t->kids[i] );
         }
      }
   }


   // If t does not have a type, return NULL by default
   return NULL;
}


void initSemanticCheck()
{
   /* 
   Initializes things before doing semantic check.
    */
   // Base Types
   // V_Type, I_Type, D_Type, S_Type, B_Type, L_Type, T_Type;
   V_Type = calloc(1, sizeof(type_t));
   V_Type->base_type = -1;

   I_Type = calloc(1, sizeof(type_t));
   I_Type->base_type = 1;

   D_Type = calloc(1, sizeof(type_t));
   D_Type->base_type = 2;
   
   S_Type = calloc(1, sizeof(type_t));
   S_Type->base_type = 3;
   
   B_Type = calloc(1, sizeof(type_t));
   B_Type->base_type = 4;
   
   L_Type = calloc(1, sizeof(type_t));
   L_Type->base_type = 5;
   L_Type->u.l.size = -1;
   L_Type->u.l.elemtype = I_Type;


   T_Type = calloc(1, sizeof(type_t));
   T_Type->base_type = 6;
   T_Type->u.t.size = -1;

   T_Type->u.t.index = S_Type;
   T_Type->u.t.elemtype = I_Type;

   /* initialize unfreed pointers */
   unfreedPointers = calloc( 5, sizeof( type_t* ) );
}

int semanticCheck(tree *t, int p)
{
   /* 
      Does Complete semantic analysis in several stages.
    */

   print = p;

   initSemanticCheck();
   // Make Global scope
   yyscope = create_scope("Global Scope", NULL);

   tok_t T;
   T.category = IDENT;
   T.lineno = -1;
   T.filename = "";

   // Need to add read() and write() to the symbol tables.
   {
      // string read();
      type_t* n = calloc( 1, sizeof( type_t ) );
      n->base_type = 7;
      type_t *stringtype = calloc(1, sizeof(type_t));
      stringtype->base_type = 3;
      n->u.f.retType = stringtype;
      
      tok_t* read = calloc(1, sizeof(tok_t));
      memcpy( read, &T, sizeof(tok_t) );
      read->text = "read";
      scope_addSymbol( yyscope, read, n );
   }

   {
      // void write(string);
      type_t *n = calloc(1, sizeof(type_t));
      n->base_type = 7;
      type_t *stringtype = calloc(1, sizeof(type_t));
      stringtype->base_type = 3;

      type_t *voidtype = calloc(1, sizeof(type_t));
      voidtype->base_type = -1;

      n->u.f.nargs = 1;
      n->u.f.argtype = calloc( 1, sizeof( type_t* ) );
      n->u.f.argtype[0] = stringtype;

      n->u.f.retType = voidtype;

      tok_t *write = calloc(1, sizeof(tok_t));
      memcpy(write, &T, sizeof(tok_t));
      write->text = "write";
      scope_addSymbol(yyscope, write, n);
   }

   generateSymbolTables( t, p );
   // checkUndeclaredSymbols( t, print );
   checkTypes( t );

   /* Free unfreed pointers */
   int i = 0;
   for( i = 0; i < unfreed; ++i )
   {
      free( unfreedPointers[i] );
   }
   free(unfreedPointers);
   return 0;
}