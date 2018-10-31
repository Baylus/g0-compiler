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
#include <string.h>  //memcpy

int isLeaf(tree *t);

// Consider making these examples of the basic types if it helps later.
// Doing this would make clean up severely more troublesome.
type_t *V_Type, *I_Type, *D_Type, *S_Type, *B_Type, *L_Type, *T_Type;

extern scope_t* yyscope;   // g0lex.l

extern int yydebug;     // g0gram.y/g0gram.h

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

   fprintf(stderr, "Semantic Error:\n %s\n", s);

   exit(3);
}

void checkSym( scope_t* s, tok_t* t )
{
   /* 
      Reports an error if symbol does not exist in the current context.
    */
   if ( findSymbol(s, t->text) == NULL ) {
      /* Symbol undefined */
      fprintf(stderr, "Undeclared symbol %s on line %d\n", t->text, t->lineno );
      error( NULL, "Undeclared reference to symbol");
   }
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
   scope_t* tempScope = NULL;
   tempScope = getSymbolScope( s, sym );
   // if ( s == NULL )
   // {
   //    /* Symbol did not have a scope. */
   //    fprintf(stderr, "Symbol %s\n", sName);
   //    error("Symbol provided did not have a scope to enter!");
   // }
   return tempScope;
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
//    if ( t == NULL ) return 0;
   if ( t->nkids < 1 && t->token != NULL )
   {
      return 1;
   }
   
   return 0;
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
            p->u.s.label = strdup(t->kids[1]->token->text);
            break;
         case 697: // MethodHeader: Type MethodDeclarator
         case 698: // MethodHeader: VOID MethodDeclarator
            {
               tree* q = t->kids[1];   // q = MethodDeclarator: IDENT ( [FormalParameterList] )
               if ( q->nkids > 1 )
               {
                  free( p );
                  p = getType( t->kids[1] );
               }
               else
               {
                  p->base_type = 7;
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
         case 741:   // FunctionDefinition: Type IDENT ( [FormalParameterList] )
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
         case 1033: // FormalParameterList: FormalParameterList , FormalParameter
            // p = calloc(1, sizeof(type_t));
            p->base_type = 7; // Function type
            p->u.f.nargs = 0;
            int max = 5;
            p->u.f.argtype = calloc(max, sizeof(type_t));
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
                  p->u.f.argtype = realloc(p->u.f.argtype, sizeof(type_t) * max);
               }
               q = q->kids[0];
            }
            p->u.f.argtype[(p->u.f.nargs)++] = getType(q);
            if (p->u.f.nargs < max)
            {
               p->u.f.argtype = realloc(p->u.f.argtype, sizeof(type_t) * (p->u.f.nargs));
            }
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
      if ( n == NULL ) error(t, "Failed to qualify name");
      
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
         case 896:   /* PrimaryNoNewArray: SHARP IDENT */
            sym = NULL;
            if ( ( sym = findSymbol( yyscope, t->kids[0]->token->text ) ) == NULL )
            {
               // Undeclared variable usage.
               T = getToken( t->kids[0] );
               fprintf(stderr, "Undeclared variable usage.\n variable name %s was used on line %d before declaration was given.\n", T->text, T->lineno);
               error(t, "Undeclared variable.");
            }
            break;
         case 901:   /* FieldAccess: Primary . IDENT */
         case 907:   /* MethodInvocation: Primary . IDENT ( [ArgumentList] ) */
         case 908:   /* MethodInvocation: Primary . IDENT ( [ArgumentList] ) */
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

// int checkUndeclaredSymbols(tree *t, int print)
// {
//    /* 
//       Checks the entire tree to ensure that no symbols are used but not declared.

//       Details:
//          Processes every node in the tree, excluding variable declarations, function parameters,
//          and several others, and checks that the symbols they use are defined in the proper scope.
//     */

//    if ( !isLeaf(t) )
//    {
//       /* not Leaf node of tree */
//       switch( t->code )
//       {
//          /* Ignore these things category */
//          case 677:   /* ClassVariable */
//          case 613:   /* GlobalVariable */
//          case 1041:   /* LocalVariable */
//          case 677:   /*  */
//          case 677:   /*  */
//          case 677:   /*  */
//          case 677:   /*  */
//          case 677:   /*  */
//          case 677:   /*  */
//          case 677:   /*  */
//          case 677:   /*  */

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//          case :

//             break;
//       }
//    }

// }

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
}

int semanticCheck(tree *t, int print)
{
   /* 
      Does Complete semantic analysis in several stages.
    */

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

   generateSymbolTables( t, print );
   // checkUndeclaredSymbols( t, print );
   return 0;
}