/*
 * g0 Grammar
 * Derived from the Godiva Grammar, which was in turn
 * based on the Java grammar in Gosling/Joy/Steele, Chapter 19
 */

%{
#include <stdio.h>
#include "tree.h"
%}

/*
 * %union declares of what kinds of values appear on the value stack
 */
%union {
   tree* node;
   }

/*
 * each token is declared.  tokens store leaf values on the value stack
 *
 * Back in javalex.l, we put things on the stack by assigning to yylval

 "if"           { yylval.node = alcnode(IF, 0); return IF; }

 *
 */
%token < node > BOOLEAN BREAK CLASS
%token < node > DOUBLE ELSE FOR
%token < node > IF INT
%token < node > RETURN
%token < node > THIS
%token < node > VOID WHILE IDENT CONTINUE
%token < node > NULLLITERAL BOOLLITERAL INTLITERAL
%token < node > CHARLITERAL FLOATLITERAL STRINGLITERAL
%token < node > LP RP LC RC LB RB SM CM DOT ASN LT GT BANG TILDE QUEST COLON
%token < node > EQ NE LE GE ANDAND OROR PLUS MINUS MUL DIV AND OR CARET
%token < node > MOD SHL SHR LSHR PLASN MIASN MUASN DIASN  ORASN CARETASN
%token < node > SLASN SRASN LSRASN MODASN BAD_TOKEN

/*
 * each nonterminal is declared.  nonterminals correspond to internal nodes
 */
%type < node > Program GlobalVariables GlobalVariable
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > 


/*
 * the start symbol, Goal, may seem to be here for rhetorical purposes,
 * but it is also the ideal spot to insert a semantic action that passes
 * the completed parse tree to a later phase of compilation.

ProductionRule:
        
      | 
      ;

 */
%start Program

%%



Program:
        GlobalVariables Functions
      | Epsilon
      ;

Semicolon:
        SEMICOLON
      | Epsilon
      ;

Epsilon: { $$ = NULL; };

	  
	  
GlobalVariables:
        GlobalVariable
      | GlobalVariable GlobalVariables
      | Epsilon
      ;

GlobalVariable:
        VariableDeclaration 
      | T
      ;

VariableDeclaration:
        Type SimpleName Semicolon
      | 
      ;

Name:
		  SimpleName
		| QualifiedName
		;

SimpleName:
		  IDENT
		;

QualifiedName:
		  Name DOT IDENT
		;


Type:
        ClassType
      | ArrayType
      | PrimitiveType
      | { $$ = NULL; printf("Error: Type name expected."); }
      ;

PrimitiveType:
        INT
      | BOOLEAN
      | 
      | 
      | 
      ;

ArrayType:
        
      | 
      ;

ClassType:
        
      | 
      ;

ProductionRule:
        
      | 
      ;






Semicolon:
        SM
      | Epsilon
      ;

Epsilon: { $$ = NULL; };


