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
%token < node > BOOLEAN BREAK CLASS STRING
%token < node > DOUBLE ELSE FOR
%token < node > IF INT RETURN
%token < node > TABLE LIST
%token < node > THIS TRUE FALSE
%token < node > VOID WHILE IDENT CONTINUE
%token < node > NULLLITERAL INTLITERAL
%token < node > CHARLITERAL FLOATLITERAL STRINGLITERAL
%token < node > LP RP LC RC LB RB SM CM DOT ASN LT GT BANG
%token < node > EQ NE LE GE ANDAND OROR PLUS MINUS MUL DIV AND OR
%token < node > MOD PLASN MIASN SWAP
%token < node >     BAD_TOKEN

/*
 * each nonterminal is declared.  nonterminals correspond to internal nodes
 */
%type < node > Program GlobalVariablesOpt GlobalVariables GlobalVariable VariableDeclaratorList
%type < node > ClassDeclaration CompilationUnitsOpt CompilationUnits CompilationUnit
%type < node > Function FunctionPrototype FunctionDefinition FunctionHeader AssignmentStatement
%type < node > FunctionBody Name SimpleName QualifiedName
%type < node > ClassHeader ClassBlock ClassVariables ClassVariable ClassBlockUnitList ClassBlockUnit
%type < node > MethodDeclaration ConstructorDeclaration ConstructorDeclarator ConstructorBody
%type < node > VariableDeclaratorId
%type < node > TypeListOpt TypeList
%type < node > Block BlockStatementListOpt BlockStatementList BlockStatement Statement
%type < node > NoLocalVariableBlock NoLocalVariableBlockStatementListOpt NoLocalVariableBlockStatementList NoLocalVariableBlockStatement Statement
%type < node > StatementNoShortIf StatementWithoutTrailingSubstatement ExpressionStatement
%type < node > StatementExpression IfThenStatement IfThenElseStatement IfThenElseStatementNoShortIf
%type < node > WhileStatement WhileStatementNoShortIf ForInitOpt ForInit ExpressionOpt ForUpdateOpt
%type < node > ForUpdate ForStatement ForStatementNoShortIf StatementExpressionList
%type < node > EmptyStatement BreakStatement ReturnStatement 
%type < node > Expression AssignmentOperator Assignable Assignment ConditionalOrExpression 
%type < node > EqualityExpression RelationalExpression AdditiveExpression MultiplicativeExpression 
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > 
%type < node > LocalVariablesOpt LocalVariables LocalVariable
%type < node > FormalParameterListOpt FormalParameterList FormalParameter
%type < node > BoolLiteral Semicolon Epsilon
%type < node > Type PrimitiveType ClassType ArrayType ListType TableType

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
        CompilationUnitsOpt
      | Epsilon
      ;

GlobalVariablesOpt:
        GlobalVariables
      | Epsilon
      ;

GlobalVariables:
        GlobalVariable
      | GlobalVariables GlobalVariable
      ;

GlobalVariable:
        Type VariableDeclaratorList Semicolon
      ;

VariableDeclaratorList:
        VariableDeclaratorId 
      | VariableDeclaratorList CM VariableDeclaratorId
      ;

Type:
        ClassType
      | ArrayType
      | PrimitiveType
      | ListType
      | TableType
      | { $$ = NULL; printf("Error: Type name expected."); }
      ;

ListType:
        LIST
      | LIST LT PrimitiveType GT
      ;

TableType:
        TABLE
      | TABLE LT PrimitiveType GT
      | TABLE LT PrimitiveType CM PrimitiveType GT
      ;

PrimitiveType:
        INT
      | BOOLEAN
      | DOUBLE
      | STRING
      ;

ArrayType:
        PrimitiveType LB RB
      | Name LB RB
      | ArrayType LB RB
      ;

ClassType:
        IDENT
      ;

CompilationUnitsOpt:
        CompilationUnits
      | Epsilon
      ;

CompilationUnits:
        CompilationUnit
      | CompilationUnits CompilationUnit
      ;

CompilationUnit:
        ClassDeclaration
      | Function
      | AssignmentStatement
      | GlobalVariable
      ;

ClassDeclaration:
        ClassHeader ClassBlock
      ;

ClassHeader:
        CLASS IDENT
      ;

ClassBlock:
        LC ClassBlockUnitListOpt RC
      ;

ClassVariablesOpt:
        ClassVariables
      | Epsilon
      ;

ClassVariables:
        ClassVariable
      | ClassVariables ClassVariable
      ;

ClassVariable:
        Type IDENT Semicolon
      ;

ClassBlockUnitListOpt:
        ClassBlockUnitList
      | Epsilon
      ;

ClassBlockUnitList:
        ClassBlockUnit
      | ClassBlockUnitList ClassBlockUnit
      ;

ClassBlockUnit:
        ClassVariable
      | MethodDeclaration
      | ConstructorDeclaration
      ;

MethodDeclaration:
        MethodHeader MethodBody
      ;

MethodHeader:
        Type MethodDeclarator
      | VOID MethodDeclarator
      ;

MethodDeclarator:
        IDENT LP FormalParameterListOpt RP
      ;

MethodBody:
        Block
      ;

ConstructorDeclaration:
         ConstructorDeclarator ConstructorBody
      ;

ConstructorDeclarator:
        IDENT LP FormalParameterListOpt RP
      ;

ConstructorBody:
        Block
      ;

Function:
        FunctionPrototype
      | FunctionDefinition
      ;

FunctionPrototype:
        FunctionHeader LP TypeListOpt RP Semicolon
      ;

TypeListOpt:
        TypeList
      | { $$ = NULL; }
      ;

TypeList:
        Type
      | TypeList CM Type
      ;

FunctionHeader:
        Type IDENT
      | VOID IDENT
      ;

FunctionDefinition:
        FunctionHeader LP FormalParameterListOpt RP FunctionBody
      ;

FunctionBody:
        Block
      ;

Block:
        LC BlockStatementListOpt RC 
      ;

BlockStatementListOpt:
        BlockStatementList
      | { $$ = NULL; }
      ;

BlockStatementList:
        BlockStatement
      | BlockStatementList BlockStatement
      ;

BlockStatement:
        LocalVariable
      | Statement
      ;

NoLocalVariableBlock:
        LC NoLocalVariableBlockStatementListOpt RC 
      ;

NoLocalVariableBlockStatementListOpt:
        NoLocalVariableBlockStatementList
      | { $$ = NULL; }
      ;

NoLocalVariableBlockStatementList:
        NoLocalVariableBlockStatement
      | NoLocalVariableBlockStatementList NoLocalVariableBlockStatement
      ;

NoLocalVariableBlockStatement:
        Statement
      ;

Statement:
        StatementWithoutTrailingSubstatement
      | IfThenStatement
      | IfThenElseStatement
      | WhileStatement
      | ForStatement
      ;

StatementNoShortIf:
        StatementWithoutTrailingSubstatement
      | IfThenElseStatementNoShortIf
      | WhileStatementNoShortIf
      | ForStatementNoShortIf
      ;

StatementWithoutTrailingSubstatement:
        NoLocalVariableBlock
      | EmptyStatement
      | ExpressionStatement
      | BreakStatement
      | ReturnStatement
      ;

ExpressionStatement:
        StatementExpression Semicolon
      ;

StatementExpression:
        Assignment
      | MethodInvocation
      ;

IfThenStatement:
        IF LP Expression RP Statement
      ;

IfThenElseStatement:
        IF LP Expression RP StatementNoShortIf 
            ELSE Statement
      ;

IfThenElseStatementNoShortIf:
        IF LP Expression RP StatementNoShortIf
            ELSE StatementNoShortIf
      ;

WhileStatement:
        WHILE LP Expression RP Statement
      ;

WhileStatementNoShortIf:
        WHILE LP Expression RP StatementNoShortIf
      ;

ForInitOpt:
        ForInit 
      | { $$ = NULL; } 
      ;

ForInit:
        StatementExpressionList
      ;

ExpressionOpt:
        Expression 
      | { $$ = NULL; } 
      ;

ForUpdateOpt:
        ForUpdate 
      | { $$ = NULL; } 
      ;

ForUpdate:
        StatementExpressionList
      ;

ForStatement:
        FOR LP ForInitOpt SM ExpressionOpt SM ForUpdateOpt RP
            Statement
      ;

ForStatementNoShortIf:
        FOR LP ForInitOpt SM ExpressionOpt SM ForUpdateOpt RP
            StatementNoShortIf
      ;

StatementExpressionList:
        StatementExpression
      | StatementExpressionList CM StatementExpression
      ;

EmptyStatement:
        Semicolon
      ;

BreakStatement:
        BREAK Semicolon
      ;

ReturnStatement:
        RETURN ExpressionOpt Semicolon
      ;
_________________________________________________________________________________________

DimsOpt: 
	  Dims 
	| { $$ = NULL; } 
	;

Dims:		  
	  LB RB
	| Dims LB RB
	;

ArgumentListOpt:
	  ArgumentList 
	| Epsilon ;

ArgumentList:
	  Expression
	| ArgumentList CM Expression
	;

Primary:
     PrimaryNoNewArray
   ;

PrimaryNoNewArray:
     Literal
   | LP Expression RP
   | FieldAccess
   | MethodInvocation
   | ArrayAccess
   ;

FieldAccess:
     Primary DOT IDENT
   ;

MethodInvocation:
     Name LP ArgumentListOpt RP
   | Primary DOT IDENT LP ArgumentListOpt RP
   ;

ArrayAccess:
     Name LB Expression RB
   | PrimaryNoNewArray LB Expression RB
   ;

PostFixExpression:
     Primary
   | Name
   ;

UnaryExpression:
     MINUS UnaryExpression
   | UnaryExpressionNotPlusMinus
   ;

UnaryExpressionNotPlusMinus:
     PostFixExpression
   | BANG UnaryExpression
   | DROLL UnaryExpression
   ;

CastExpression:
     LP PrimitiveType DimsOpt RP UnaryExpression
   | LP Expression RP UnaryExpressionNotPlusMinus
   | LP Name Dims RP UnaryExpressionNotPlusMinus
   ;

MultiplicativeExpression:
     UnaryExpression
   | MultiplicativeExpression MUL UnaryExpression
   | MultiplicativeExpression DIV UnaryExpression
   | MultiplicativeExpression MOD UnaryExpression
   | MultiplicativeExpression DROLL UnaryExpression
   ;

AdditiveExpression:
     MultiplicativeExpression
   | AdditiveExpression PLUS MultiplicativeExpression
   | AdditiveExpression MINUS MultiplicativeExpression
   ;

RelationalExpression:
     AdditiveExpression
   | RelationalExpression LT AdditiveExpression
   | RelationalExpression LE AdditiveExpression
   | RelationalExpression GT AdditiveExpression
   | RelationalExpression GE AdditiveExpression
   ;

EqualityExpression:
     RelationalExpression
   | EqualityExpression EQ RelationalExpression
   | EqualityExpression NE RelationalExpression
   ;

ConditionalAndExpression:
     EqualityExpression
   | ConditionalAndExpression ANDAND EqualityExpression
   ;

ConditionalOrExpression:
     ConditionalAndExpression
   | ConditionalOrExpression OROR ConditionalAndExpression
   ;

AssignmentExpression:
     ConditionalOrExpression
   | Assignment
   ;

Assignment:
     Assignable AssignmentOperator ConditionalOrExpression
   ;

Assignable:
     Name
   | FieldAccess
   | ArrayAccess
   ;

AssignmentOperator:
     ASN
   | PLASN
   | MIASN
   ;

Expression:
       AssignmentExpression
   ;


___________________________________________________________________________________________

ProductionRule:
        
      | 
      ;
		
FormalParameterListOpt:
        FormalParameterList
      | Epsilon
      ;

FormalParameterList:
        FormalParameter
      | FormalParameterList CM FormalParameter
      ;

FormalParameter:
        Type VariableDeclaratorId
      ;

LocalVariablesOpt:
        LocalVariables
      | Epsilon
      ;

LocalVariables:
        LocalVariable
      | LocalVariables LocalVariable
      ;

LocalVariable:
        Type VariableDeclaratorList Semicolon
      ;

VariableDeclaratorId:
        IDENT
      | VariableDeclaratorId LB RB
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


BoolLiteral:
        TRUE
      | FALSE
      | { $$ = NULL; printf("ERROR: Expected BoolLiteral"); }
      ;

Semicolon:
        SM
      | Epsilon
      ;

Epsilon: { $$ = NULL; };


/*
 * the start symbol, Goal, may seem to be here for rhetorical purposes,
 * but it is also the ideal spot to insert a semantic action that passes
 * the completed parse tree to a later phase of compilation.

ProductionRule:
        
      | 
      ;

 */