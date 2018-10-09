%{
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "token.h"
#include "cntable.h"

extern char *yyfilename;
extern int yylineno;
extern int yylex(void);	/* call this function once for each token */
void yyerror(char const*);
%}

%union {
struct token *tok;
}

/* terminal tokens declared*/
%token BREAK DROLL DOUBLE ELSE FOR IF INT RETURN VOID WHILE
%token <tok> IDENTIFIER
%token CLASSNAME CLASS LIST TABLE PERIOD STRING BOOL
%token INTLIT DOUBLELIT STRINGLIT BOOLFALSE BOOLTRUE NULLVAL
%token LEFTPAREN RIGHTPAREN LEFTSUBSCRIPT RIGHTSUBSCRIPT LEFTBRACE RIGHTBRACE
%token SEMICOLON COLON COMMA LOGICALNOT SIZE MULTIPLY DIVIDE MODULUS PLUS MINUS
%token LESSTHAN LESSTHANOREQUAL GREATERTHAN GREATERTHANOREQUAL
%token ISEQUALTO NOTEQUALTO LOGICALAND LOGICALOR
%token EQUALS INCREMENT DECREMENT SWAP
 
%start CompilationUnit
%%

Literal:	  INTLIT
		| DOUBLELIT
		| BOOLTRUE
		| BOOLFALSE
		| STRINGLIT
		| NULLVAL
		;
	
Type:		  PrimitiveType
		| ReferenceType
		;

PrimitiveType:	  NumericType
		| BOOL
		| STRING
		;

NumericType:	  INT
		| DOUBLE
		;


ReferenceType:	  ClassType
		| DefaultTableMap
		| ListType
		| TableType
		;

ClassType:	  Name
		;

ListType:	  LIST
		| LIST LESSTHAN LegalElement GREATERTHAN
		;

TableType:	  TABLE
		| TABLE LESSTHAN LegalElement GREATERTHAN
		| TABLE LESSTHAN TableIndex COMMA LegalElement GREATERTHAN
		;

LegalElement:	  INT
		| DOUBLE
		| STRING
		| ClassType
		;

TableIndex:	  INT
		| STRING
		;

DefaultTableMap: Name LEFTSUBSCRIPT RIGHTSUBSCRIPT
		;

Name:		  SimpleName
		| QualifiedName
		;

SimpleName:	  IDENTIFIER
		| CLASSNAME
		;

QualifiedName:	  Name PERIOD IDENTIFIER
		;

CompilationUnit: TypeDeclarationsOpt
		;

TypeDeclarationsOpt: TypeDeclarations
		|
		;


TypeDeclarations: TypeDeclaration
		| TypeDeclarations TypeDeclaration
		;

TypeDeclaration:  ClassDeclaration
		| ClassMemberDeclaration
		;


ClassDeclaration: CLASS IDENTIFIER ClassBody SEMICOLON
		 { insert_cnt(cnt, $2->text); }
		;

ClassBody:	  LEFTBRACE ClassBodyDeclarationsOpt RIGHTBRACE
		;

ClassBodyDeclarationsOpt: ClassBodyDeclarations
		|
		;

ClassBodyDeclarations: ClassBodyDeclaration
		| ClassBodyDeclarations ClassBodyDeclaration
		;

ClassBodyDeclaration: ClassMemberDeclaration
		| ConstructorDeclaration
		;

ClassMemberDeclaration: FieldDeclaration
		| MethodDeclaration
		;

FieldDeclaration: Type VariableDeclarators SEMICOLON
		;

VariableDeclarators: VariableDeclarator
		| VariableDeclarators COMMA VariableDeclarator
		;

VariableDeclarator: VariableDeclaratorId
		;

VariableDeclaratorId: IDENTIFIER
		| VariableDeclaratorId LEFTSUBSCRIPT RIGHTSUBSCRIPT
		;

MethodDeclaration: MethodHeader Block
		;

MethodHeader:      Type MethodDeclarator
		|  VOID MethodDeclarator
		;

FormalParameterListOpt: FormalParameterList
		|
		;

MethodDeclarator: IDENTIFIER LEFTPAREN FormalParameterListOpt RIGHTPAREN
		| MethodDeclarator LEFTSUBSCRIPT RIGHTSUBSCRIPT
		;

FormalParameterList: FormalParameter
		| FormalParameterList COMMA FormalParameter
		;

FormalParameter: Type VariableDeclaratorId
		;

ConstructorDeclaration: ConstructorDeclarator ConstructorBody
		;

ConstructorDeclarator: SimpleName LEFTPAREN FormalParameterListOpt RIGHTPAREN
		;

ExplicitConstructorInvocationOpt: ExplicitConstructorInvocation
		|
		;

ArgumentListOpt:  ArgumentList
		|
		;

ConstructorBody: LEFTBRACE ExplicitConstructorInvocationOpt BlockStatementsOpt RIGHTBRACE SEMICOLON
		;

ExplicitConstructorInvocation: LEFTPAREN ArgumentListOpt RIGHTPAREN SEMICOLON
		;

Block:		  LEFTBRACE BlockStatementsOpt RIGHTBRACE SEMICOLON
		;

BlockStatementsOpt: BlockStatements
		|
		;

BlockStatements:  BlockStatement
		| BlockStatements BlockStatement
		;

BlockStatement:   LocalVariableDeclarationStatement
		| Statement
		;

LocalVariableDeclarationStatement: LocalVariableDeclaration SEMICOLON
		;

LocalVariableDeclaration: Type VariableDeclarators
		;

Statement:	  StatementWithoutTrailingSubstatement
		| IfThenStatement
		| IfThenElseStatement
		| IfThenElseIfStatement
		| WhileStatement
		| ForStatement
		;

StatementWithoutTrailingSubstatement: Block
		| EmptyStatement
		| ExpressionStatement
		| BreakStatement
		| ReturnStatement
		;

EmptyStatement:	  SEMICOLON
		;

ExpressionStatement: StatementExpression SEMICOLON
		;

StatementExpression: Assignment
		| MethodInvocation
		| ClassInstanceCreationExpression
		;

IfThenStatement:  IF LEFTPAREN Expression RIGHTPAREN Block
		;

IfThenElseStatement:  IF LEFTPAREN Expression RIGHTPAREN Block ELSE Block
		;

IfThenElseIfStatement: IF LEFTPAREN Expression RIGHTPAREN Block ElseIfSequence
		|  IF LEFTPAREN Expression RIGHTPAREN Block ElseIfSequence ELSE Block
;

ElseIfSequence:   ElseIfStatement
		| ElseIfSequence ElseIfStatement
		;

ElseIfStatement: ELSE IF LEFTPAREN Expression RIGHTPAREN Block
;

			
WhileStatement:	 WHILE LEFTPAREN Expression RIGHTPAREN Statement
		;

ForInitOpt: ForInit |  ;

ExpressionOpt: Expression |  ;

ForUpdateOpt: ForUpdate 	|  ;

ForStatement: FOR LEFTPAREN ForInitOpt SEMICOLON ExpressionOpt SEMICOLON ForUpdateOpt RIGHTPAREN Block
		;

ForInit:	  StatementExpressionList
		| LocalVariableDeclaration
		;

ForUpdate:	  StatementExpressionList
		;

StatementExpressionList: StatementExpression
		| StatementExpressionList COMMA StatementExpression
		;

IDENTOpt: IDENTIFIER |  ;

BreakStatement:	  BREAK IDENTOpt SEMICOLON
		;

ReturnStatement:  RETURN ExpressionOpt SEMICOLON
		;

Primary:	  Literal
		| LEFTPAREN Expression RIGHTPAREN
		| FieldAccess
		| MethodInvocation
		| ListTableAccess
		;

ClassInstanceCreationExpression: ClassType LEFTPAREN ArgumentListOpt RIGHTPAREN
		;

ArgumentList:	  Expression
		| ArgumentList COMMA Expression
		;

FieldAccess:	  Primary PERIOD IDENTIFIER
		;

MethodInvocation: Name LEFTPAREN ArgumentListOpt RIGHTPAREN
		| Primary PERIOD IDENTIFIER LEFTPAREN ArgumentListOpt RIGHTPAREN
		| Name LEFTBRACE ArgumentListOpt RIGHTBRACE
		| Primary PERIOD IDENTIFIER LEFTBRACE ArgumentListOpt RIGHTBRACE
		;

ListTableAccess:  Name LEFTSUBSCRIPT Expression RIGHTSUBSCRIPT
		| Primary LEFTSUBSCRIPT Expression RIGHTSUBSCRIPT
		| Name LEFTSUBSCRIPT Expression COLON Expression RIGHTSUBSCRIPT
		;

PostFixExpression: Primary
		| Name
		;

UnaryExpression:
		MINUS UnaryExpression
		| UnaryExpressionNotPlusMinus
		| DROLL UnaryExpression
		| SIZE UnaryExpression
		;

UnaryExpressionNotPlusMinus: PostFixExpression
		| LOGICALNOT UnaryExpression
		;

MultiplicativeExpression: UnaryExpression
		| MultiplicativeExpression MULTIPLY UnaryExpression
		| MultiplicativeExpression DIVIDE UnaryExpression
		| MultiplicativeExpression MODULUS UnaryExpression
		| MultiplicativeExpression DROLL UnaryExpression
		;

AdditiveExpression: MultiplicativeExpression
		| AdditiveExpression PLUS MultiplicativeExpression
		| AdditiveExpression MINUS MultiplicativeExpression
		;

RelationalExpression: AdditiveExpression
		| RelationalExpression LESSTHAN AdditiveExpression
		| RelationalExpression GREATERTHAN AdditiveExpression
		| RelationalExpression LESSTHANOREQUAL AdditiveExpression
		| RelationalExpression GREATERTHANOREQUAL AdditiveExpression
		;

EqualityExpression: RelationalExpression
		| EqualityExpression ISEQUALTO RelationalExpression
		| EqualityExpression NOTEQUALTO RelationalExpression
		;

ConditionalAndExpression: EqualityExpression
		| ConditionalAndExpression LOGICALAND EqualityExpression
		;

ConditionalOrExpression: ConditionalAndExpression
		| ConditionalOrExpression LOGICALOR ConditionalAndExpression
		;

AssignmentExpression: ConditionalOrExpression
		| Assignment
		| concatOpt
		| ListAssignment
		| TableAssignment
		;

Assignment:	  LeftHandSide AssignmentOperator AssignmentExpression
		;

LeftHandSide:	  Name
		| FieldAccess
		| ListTableAccess
		| DefaultTableMap
		;

AssignmentOperator: EQUALS
		| INCREMENT
		| DECREMENT
		| SWAP
		;

Expression:	  AssignmentExpression
		;

ListAssignment: LEFTSUBSCRIPT ListValues RIGHTSUBSCRIPT
		;

ListValues: LegalValue
		| ListValues COMMA LegalValue
		;

TableAssignment: LEFTBRACE TableValues SEMICOLON RIGHTBRACE
/* semicolon required after tablevalues because of semicolon insertion rules that allow statements in blocks to not have semicolons that appear before RIGHBRACE on the same line*/
;

TableValues: TableObject
		| TableValues COMMA TableObject
		;

TableObject: LegalIndex COLON LegalValue
		;

LegalIndex:	  INTLIT
		| STRINGLIT
		;

LegalValue:	  INTLIT
		| DOUBLELIT
		| STRINGLIT
		| IDENTIFIER
		| CLASSNAME
		;


concatOpt:	  concat concat
		| concatOpt concat
		;

concat:		  Name
		| Primary
		;

%%

void yyerror(char const *s)
{
   printf("\n%s, file:%s, line: %d\n", s, yyfilename, yylineno);
   exit(2);	
}
