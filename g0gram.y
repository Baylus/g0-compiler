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
   nodeptr node;
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
%type < node > Goal Literal Type PrimitiveType NumericType IntegralType
%type < node > FloatingPointType ReferenceType ClassOrInterfaceType
%type < node > ClassType  ArrayType Name SimpleName
%type < node > QualifiedName CompilationUnit ImportDeclarations
%type < node > TypeDeclarations PackageDeclaration ImportDeclaration
%type < node > SingleTypeImportDeclaration TypeImportOnDemandDeclaration
%type < node > TypeDeclaration Modifiers Modifier ClassDeclaration
%type < node > Interfaces InterfaceTypeList ClassBody
%type < node > ClassBodyDeclarations ClassBodyDeclaration
%type < node > ClassMemberDeclaration FieldDeclaration VariableDeclarators
%type < node > VariableDeclarator VariableDeclaratorId VariableInitializer
%type < node > MethodDeclaration MethodHeader MethodDeclarator
%type < node > FormalParameterList FormalParameter Throws ClassTypeList
%type < node > MethodBody  ConstructorDeclaration
%type < node > ConstructorDeclarator ConstructorBody
%type < node > ExplicitConstructorInvocation InterfaceDeclaration
%type < node > ExtendsInterfaces InterfaceBody InterfaceMemberDeclarations
%type < node > InterfaceMemberDeclaration 
%type < node > AbstractMethodDeclaration ArrayInitializer
%type < node > VariableInitializers Block BlockStatements BlockStatement
%type < node > LocalVariableDeclarationStatement LocalVariableDeclaration
%type < node > Statement StatementNoShortIf
%type < node > StatementWithoutTrailingSubstatement EmptyStatement
%type < node > LabeledStatement LabeledStatementNoShortIf
%type < node > ExpressionStatement StatementExpression IfThenStatement
%type < node > IfThenElseStatement IfThenElseStatementNoShortIf
%type < node > SwitchStatement SwitchBlock SwitchBlockStatementGroups
%type < node > SwitchBlockStatementGroup SwitchLabels SwitchLabel
%type < node > WhileStatement WhileStatementNoShortIf 
%type < node > ForStatement ForStatementNoShortIf ForInit ForUpdate
%type < node > StatementExpressionList BreakStatement ContinueStatement
%type < node > ReturnStatement
%type < node > Primary
%type < node > PrimaryNoNewArray ClassInstanceCreationExpression
%type < node > ArgumentList ArrayCreationExpression DimExprs DimExpr Dims
%type < node > FieldAccess MethodInvocation ArrayAccess PostFixExpression
%type < node > 
%type < node > UnaryExpression
%type < node > UnaryExpressionNotPlusMinus CastExpression
%type < node > MultiplicativeExpression AdditiveExpression ShiftExpression
%type < node > RelationalExpression EqualityExpression AndExpression
%type < node >  InclusiveOrExpression
%type < node > ConditionalAndExpression ConditionalOrExpression
%type < node > ConditionalExpression AssignmentExpression Assignment
%type < node > Assignable AssignmentOperator Expression ConstantExpression
%type < node > PackageDeclarationOpt ImportDeclarationsOpt TypeDeclarationsOpt
%type < node > ModifiersOpt InterfacesOpt ClassBodyDeclarationsOpt
%type < node > FormalParameterListOpt IDENTOpt CatchesOpt
%type < node > ExplicitConstructorInvocationOpt BlockStatementsOpt
%type < node > ArgumentListOpt ExplicitConstructorInvocationOpt DimsOpt
%type < node > ExtendsInterfacesOpt InterfaceMemberDeclarationsOpt
%type < node > VariableInitializersOpt CMOpt SwitchBlockStatementGroupsOpt
%type < node > SwitchLabelsOpt ForInitOpt ExpressionOpt ForUpdateOpt

/*
 * the start symbol, Goal, may seem to be here for rhetorical purposes,
 * but it is also the ideal spot to insert a semantic action that passes
 * the completed parse tree to a later phase of compilation.
 */
%start Goal

%%

Goal:		  CompilationUnit
		;

Literal:	  INTLITERAL
		| FLOATLITERAL
		| BOOLLITERAL
		| STRINGLITERAL
		| CHARLITERAL
		| NULLLITERAL
		;
	
Type:		  PrimitiveType
		| ReferenceType
		;

PrimitiveType:	  NumericType
		| BOOLEAN
		;

NumericType:	  IntegralType
		| FloatingPointType
		;

IntegralType:	  BYTE
		| SHORT
		| INT
		| LONG
		| CHAR
		;

FloatingPointType: DOUBLE
		;

ReferenceType:	  ClassOrInterfaceType
		| ArrayType
		;

ClassOrInterfaceType: Name
		;

ArrayType:	  PrimitiveType LB RB
		| Name LB RB
		| ArrayType LB RB
		;

Name:		  SimpleName
		| QualifiedName
		;

SimpleName:	  IDENT
		;

QualifiedName:	  Name DOT IDENT
		;

CompilationUnit:	TypeDeclarationsOpt
		;

TypeDeclarationsOpt: TypeDeclarations | { $$ = NULL; } ;

TypeDeclarations: TypeDeclaration
		| TypeDeclarations TypeDeclaration
		;

TypeDeclaration:  ClassDeclaration
		| MethodDeclaration
		| AbstractMethodDeclaration
		;

ClassDeclaration:  CLASS IDENT ClassBody
		;

ClassBody:	  LC ClassBodyDeclarationsOpt RC
		;

ClassBodyDeclarationsOpt: ClassBodyDeclarations | { $$ = NULL; } ;

ClassBodyDeclarations: ClassBodyDeclaration
		| ClassBodyDeclarations ClassBodyDeclaration
		;

ClassBodyDeclaration: ClassMemberDeclaration
		| ConstructorDeclaration
		;

ClassMemberDeclaration: FieldDeclaration
		| MethodDeclaration
		;

FieldDeclaration: Type VariableDeclarators SM
		;

VariableDeclarators: VariableDeclarator
		| VariableDeclarators CM VariableDeclarator
		;

VariableDeclarator: VariableDeclaratorId
		| VariableDeclaratorId ASN VariableInitializer
		;

VariableDeclaratorId: IDENT
		| VariableDeclaratorId LB RB
		;

VariableInitializer: Expression
		| ArrayInitializer
		;

MethodDeclaration: MethodHeader MethodBody
		;

MethodHeader: Type MethodDeclarator
		| VOID MethodDeclarator
		;

FormalParameterListOpt: FormalParameterList | { $$ = NULL; } ;

MethodDeclarator: IDENT LP FormalParameterListOpt RP
		| MethodDeclarator LB RB
		;

FormalParameterList: FormalParameter
		| FormalParameterList CM FormalParameter
		;

FormalParameter: Type VariableDeclaratorId
		;

MethodBody: Block
		;

ConstructorDeclaration: ModifiersOpt ConstructorDeclarator
				 ConstructorBody
		;

ConstructorDeclarator: SimpleName LP FormalParameterListOpt RP
		;

ExplicitConstructorInvocationOpt: ExplicitConstructorInvocation | { $$ = NULL; } ;

BlockStatementsOpt: BlockStatements | { $$ = NULL; } ;

ArgumentListOpt:  ArgumentList | { $$ = NULL; } ;

ConstructorBody: LC ExplicitConstructorInvocationOpt BlockStatementsOpt RC
		;

ExplicitConstructorInvocation: THIS LP ArgumentListOpt RP SM
		;

AbstractMethodDeclaration: MethodHeader SM
		;

VariableInitializersOpt: VariableInitializers | { $$ = NULL; } ;

CMOpt:	CM { $$ = NULL; } | { $$ = NULL; } ;

ArrayInitializer: LC VariableInitializersOpt CMOpt RC
		;

VariableInitializers: VariableInitializer
		| VariableInitializers CM VariableInitializer
		;

Block:		  LC BlockStatementsOpt RC
		;

BlockStatements:  BlockStatement
		| BlockStatements BlockStatement
		;

BlockStatement:   LocalVariableDeclarationStatement
		| Statement
		;

LocalVariableDeclarationStatement: LocalVariableDeclaration SM
		;

LocalVariableDeclaration: Type VariableDeclarators
		;

Statement:	  StatementWithoutTrailingSubstatement
		| LabeledStatement
		| IfThenStatement
		| IfThenElseStatement
		| WhileStatement
		| ForStatement
		;

StatementNoShortIf: StatementWithoutTrailingSubstatement
		| LabeledStatementNoShortIf
		| IfThenElseStatementNoShortIf
		| WhileStatementNoShortIf
		| ForStatementNoShortIf
		;

StatementWithoutTrailingSubstatement: Block
		| EmptyStatement
		| ExpressionStatement
		| BreakStatement
		| ContinueStatement
		| ReturnStatement
		;

EmptyStatement:	  SM
		;

LabeledStatement: IDENT COLON Statement
		;

LabeledStatementNoShortIf: IDENT COLON StatementNoShortIf
		;

ExpressionStatement: StatementExpression SM
		;

StatementExpression: Assignment
		| MethodInvocation
		;

IfThenStatement:  IF LP Expression RP Statement
		;

IfThenElseStatement:  IF LP Expression RP StatementNoShortIf ELSE Statement
		;

IfThenElseStatementNoShortIf:  IF LP Expression RP StatementNoShortIf
			ELSE StatementNoShortIf
		;

WhileStatement:	  WHILE LP Expression RP Statement
		;

WhileStatementNoShortIf:  WHILE LP Expression RP StatementNoShortIf
		;

ForInitOpt: ForInit | { $$ = NULL; } ;

ExpressionOpt: Expression | { $$ = NULL; } ;

ForUpdateOpt: ForUpdate | { $$ = NULL; } ;

ForStatement:	  FOR LP ForInitOpt SM ExpressionOpt SM ForUpdateOpt RP
			Statement
		;

ForStatementNoShortIf:	  FOR LP ForInitOpt SM ExpressionOpt SM ForUpdateOpt RP
			StatementNoShortIf
		;


ForInit:	  StatementExpressionList
		| LocalVariableDeclaration
		;

ForUpdate:	  StatementExpressionList
		;

StatementExpressionList: StatementExpression
		| StatementExpressionList CM StatementExpression
		;

IDENTOpt: IDENT | { $$ = NULL; } ;

BreakStatement:	  BREAK IDENTOpt SM
		;

ContinueStatement: CONTINUE IDENTOpt SM
		;

ReturnStatement:  RETURN ExpressionOpt SM
		| SUSPEND ExpressionOpt SM
		;

Primary:	  PrimaryNoNewArray
		;

PrimaryNoNewArray: Literal
		| LP Expression RP
		| FieldAccess
		| MethodInvocation
		| ArrayAccess
		;

ArgumentList:	  Expression
		| ArgumentList CM Expression
		;

DimsOpt: Dims | { $$ = NULL; } ;

DimExprs:	  DimExpr
		| DimExprs DimExpr
		;

DimExpr:	  LB Expression RB
		;

Dims:		  LB RB
		| Dims LB RB
		;

FieldAccess:	  Primary DOT IDENT
		;

MethodInvocation: Name LP ArgumentListOpt RP
		| Primary DOT IDENT LP ArgumentListOpt RP
		;

ArrayAccess:	  Name LB Expression RB
		| PrimaryNoNewArray LB Expression RB
		;

PostFixExpression: Primary
		| Name
		;

UnaryExpression: PLUS UnaryExpression
		| MINUS UnaryExpression
		| UnaryExpressionNotPlusMinus
		;

UnaryExpressionNotPlusMinus: PostFixExpression
		| BANG UnaryExpression
		| CastExpression
		;

CastExpression:   LP PrimitiveType DimsOpt RP UnaryExpression
		| LP Expression RP UnaryExpressionNotPlusMinus
		| LP Name Dims RP UnaryExpressionNotPlusMinus
		;

MultiplicativeExpression: UnaryExpression
		| MultiplicativeExpression MUL UnaryExpression
		| MultiplicativeExpression DIV UnaryExpression
		| MultiplicativeExpression MOD UnaryExpression
		;

AdditiveExpression: MultiplicativeExpression
		| AdditiveExpression PLUS MultiplicativeExpression
		| AdditiveExpression MINUS MultiplicativeExpression
		;

RelationalExpression: AdditiveExpression
		| RelationalExpression LT AdditiveExpression
		| RelationalExpression GT AdditiveExpression
		| RelationalExpression LE AdditiveExpression
		| RelationalExpression GE AdditiveExpression
		;

EqualityExpression: RelationalExpression
		| EqualityExpression EQ RelationalExpression
		| EqualityExpression NE RelationalExpression
		;

AndExpression: EqualityExpression
		| AndExpression AND EqualityExpression
		;

InclusiveOrExpression: AndExpression
		| InclusiveOrExpression OR AndExpression
		;

ConditionalAndExpression: InclusiveOrExpression
		| ConditionalAndExpression ANDAND InclusiveOrExpression
		;

ConditionalOrExpression: ConditionalAndExpression
		| ConditionalOrExpression OROR ConditionalAndExpression
		;

ConditionalExpression: ConditionalOrExpression
		| ConditionalOrExpression QUEST Expression
			COLON ConditionalExpression
		;

AssignmentExpression: ConditionalExpression
		| Assignment
		;

Assignment:	  Assignable AssignmentOperator AssignmentExpression
		;

Assignable:	  Name
		| FieldAccess
		| ArrayAccess
		;

AssignmentOperator: ASN
		| PLASN
		| MIASN
		;

Expression:	  AssignmentExpression
		;

ConstantExpression: Expression
		;
