/*
 * g0 Grammar
 * Derived from the Godiva Grammar, which was in turn
 * based on the Java grammar in Gosling/Joy/Steele, Chapter 19
 */

%{
#include <stdio.h>
#include "tree.h"
// #include ""

// int yydebug = 1;
tree* yytree = NULL;
extern void yyerror(char* s); //g0lex.l
int yylex();
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
%token < node > BOOL BREAK CLASS STRING
%token < node > DOUBLE ELSE FOR
%token < node > IF INT RETURN
%token < node > TABLE LIST
%token < node > THIS TRUE FALSE
%token < node > VOID WHILE IDENT CONTINUE
%token < node > NULLLITERAL INTLITERAL
%token < node > CHARLITERAL FLOATLITERAL STRINGLITERAL
%token < node > LP RP LC RC LB RB SM CM DOT ASN LT GT BANG SHARP
%token < node > EQ NE LE GE ANDAND OROR PLUS MINUS MUL DIV AND OR
%token < node > MOD PLASN MIASN SWAP COLON
%token < node > DROLL BAD_TOKEN CLASS_NAME
%token END 0 "end of file"

/*
 * each nonterminal is declared.  nonterminals correspond to internal nodes
 */
%type < node > Program GlobalVariable VariableDeclaratorList
%type < node > ClassDeclaration CompilationUnits CompilationUnit
%type < node > Function FunctionPrototype FunctionDefinition AssignmentExpression
%type < node > FunctionBody Name SimpleName QualifiedName
%type < node > ClassHeader ClassBlock ClassVariable ClassBlockUnitList ClassBlockUnit
%type < node > MethodDeclaration ConstructorDeclaration ConstructorDeclarator ConstructorBody
%type < node > VariableDeclaratorId MethodDeclarator 
%type < node > TypeList MethodHeader MethodBody ClassVariableList
%type < node > Block BlockStatementList BlockStatement Statement
%type < node > NoLocalVariableBlock NoLocalVariableBlockStatementList NoLocalVariableBlockStatement Statement
%type < node > StatementNoShortIf StatementWithoutTrailingSubstatement ExpressionStatement
%type < node > StatementExpression IfThenStatement IfThenElseStatement IfThenElseStatementNoShortIf
%type < node > WhileStatement WhileStatementNoShortIf ForInit
%type < node > ForUpdate ForStatement ForStatementNoShortIf StatementExpressionList
%type < node > EmptyStatement BreakStatement ReturnStatement 
%type < node > Expression AssignmentOperator Assignable Assignment ConditionalOrExpression 
%type < node > EqualityExpression RelationalExpression AdditiveExpression MultiplicativeExpression 
%type < node > SwapExpression UnaryExpressionNotPlusMinus UnaryExpression PostFixExpression 
%type < node > ArrayAccess MethodInvocation FieldAccess PrimaryNoNewArray Primary
%type < node > ArgumentList ExpressionOpt
%type < node > ConditionalAndExpression 
%type < node > Literal  ConcatentationExpresssion ImplicitConcatExpression
%type < node > LocalVariable ListInitializer
%type < node >  FormalParameterList FormalParameter
%type < node > BoolLiteral Semicolon 
%type < node > Type PrimitiveType ListType TableType

%left SWAP MIASN PLASN ASN
%left OROR
%left ANDAND
%left NE EQ
%left GT GE LT LE
%left PLUS MINUS
%left DROLL MOD DIV MUL
%left SHARP UDROLL UNEGATE UMINUS
%left PAREN SUB

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
        CompilationUnits { yytree = $1; }
      ;

GlobalVariable:
        Type VariableDeclaratorList Semicolon		{ $$ = alctree( "global var", 108, 3, $1, $2, $3 ); }
      ;

VariableDeclaratorList:
        VariableDeclaratorId       				{ $$ = $1; }
      | VariableDeclaratorList CM VariableDeclaratorId		{ $$ = alctree( "var list", 113, 3, $1, $2, $3 ); }
      ;

Type:
        CLASS_NAME       { $$ = $1; }
      | PrimitiveType       { $$ = $1; }
      | ListType       { $$ = $1; }
      | TableType       { $$ = $1; }
      ;

ListType:
        LIST				{ $$ = $1; }
      | LIST LT PrimitiveType GT	{ $$ = alctree( "list", 125, 4, $1, $2, $3, $4 ); }
      ;

TableType:
        TABLE						{ $$ = $1; }
      | TABLE LT PrimitiveType GT			{ $$ = alctree( "table", 130, 4, $1, $2, $3, $4 ); }
      | TABLE LT PrimitiveType CM PrimitiveType GT	{ $$ = alctree( "table", 131, 5, $1, $2, $3, $4, $5 ); }
      ;

PrimitiveType:
        INT		{ $$ = $1; }
      | BOOL		{ $$ = $1; }
      | DOUBLE		{ $$ = $1; }
      | STRING		{ $$ = $1; }
      ;

CompilationUnits:
        CompilationUnit				{ $$ = $1; }
      | CompilationUnits CompilationUnit	{ $$ = alctree( "Compilation Units", 143, 2, $1, $2 ); }
      ;

CompilationUnit:
        ClassDeclaration		{ $$ = $1; }
      | Function			{ $$ = $1; }
      | AssignmentExpression		{ $$ = $1; }
      | GlobalVariable			{ $$ = $1; }
      ;

ClassDeclaration:
        ClassHeader ClassBlock		{ $$ = alctree( "Class declaration", 154, 2, $1, $2 ); }
      ;

ClassHeader:
        CLASS CLASS_NAME			{ $$ = alctree( "Class header", 158, 2, $1, $2 ); }
      ;

ClassBlock:
        LC ClassBlockUnitList RC		{ $$ = alctree( "Class Block", 162, 3, $1, $2, $3 ); }
      | LC RC					{ $$ = alctree( "Empty class block", 163, 2, $1, $2 ); }
      ;

ClassVariableList:
	 IDENT		{ $$ = $1; }
	| ClassVariableList CM IDENT		{ $$ = alctree( "Class variable list", 168, 3, $1, $2, $3 ); }
	;

ClassVariable:
      Type ClassVariableList Semicolon	{ $$ = alctree( "Class variable list", 172, 3, $1, $2, $3 ); }
      | Type Assignment { yyerror("syntax error"); }
      ;

ClassBlockUnitList:
        ClassBlockUnit		{ $$ = $1; }
      | ClassBlockUnitList ClassBlockUnit	{ $$ = alctree( "Class Block statements", 178, 2, $1, $2 ); }
      ;

ClassBlockUnit:
        ClassVariable			{ $$ = $1; }
      | MethodDeclaration		{ $$ = $1; }
      | ConstructorDeclaration		{ $$ = $1; }
      ;

MethodDeclaration:
        MethodHeader MethodBody		{ $$ = alctree( "Method", 188, 2, $1, $2 ); }
      ;

MethodHeader:
        Type MethodDeclarator		{ $$ = alctree( "Method header", 192, 2, $1, $2 ); }
      | VOID MethodDeclarator		{ $$ = alctree( "Method header", 193, 2, $1, $2 ); }
      ;

MethodDeclarator:
        IDENT LP FormalParameterList RP	{ $$ = alctree( "Method declarator", 197, 4, $1, $2, $3, $4 ); }
      | IDENT LP RP			{ $$ = alctree( "Method declarator", 198, 3, $1, $2, $3 ); }
      ;

MethodBody:
        Block       { $$ = $1; }
      ;

ConstructorDeclaration:
         ConstructorDeclarator ConstructorBody		{ $$ = alctree( "Constructor delcaration", 206, 2, $1, $2 ); }
      ;

ConstructorDeclarator:
        CLASS_NAME LP FormalParameterList RP	{ $$ = alctree( "Constructor Header", 210, 4, $1, $2, $3, $4 ); }
      | CLASS_NAME LP RP			{ $$ = alctree( "Constructor Header", 211, 3, $1, $2, $3 ); }
      ;

ConstructorBody:
        Block       { $$ = $1; }
      ;

Function:
        FunctionPrototype       { $$ = $1; }
      | FunctionDefinition       { $$ = $1; }
      ;

FunctionPrototype:
        Type IDENT LP TypeList RP Semicolon	{ $$ = alctree( "func proto", 224, 6, $1, $2, $3, $4, $5, $6 ); }
      | Type IDENT LP RP Semicolon		{ $$ = alctree( "func proto", 225, 5, $1, $2, $3, $4, $5 ); }
      | VOID IDENT LP TypeList RP Semicolon	{ $$ = alctree( "func proto", 226, 6, $1, $2, $3, $4, $5, $6 ); }
      | VOID IDENT LP RP Semicolon		{ $$ = alctree( "func proto", 227, 5, $1, $2, $3, $4, $5 ); }
      ;

TypeList:
        Type      			 { $$ = $1; }
      | TypeList CM Type		{ $$ = alctree( "Type list", 232, 3, $1, $2, $3 ); }
      ;

FunctionDefinition:
        Type IDENT LP FormalParameterList RP FunctionBody	{ $$ = alctree( "func defn", 236, 6, $1, $2, $3, $4, $5, $6 ); }
      | Type IDENT LP RP FunctionBody				{ $$ = alctree( "func defn", 237, 5, $1, $2, $3, $4, $5 ); }
      | VOID IDENT LP FormalParameterList RP FunctionBody	{ $$ = alctree( "func defn", 238, 6, $1, $2, $3, $4, $5, $6 ); }
      | VOID IDENT LP RP FunctionBody				{ $$ = alctree( "func defn", 239, 5, $1, $2, $3, $4, $5 ); }
      ;

FunctionBody:
        Block       { $$ = $1; }
      ;

Block:
        LC BlockStatementList RC 		{ $$ = alctree( "Block", 247, 3, $1, $2, $3 ); }
      | LC RC 					{ $$ = alctree( "Empty Block", 248, 2, $1, $2 ); }
      ;

BlockStatementList:
        BlockStatement       			{ $$ = $1; }
      | BlockStatementList BlockStatement		{ $$ = alctree( "Block stmt list", 253, 2, $1, $2 ); }
      ;

BlockStatement:
        LocalVariable    				   { $$ = $1; }
      | Statement       				{ $$ = $1; }
      ;

NoLocalVariableBlock:
        LC NoLocalVariableBlockStatementList RC 		{ $$ = alctree( "No local var block", 262 , 3, $1, $2, $3 ); }
      | LC RC 							{ $$ = alctree( "Empty block", 263, 2, $1, $2 ); }
      ;

NoLocalVariableBlockStatementList:
        NoLocalVariableBlockStatement       					{ $$ = $1; }
      | NoLocalVariableBlockStatementList NoLocalVariableBlockStatement		{ $$ = alctree( "No local var statement list", 268, 2, $1, $2 ); }
      ;

NoLocalVariableBlockStatement:
        Statement       { $$ = $1; }
      ;

Statement:
        StatementWithoutTrailingSubstatement       { $$ = $1; }
      | IfThenStatement       { $$ = $1; }
      | IfThenElseStatement       { $$ = $1; }
      | WhileStatement       { $$ = $1; }
      | ForStatement       { $$ = $1; }
      ;

StatementNoShortIf:
        StatementWithoutTrailingSubstatement	{ $$ = $1; }
      | IfThenElseStatementNoShortIf       	{ $$ = $1; }
      | WhileStatementNoShortIf       		{ $$ = $1; }
      | ForStatementNoShortIf       		{ $$ = $1; }
      ;

StatementWithoutTrailingSubstatement:
        NoLocalVariableBlock       		{ $$ = $1; }
      | EmptyStatement       			{ $$ = $1; }
      | ExpressionStatement       		{ $$ = $1; }
      | BreakStatement       			{ $$ = $1; }
      | ReturnStatement       			{ $$ = $1; }
      ;

ExpressionStatement:
        StatementExpression Semicolon		{ $$ = alctree( "Expression Stmt", 299, 2, $1, $2 ); }
	    | SwapExpression Semicolon		{ $$ = alctree( "Swap Expr Stmt", 300, 2, $1, $2 ); }
      ;

StatementExpression:
        Assignment       { $$ = $1; }
      | MethodInvocation       { $$ = $1; }
      ;

IfThenStatement:
        IF LP Expression RP Statement	{ $$ = alctree( "If-then", 309, 5, $1, $2, $3, $4, $5 ); }
      ;

IfThenElseStatement:
        IF LP Expression RP StatementNoShortIf 
            ELSE Statement		{ $$ = alctree( "if-then-else stmt", 314, 7, $1, $2, $3, $4, $5, $6 ); }
      ;

IfThenElseStatementNoShortIf:
        IF LP Expression RP StatementNoShortIf
            ELSE StatementNoShortIf		{ $$ = alctree( "if-then-else stmt (no short if)", 319, 7, $1, $2, $3, $4, $5, $6, $7 ); }
      ;

WhileStatement:
        WHILE LP Expression RP Statement	{ $$ = alctree( "while", 323, 5, $1, $2, $3, $4, $5 ); }
	| IDENT NoLocalVariableBlock WHILE LP Expression RP Semicolon				{ yyerror("syntax error"); fprintf(stderr, "do-while loops not supported in g0!\n"); exit(2); }
      ;

WhileStatementNoShortIf:
        WHILE LP Expression RP StatementNoShortIf		{ $$ = alctree( "While (No short if)", 328, 5, $1, $2, $3, $4, $5 ); }
      ;

ForInit:
        StatementExpressionList       { $$ = $1; }
      | { $$ = NULL; }
      ;

ExpressionOpt:
        Expression
      | { $$ = NULL; }
      ;

ForUpdate:
        StatementExpressionList       { $$ = $1; }
      | { $$ = NULL; }
      ;

ForStatement:
        FOR LP ForInit SM ExpressionOpt SM ForUpdate RP Statement	{ $$ = alctree( "for stmt", 347, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9 ); }
      ;

ForStatementNoShortIf:
        FOR LP ForInit SM ExpressionOpt SM ForUpdate RP StatementNoShortIf	{ $$ = alctree( "for stmt (no short if)", 351, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9 ); }
      ;

StatementExpressionList:
        StatementExpression       { $$ = $1; }
      | StatementExpressionList CM StatementExpression		{ $$ = alctree( "expression list", 356, 3, $1, $2, $3 ); }
      ;

EmptyStatement:
        Semicolon       { $$ = $1; }
      ;

BreakStatement:
        BREAK Semicolon		{ $$ = alctree( "break", 364, 2, $1, $2 ); }
      ;

ReturnStatement:
        RETURN Expression Semicolon		{ $$ = alctree( "return stmt", 368, 3, $1, $2, $3 ); }
      | RETURN Semicolon			{ $$ = alctree( "return stmt", 369, 2, $1, $2 ); }
      ;

ArgumentList:
	  Expression       { $$ = $1; }
	| ArgumentList CM Expression		{ $$ = alctree( "arg list", 374, 3, $1, $2, $3 ); }
	;

Primary:
     PrimaryNoNewArray       { $$ = $1; }
   ;

ListInitializer:
	PrimaryNoNewArray			{ $$ = $1; }
	| ListInitializer CM PrimaryNoNewArray { $$ = alctree( "List items", 383, 3, $1, $2, $3 ); }
	;

PrimaryNoNewArray:
     Literal       { $$ = $1; }
   | LP Expression RP		{ $$ = alctree( "Paren Expr", 388, 3, $1, $2, $3 ); }
   | MethodInvocation       { $$ = $1; }
   | Assignable       { $$ = $1; }
   | SHARP IDENT    { $$ = alctree( "List Size", 391, 1, $2); }
   ;

FieldAccess:
     Primary DOT IDENT		{ $$ = alctree( "Field Access", 395, 3, $1, $2, $3 ); }
   ;

MethodInvocation:
     Name LP ArgumentList RP			{ $$ = alctree( "Method call", 399, 4, $1, $2, $3, $4 ); }
   | Name LP RP					{ $$ = alctree( "Method call", 400, 3, $1, $2, $3 ); }
   | Primary DOT IDENT LP ArgumentList RP	{ $$ = alctree( "Method call", 401, 6, $1, $2, $3, $4, $5, $6 ); }
   | Primary DOT IDENT LP  RP			{ $$ = alctree( "Method call", 402, 5, $1, $2, $3, $4, $5 ); }
   | CLASS_NAME LP RP			{ $$ = alctree( "Class Constructor Call", 403, 1, $1 ); }
   ;

ArrayAccess:
     PrimaryNoNewArray LB Expression RB		{ $$ = alctree( "Array Access", 407, 4, $1, $2, $3, $4 ); }
   ;

PostFixExpression:
     Primary       { $$ = $1; }
   | ConcatentationExpresssion { $$ = $1; }
   ;

UnaryExpression:
     MINUS UnaryExpression		{ $$ = alctree( "UnaryExpression", 416, 2, $1, $2 ); }
   | UnaryExpressionNotPlusMinus       { $$ = $1; }
   ;

UnaryExpressionNotPlusMinus:
     PostFixExpression       { $$ = $1; }
   | BANG UnaryExpression		{ $$ = alctree( "UnaryExpressionNo+-", 422, 2, $1, $2 ); }
   | DROLL UnaryExpression		{ $$ = alctree( "Unary Dice roll", 423, 2, $1, $2 ); }
   ;

MultiplicativeExpression:
     UnaryExpression       { $$ = $1; }
   | MultiplicativeExpression MUL UnaryExpression		{ $$ = alctree( "Multiplicative Expression", 428, 3, $1, $2, $3 ); }
   | MultiplicativeExpression DIV UnaryExpression		{ $$ = alctree( "Division Expression", 429, 3, $1, $2, $3 ); }
   | MultiplicativeExpression MOD UnaryExpression		{ $$ = alctree( "Modulus Expression", 430, 3, $1, $2, $3 ); }
   | MultiplicativeExpression DROLL UnaryExpression		{ $$ = alctree( "Dice Roll Expression", 431, 3, $1, $2, $3 ); }
   ;

ImplicitConcatExpression:
	  STRINGLITERAL Name				{ $$ = alctree( "Imp. Concat Expr (str + var)", 435, 2, $1, $2 ); }
	| STRINGLITERAL MethodInvocation			{ $$ = alctree( "Imp. Concat Expr (str + method)", 436, 2, $1, $2 ); }
	| Name STRINGLITERAL				{ $$ = alctree( "Imp. Concat Expr (var + str)", 437, 2, $1, $2 ); }
	| MethodInvocation STRINGLITERAL			{ $$ = alctree( "Imp. Concat Expr (method + str)", 438, 2, $1, $2 ); }
	// | Name Name						{ yyerror("syntax error"); fprintf(stderr, "implicit string concatenation must have a string literal in the first two values\n"); exit(2); }
	| STRINGLITERAL STRINGLITERAL				{ yyerror("syntax error"); fprintf(stderr, "implicit string concatenation not allowed between two string literals\n"); exit(2); }
	| ConcatentationExpresssion Name			{ $$ = alctree( "Imp. Concat Expr list (+name)", 441, 2, $1, $2 ); }
	| ConcatentationExpresssion MethodInvocation		{ $$ = alctree( "Imp. Concat Expr list (+method)", 442, 2, $1, $2 ); }
	| ConcatentationExpresssion STRINGLITERAL		{ $$ = alctree( "Imp. Concat Expr list (+string)", 443, 2, $1, $2 ); }
	;

ConcatentationExpresssion:
	  ImplicitConcatExpression 		{ $$ = $1; }
	// | ExplicitConcatExpression 		{ $$ = $1; }
	;

AdditiveExpression:
     MultiplicativeExpression       { $$ = $1; }
   | AdditiveExpression PLUS MultiplicativeExpression		  { $$ = alctree( "Sum Expr", 453, 2, $1, $3 ); }
   | AdditiveExpression MINUS MultiplicativeExpression		{ $$ = alctree( "Difference Expr", 454, 2, $1, $3 ); }
   ;

RelationalExpression:
     AdditiveExpression       { $$ = $1; }
   | RelationalExpression LT AdditiveExpression		{ $$ = alctree( "less Expr", 459, 2, $1, $3 ); }
   | RelationalExpression LE AdditiveExpression		{ $$ = alctree( "less/eq Expr", 460, 2, $1, $3 ); }
   | RelationalExpression GT AdditiveExpression		{ $$ = alctree( "great Expr", 461, 2, $1, $3 ); }
   | RelationalExpression GE AdditiveExpression		{ $$ = alctree( "great/eq Expr", 462, 2, $1, $3 ); }
   ;

EqualityExpression:
     RelationalExpression       { $$ = $1; }
   | EqualityExpression EQ RelationalExpression		{ $$ = alctree( "'==' Expr", 467, 2, $1, $3 ); }
   | EqualityExpression NE RelationalExpression		{ $$ = alctree( "'!=' Expr", 468, 2, $1, $3 ); }
   ;

ConditionalAndExpression:
     EqualityExpression       { $$ = $1; }
   | ConditionalAndExpression ANDAND EqualityExpression		{ $$ = alctree( "AND", 473, 2, $1, $3 ); }
   ;

ConditionalOrExpression:
     ConditionalAndExpression       { $$ = $1; }
   | ConditionalOrExpression OROR ConditionalAndExpression		{ $$ = alctree( "OR", 478, 2, $1, $3 ); }
   ;

AssignmentExpression:
     ConditionalOrExpression       { $$ = $1; }
   | Assignment       { $$ = $1; }
   ;

Assignment:
     Assignable AssignmentOperator AssignmentExpression		{ $$ = alctree( "Assign", 487, 3, $1, $2, $3 ); }
     | Assignable AssignmentOperator LB ListInitializer RB       { $$ = alctree( "List Initializer", 488, 3, $1, $2, $4); }
   ;

SwapExpression:
      Assignable SWAP Assignable		{ $$ = alctree( "Swap", 492, 2, $1, $3 ); }
    | Assignable SWAP Assignment		{ $$ = alctree( "Swap w/ Assign", 493, 2, $1, $3 ); }
    | Assignable SWAP SwapExpression		{ $$ = alctree( "Multiple Swap", 494, 2, $1, $3 ); }
    ;

Assignable:
     Name       { $$ = $1; }
   | FieldAccess       { $$ = $1; }
   | ArrayAccess       { $$ = $1; }
   ;

AssignmentOperator:
     ASN		{ $$ = $1; }
   | PLASN		{ $$ = $1; }
   | MIASN		{ $$ = $1; }
   ;

Expression:
        SwapExpression
      | AssignmentExpression
      ;
		

FormalParameterList:
        FormalParameter       { $$ = $1; }
      | FormalParameterList CM FormalParameter		{ $$ = alctree( "Named param list", 517, 3, $1, $2, $3 ); }
      ;

FormalParameter:
        Type VariableDeclaratorId		{ $$ = alctree( "named parameter", 521, 2, $1, $2 ); }
      ;

LocalVariable:
        Type VariableDeclaratorList Semicolon		{ $$ = alctree( "local variable", 525, 3, $1, $2, $3 ); }
      ;

VariableDeclaratorId:
        IDENT 		{ $$ = $1; }
      | VariableDeclaratorId LB RB		{ $$ = alctree( "variable declarator", 530, 3, $1, $2, $3 ); }
      ;

Name:
        SimpleName       { $$ = $1; }
      | QualifiedName       { $$ = $1; }
      ;

SimpleName:
        IDENT		{ $$ = $1; }
      ;

QualifiedName:
        Name DOT IDENT		{ $$ = alctree( "qualified name", 543, 3, $1, $2, $3 ); }
      ;

BoolLiteral:
        TRUE		{ $$ = $1; }
      | FALSE		{ $$ = $1; }
      ;

Semicolon:
        SM		{ $$ = $1; }
      | { $$ = NULL; }
      ;

Literal:
		  INTLITERAL		{ $$ = $1; }   
		| FLOATLITERAL		{ $$ = $1; }
		| BoolLiteral       { $$ = $1; }
		| STRINGLITERAL		{ $$ = $1; }
		| CHARLITERAL		{ $$ = $1; }
		| NULLLITERAL		{ $$ = $1; }
		;

/*
 * the start symbol, Goal, may seem to be here for rhetorical purposes,
 * but it is also the ideal spot to insert a semantic action that passes
 * the completed parse tree to a later phase of compilation.

ProductionRule:
        
      | 
      ;

 */