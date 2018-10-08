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
%type < node > ArgumentList
%type < node > ConditionalAndExpression 
%type < node > Literal  ConcatentationExpresssion ImplicitConcatExpression
%type < node > LocalVariable ArrayInitializer
%type < node >  FormalParameterList FormalParameter
%type < node > BoolLiteral Semicolon 
%type < node > Type PrimitiveType ArrayType ListType TableType

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
        Type VariableDeclaratorList Semicolon		{ $$ = alctree( "global var", 20, 3, $1, $2, $3 ); }
      ;

VariableDeclaratorList:
        VariableDeclaratorId       				{ $$ = $1; }
      | VariableDeclaratorList CM VariableDeclaratorId		{ $$ = alctree( "var list", 30, 3, $1, $2, $3 ); }
      ;

Type:
        CLASS_NAME       { $$ = $1; }
      | ArrayType       { $$ = $1; }
      | PrimitiveType       { $$ = $1; }
      | ListType       { $$ = $1; }
      | TableType       { $$ = $1; }
      ;

ListType:
        LIST				{ $$ = $1; }
      | LIST LT PrimitiveType GT	{ $$ = alctree( "list", 40, 4, $1, $2, $3, $4 ); }
      ;

TableType:
        TABLE						{ $$ = $1; }
      | TABLE LT PrimitiveType GT			{ $$ = alctree( "table", 50, 4, $1, $2, $3, $4 ); }
      | TABLE LT PrimitiveType CM PrimitiveType GT	{ $$ = alctree( "table", 51, 5, $1, $2, $3, $4, $5 ); }
      ;

PrimitiveType:
        INT		{ $$ = $1; }
      | BOOL		{ $$ = $1; }
      | DOUBLE		{ $$ = $1; }
      | STRING		{ $$ = $1; }
      ;

ArrayType:
        PrimitiveType LB RB		{ $$ = alctree( "Array Type", 70, 3, $1, $2, $3 ); }
      | Name LB RB			{ $$ = alctree( "Array Type", 71, 3, $1, $2, $3 ); }
      | ArrayType LB RB			{ $$ = alctree( "Array Type", 72, 3, $1, $2, $3 ); }
      ;

CompilationUnits:
        CompilationUnit				{ $$ = $1; }
      | CompilationUnits CompilationUnit	{ $$ = alctree( "Compilation Units", 80, 2, $1, $2 ); }
      ;

CompilationUnit:
        ClassDeclaration		{ $$ = $1; }
      | Function			{ $$ = $1; }
      | AssignmentExpression		{ $$ = $1; }
      | GlobalVariable			{ $$ = $1; }
      ;

ClassDeclaration:
        ClassHeader ClassBlock		{ $$ = alctree( "Class declaration", 100, 2, $1, $2 ); }
      ;

ClassHeader:
        CLASS CLASS_NAME			{ $$ = alctree( "Class header", 110, 2, $1, $2 ); }
      ;

ClassBlock:
        LC ClassBlockUnitList RC		{ $$ = alctree( "Class Block", 120, 3, $1, $2, $3 ); }
      | LC RC					{ $$ = alctree( "Empty class block", 121, 2, $1, $2 ); }
      ;

ClassVariableList:
	 IDENT		{ $$ = $1; }
	| ClassVariableList CM IDENT		{ $$ = alctree( "Class variable list", 129, 3, $1, $2, $3 ); }
	;

ClassVariable:
      Type ClassVariableList Semicolon	{ $$ = alctree( "Class variable list", 131, 3, $1, $2, $3 ); }
      | Type Assignment { yyerror("syntax error"); }
      ;

ClassBlockUnitList:
        ClassBlockUnit		{ $$ = $1; }
      | ClassBlockUnitList ClassBlockUnit	{ $$ = alctree( "Class Block statements", 140, 2, $1, $2 ); }
      ;

ClassBlockUnit:
        ClassVariable			{ $$ = $1; }
      | MethodDeclaration		{ $$ = $1; }
      | ConstructorDeclaration		{ $$ = $1; }
      ;

MethodDeclaration:
        MethodHeader MethodBody		{ $$ = alctree( "Method", 160, 2, $1, $2 ); }
      ;

MethodHeader:
        Type MethodDeclarator		{ $$ = alctree( "Method header", 170, 2, $1, $2 ); }
      | VOID MethodDeclarator		{ $$ = alctree( "Method header", 171, 2, $1, $2 ); }
      ;

MethodDeclarator:
        IDENT LP FormalParameterList RP	{ $$ = alctree( "Method declarator", 180, 4, $1, $2, $3, $4 ); }
      | IDENT LP RP			{ $$ = alctree( "Method declarator", 181, 3, $1, $2, $3 ); }
      ;

MethodBody:
        Block       { $$ = $1; }
      ;

ConstructorDeclaration:
         ConstructorDeclarator ConstructorBody		{ $$ = alctree( "Constructor delcaration", 200, 2, $1, $2 ); }
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
        Type IDENT LP TypeList RP Semicolon	{ $$ = alctree( "func proto", 240, 6, $1, $2, $3, $4, $5, $6 ); }
      | Type IDENT LP RP Semicolon		{ $$ = alctree( "func proto", 241, 5, $1, $2, $3, $4, $5 ); }
      | VOID IDENT LP TypeList RP Semicolon	{ $$ = alctree( "func proto", 242, 6, $1, $2, $3, $4, $5, $6 ); }
      | VOID IDENT LP RP Semicolon		{ $$ = alctree( "func proto", 243, 5, $1, $2, $3, $4, $5 ); }
      ;

TypeList:
        Type      			 { $$ = $1; }
      | TypeList CM Type		{ $$ = alctree( "Type list", 250, 3, $1, $2, $3 ); }
      ;

FunctionDefinition:
        Type IDENT LP FormalParameterList RP FunctionBody	{ $$ = alctree( "func defn", 260, 6, $1, $2, $3, $4, $5, $6 ); }
      | Type IDENT LP RP FunctionBody				{ $$ = alctree( "func defn", 261, 5, $1, $2, $3, $4, $5 ); }
      | VOID IDENT LP FormalParameterList RP FunctionBody	{ $$ = alctree( "func defn", 262, 6, $1, $2, $3, $4, $5, $6 ); }
      | VOID IDENT LP RP FunctionBody				{ $$ = alctree( "func defn", 263, 5, $1, $2, $3, $4, $5 ); }
      ;

FunctionBody:
        Block       { $$ = $1; }
      ;

Block:
        LC BlockStatementList RC 		{ $$ = alctree( "Block", 280, 3, $1, $2, $3 ); }
      | LC RC 					{ $$ = alctree( "Empty Block", 281, 2, $1, $2 ); }
      ;

BlockStatementList:
        BlockStatement       			{ $$ = $1; }
      | BlockStatementList BlockStatement		{ $$ = alctree( "Block stmt list", 290, 2, $1, $2 ); }
      ;

BlockStatement:
        LocalVariable    				   { $$ = $1; }
      | Statement       				{ $$ = $1; }
      ;

NoLocalVariableBlock:
        LC NoLocalVariableBlockStatementList RC 		{ $$ = alctree( "No local var block", 310 , 3, $1, $2, $3 ); }
      | LC RC 							{ $$ = alctree( "Empty block", 311, 2, $1, $2 ); }
      ;

NoLocalVariableBlockStatementList:
        NoLocalVariableBlockStatement       					{ $$ = $1; }
      | NoLocalVariableBlockStatementList NoLocalVariableBlockStatement		{ $$ = alctree( "No local var statement list", 320, 2, $1, $2 ); }
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
        StatementExpression Semicolon		{ $$ = alctree( "Expression Stmt", 370, 2, $1, $2 ); }
	| SwapExpression Semicolon		{ $$ = alctree( "Swap Expr Stmt", 371, 2, $1, $2 ); }
      ;

StatementExpression:
        Assignment       { $$ = $1; }
      | MethodInvocation       { $$ = $1; }
      ;

IfThenStatement:
        IF LP Expression RP Statement	{ $$ = alctree( "If-then", 390, 5, $1, $2, $3, $4, $5 ); }
      ;

IfThenElseStatement:
        IF LP Expression RP StatementNoShortIf 
            ELSE Statement		{ $$ = alctree( "if-then-else stmt", 400, 7, $1, $2, $3, $4, $5, $6 ); }
      ;

IfThenElseStatementNoShortIf:
        IF LP Expression RP StatementNoShortIf
            ELSE StatementNoShortIf		{ $$ = alctree( "if-then-else stmt (no short if)", 410, 7, $1, $2, $3, $4, $5, $6, $7 ); }
      ;

WhileStatement:
        WHILE LP Expression RP Statement	{ $$ = alctree( "while", 420, 5, $1, $2, $3, $4, $5 ); }
	| IDENT NoLocalVariableBlock WHILE LP Expression RP Semicolon				{ yyerror("syntax error"); fprintf(stderr, "do-while loops not supported in g0!\n"); exit(2); }
      ;

WhileStatementNoShortIf:
        WHILE LP Expression RP StatementNoShortIf		{ $$ = alctree( "While (No short if)", 430, 5, $1, $2, $3, $4, $5 ); }
      ;

ForInit:
        StatementExpressionList       { $$ = $1; }
      ;

ForUpdate:
        StatementExpressionList       { $$ = $1; }
      ;

ForStatement:
        FOR LP ForInit SM Expression SM ForUpdate RP Statement	{ $$ = alctree( "for stmt", 460, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9 ); }
      ;

ForStatementNoShortIf:
        FOR LP ForInit SM Expression SM ForUpdate RP StatementNoShortIf	{ $$ = alctree( "for stmt (no short if)", 470, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9 ); }
      ;

StatementExpressionList:
        StatementExpression       { $$ = $1; }
      | StatementExpressionList CM StatementExpression		{ $$ = alctree( "expression list", 480, 3, $1, $2, $3 ); }
      ;

EmptyStatement:
        Semicolon       { $$ = $1; }
      ;

BreakStatement:
        BREAK Semicolon		{ $$ = alctree( "break", 500, 2, $1, $2 ); }
      ;

ReturnStatement:
        RETURN Expression Semicolon		{ $$ = alctree( "return stmt", 510, 3, $1, $2, $3 ); }
      | RETURN Semicolon			{ $$ = alctree( "return stmt", 511, 2, $1, $2 ); }
      ;

ArgumentList:
	  Expression       { $$ = $1; }
	| ArgumentList CM Expression		{ $$ = alctree( "arg list", 520, 3, $1, $2, $3 ); }
	;

Primary:
     PrimaryNoNewArray       { $$ = $1; }
   ;

ArrayInitializer:
	PrimaryNoNewArray			{ $$ = $1; }
	| ArrayInitializer CM PrimaryNoNewArray { $$ = alctree( "Array Initializer List", 539, 3, $1, $2, $3 ); }
	;

PrimaryNoNewArray:
     Literal       { $$ = $1; }
   | LP Expression RP		{ $$ = alctree( "Paren Expr", 540, 3, $1, $2, $3 ); }
   | MethodInvocation       { $$ = $1; }
   | Assignable       { $$ = $1; }
   ;

FieldAccess:
     Primary DOT IDENT		{ $$ = alctree( "Field Access", 550, 3, $1, $2, $3 ); }
   ;

MethodInvocation:
     Name LP ArgumentList RP			{ $$ = alctree( "Method call", 560, 4, $1, $2, $3, $4 ); }
   | Name LP RP					{ $$ = alctree( "Method call", 561, 3, $1, $2, $3 ); }
   | Primary DOT IDENT LP ArgumentList RP	{ $$ = alctree( "Method call", 562, 6, $1, $2, $3, $4, $5, $6 ); }
   | Primary DOT IDENT LP  RP			{ $$ = alctree( "Method call", 563, 5, $1, $2, $3, $4, $5 ); }
   | CLASS_NAME LP RP			{ $$ = alctree( "Class Constructor Call", 564, 1, $1 ); }
   ;

ArrayAccess:
     PrimaryNoNewArray LB Expression RB		{ $$ = alctree( "Array Access", 570, 4, $1, $2, $3, $4 ); }
   ;

PostFixExpression:
     Primary       { $$ = $1; }
   | ConcatentationExpresssion { $$ = $1; }
   ;

UnaryExpression:
     MINUS UnaryExpression		{ $$ = alctree( "UnaryExpression", 590, 2, $1, $2 ); }
   | UnaryExpressionNotPlusMinus       { $$ = $1; }
   ;

UnaryExpressionNotPlusMinus:
     PostFixExpression       { $$ = $1; }
   | BANG UnaryExpression		{ $$ = alctree( "UnaryExpressionNo+-", 600, 2, $1, $2 ); }
   | DROLL UnaryExpression		{ $$ = alctree( "Unary Dice roll", 601, 2, $1, $2 ); }
   ;

MultiplicativeExpression:
     UnaryExpression       { $$ = $1; }
   | MultiplicativeExpression MUL UnaryExpression		{ $$ = alctree( "Multiplicative Expression", 610, 3, $1, $2, $3 ); }
   | MultiplicativeExpression DIV UnaryExpression		{ $$ = alctree( "Division Expression", 611, 3, $1, $2, $3 ); }
   | MultiplicativeExpression MOD UnaryExpression		{ $$ = alctree( "Modulus Expression", 612, 3, $1, $2, $3 ); }
   | MultiplicativeExpression DROLL UnaryExpression		{ $$ = alctree( "Dice Roll Expression", 613, 3, $1, $2, $3 ); }
   ;

ImplicitConcatExpression:
	  STRINGLITERAL Name				{ $$ = alctree( "Imp. Concat Expr (str + var)", 810, 2, $1, $2 ); }
	| STRINGLITERAL MethodInvocation			{ $$ = alctree( "Imp. Concat Expr (str + method)", 811, 2, $1, $2 ); }
	| Name STRINGLITERAL				{ $$ = alctree( "Imp. Concat Expr (var + str)", 812, 2, $1, $2 ); }
	| MethodInvocation STRINGLITERAL			{ $$ = alctree( "Imp. Concat Expr (method + str)", 813, 2, $1, $2 ); }
	// | Name Name						{ yyerror("syntax error"); fprintf(stderr, "implicit string concatenation must have a string literal in the first two values\n"); exit(2); }
	| STRINGLITERAL STRINGLITERAL				{ yyerror("syntax error"); fprintf(stderr, "implicit string concatenation not allowed between two string literals\n"); exit(2); }
	| ConcatentationExpresssion Name			{ $$ = alctree( "Imp. Concat Expr list (+name)", 814, 2, $1, $2 ); }
	| ConcatentationExpresssion MethodInvocation		{ $$ = alctree( "Imp. Concat Expr list (+method)", 815, 2, $1, $2 ); }
	| ConcatentationExpresssion STRINGLITERAL		{ $$ = alctree( "Imp. Concat Expr list (+string)", 816, 2, $1, $2 ); }
	;

ConcatentationExpresssion:
	  ImplicitConcatExpression 		{ $$ = $1; }
	// | ExplicitConcatExpression 		{ $$ = $1; }
	;

AdditiveExpression:
     MultiplicativeExpression       { $$ = $1; }
   | AdditiveExpression PLUS MultiplicativeExpression		{ $$ = alctree( "Sum Expr", 620, 3, $1, $2, $3 ); }
   | AdditiveExpression MINUS MultiplicativeExpression		{ $$ = alctree( "Difference Expr", 621, 3, $1, $2, $3 ); }
 //  | AdditiveExpression MultiplicativeExpression		{ $$ = alctree( "Implicit Concat.", 622, 2, $1, $2 ); }
   ;

RelationalExpression:
     AdditiveExpression       { $$ = $1; }
   | RelationalExpression LT AdditiveExpression		{ $$ = alctree( "less Expr", 630, 3, $1, $2, $3 ); }
   | RelationalExpression LE AdditiveExpression		{ $$ = alctree( "less/eq Expr", 631, 3, $1, $2, $3 ); }
   | RelationalExpression GT AdditiveExpression		{ $$ = alctree( "great Expr", 632, 3, $1, $2, $3 ); }
   | RelationalExpression GE AdditiveExpression		{ $$ = alctree( "great/eq Expr", 633, 3, $1, $2, $3 ); }
   ;

EqualityExpression:
     RelationalExpression       { $$ = $1; }
   | EqualityExpression EQ RelationalExpression		{ $$ = alctree( "'==' Expr", 640, 3, $1, $2, $3 ); }
   | EqualityExpression NE RelationalExpression		{ $$ = alctree( "'!=' Expr", 641, 3, $1, $2, $3 ); }
   ;

ConditionalAndExpression:
     EqualityExpression       { $$ = $1; }
   | ConditionalAndExpression ANDAND EqualityExpression		{ $$ = alctree( "AND", 650, 3, $1, $2, $3 ); }
   ;

ConditionalOrExpression:
     ConditionalAndExpression       { $$ = $1; }
   | ConditionalOrExpression OROR ConditionalAndExpression		{ $$ = alctree( "OR", 660, 3, $1, $2, $3 ); }
   ;

AssignmentExpression:
     ConditionalOrExpression       { $$ = $1; }
   | Assignment       { $$ = $1; }
   ;

Assignment:
     Assignable AssignmentOperator ConditionalOrExpression		{ $$ = alctree( "Assign", 690, 3, $1, $2, $3 ); }
     | Assignable AssignmentOperator Assignment				{ $$ = alctree( "Recursive Assign", 691, 3, $1, $2, $3 ); }
     | Assignable AssignmentOperator LC ArrayInitializer RC       { $$ = alctree( "Array Initializer", 692, 5, $1, $2, $3, $4, $5 ); }
   ;

SwapExpression:
     Assignable SWAP Assignable		{ $$ = alctree( "Swap", 680, 3, $1, $2, $3 ); }
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
       AssignmentExpression       { $$ = $1; }
   ;
		

FormalParameterList:
        FormalParameter       { $$ = $1; }
      | FormalParameterList CM FormalParameter		{ $$ = alctree( "Named param list", 730, 3, $1, $2, $3 ); }
      ;

FormalParameter:
        Type VariableDeclaratorId		{ $$ = alctree( "named parameter", 740, 2, $1, $2 ); }
      ;

LocalVariable:
        Type VariableDeclaratorList Semicolon		{ $$ = alctree( "local variable", 750, 3, $1, $2, $3 ); }
      ;

VariableDeclaratorId:
        IDENT 		{ $$ = $1; }
      | VariableDeclaratorId LB RB		{ $$ = alctree( "variable declarator", 760, 3, $1, $2, $3 ); }
      ;

Name:
        SimpleName       { $$ = $1; }
      | QualifiedName       { $$ = $1; }
      ;

SimpleName:
        IDENT		{ $$ = $1; }
      ;

QualifiedName:
        Name DOT IDENT		{ $$ = alctree( "qualified name", 790, 3, $1, $2, $3 ); }
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