/*
 * g0 Grammar
 * Derived from the Godiva Grammar, which was in turn
 * based on the Java grammar in Gosling/Joy/Steele, Chapter 19
 */

%{
#include <stdio.h>
#include <stdlib.h> //exit(),
#include "tree.h"
// #include ""

int yydebug = 0;
tree* yytree = NULL;
extern char* yyfilename;  // g0lex.l
extern int yylineno;
extern char* yytext;
// extern void yyerror(char* s); //g0lex.l
void yyerror( char* s );
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
%type < node > ArgumentList ExpressionOpt DefaultTableMapping
%type < node > ConditionalAndExpression 
%type < node > Literal  ConcatentationExpresssion ImplicitConcatExpression
%type < node > LocalVariable ListInitializer ListLiteral
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

 */
%start Program

/* All the codes for these trees are the line # of the rule + 500,
This prevents collisions with YACC token defines.*/

%%



Program:
        CompilationUnits { yytree = $1; }
      ;

GlobalVariable:
        Type VariableDeclaratorList Semicolon		{ $$ = alctree( "global var", 613, 3, $1, $2, $3 ); }
      ;

VariableDeclaratorList:
        VariableDeclaratorId       				{ $$ = $1; }
      | VariableDeclaratorList CM VariableDeclaratorId		{ $$ = alctree( "var list", 618, 2, $1, $3 ); }
      ;

Type:
        CLASS_NAME       { $$ = $1; }
      | PrimitiveType       { $$ = $1; }
      | ListType       { $$ = $1; }
      | TableType       { $$ = $1; }
      ;

ListType:
        LIST				{ $$ = $1; }
      | LIST LT Type GT	{ $$ = alctree( "list", 630, 2, $1, $3); }
      ;

TableType:
        TABLE						{ $$ = $1; }
      | TABLE LT Type GT			{ $$ = alctree( "table", 635, 2, $1, $3); }
      | TABLE LT Type CM Type GT	{ $$ = alctree( "table", 636, 3, $1, $3, $5 ); }
      ;

PrimitiveType:
        INT		{ $$ = $1; }
      | BOOL		{ $$ = $1; }
      | DOUBLE		{ $$ = $1; }
      | STRING		{ $$ = $1; }
      | VOID
      ;

CompilationUnits:
        CompilationUnit				{ $$ = $1; }
      | CompilationUnits CompilationUnit	{ $$ = alctree( "Compilation Units", 655, 2, $1, $2 ); }
      ;

CompilationUnit:
        ClassDeclaration		{ $$ = $1; }
      | Function			{ $$ = $1; }
      | AssignmentExpression Semicolon		{ $$ = $1; }
      | GlobalVariable Semicolon			{ $$ = $1; }
      ;

ClassDeclaration:
        ClassHeader ClassBlock		{ $$ = alctree( "Class declaration", 659, 2, $1, $2 ); }
      ;

ClassHeader:
        CLASS CLASS_NAME			{ $$ = alctree( "Class header", 663, 1, $2 ); }
      ;

ClassBlock:
        LC ClassBlockUnitList RC		{ $$ = alctree( "Class Block", 667, 3, $1, $2, $3 ); }
      | LC RC					{ $$ = alctree( "Empty class block", 668, 2, $1, $2 ); }
      ;

ClassVariableList:
	 IDENT		{ $$ = $1; }
	| ClassVariableList CM IDENT		{ $$ = alctree( "Class variable list", 673, 2, $1, $3 ); }
	;

ClassVariable:
      Type ClassVariableList Semicolon	{ $$ = alctree( "Class variable", 677, 2, $1, $2 ); }
      | Type Assignment { yyerror("syntax error\nInitializers not allowed in declarations"); }
      ;

ClassBlockUnitList:
        ClassBlockUnit		{ $$ = $1; }
      | ClassBlockUnitList ClassBlockUnit	{ $$ = alctree( "Class Block statements", 683, 2, $1, $2 ); }
      ;

ClassBlockUnit:
        ClassVariable			{ $$ = $1; }
      | MethodDeclaration		{ $$ = $1; }
      | ConstructorDeclaration		{ $$ = $1; }
      ;

MethodDeclaration:
        MethodHeader MethodBody		{ $$ = alctree( "Method", 693, 2, $1, $2 ); }
      ;

MethodHeader:
        Type MethodDeclarator		{ $$ = alctree( "Method header", 697, 2, $1, $2 ); }
      // | VOID MethodDeclarator		{ $$ = alctree( "Method header", 698, 2, $1, $2 ); }
      ;

MethodDeclarator:
        IDENT LP FormalParameterList RP	{ $$ = alctree( "Method declarator", 702, 2, $1, $3); }
      | IDENT LP RP			{ $$ = alctree( "Method declarator", 703, 1, $1 ); }
      ;

MethodBody:
        Block       { $$ = $1; }
      ;

ConstructorDeclaration:
         ConstructorDeclarator ConstructorBody		{ $$ = alctree( "Constructor delcaration", 711, 2, $1, $2 ); }
      ;

ConstructorDeclarator:
        CLASS_NAME LP FormalParameterList RP	{ $$ = alctree( "Constructor Header", 715, 2, $1, $3 ); }
      | CLASS_NAME LP RP			{ $$ = alctree( "Constructor Header", 716, 1, $1 ); }
      ;

ConstructorBody:
        Block       { $$ = $1; }
      ;

Function:
        FunctionPrototype       { $$ = $1; }
      | FunctionDefinition       { $$ = $1; }
      ;

FunctionPrototype:
        Type IDENT LP TypeList RP Semicolon	{ $$ = alctree( "func proto", 729, 3, $1, $2, $4 ); }
      | Type IDENT LP RP Semicolon		{ $$ = alctree( "func proto", 730, 2, $1, $2 ); }
      // | VOID IDENT LP TypeList RP Semicolon	{ $$ = alctree( "func proto", 731, 3, $1, $2, $4 ); }
      // | VOID IDENT LP RP Semicolon		{ $$ = alctree( "func proto", 732, 2, $1, $2 ); }
      ;

TypeList:
        Type      			 { $$ = $1; }
      | TypeList CM Type		{ $$ = alctree( "Type list", 737, 2, $1, $3 ); }
      ;

FunctionDefinition:
        Type IDENT LP FormalParameterList RP FunctionBody	{ $$ = alctree( "func defn", 741, 4, $1, $2, $4, $6 ); }
      | Type IDENT LP RP FunctionBody				{ $$ = alctree( "func defn", 742, 3, $1, $2, $5 ); }
      // | VOID IDENT LP FormalParameterList RP FunctionBody	{ $$ = alctree( "func defn", 743, 4, $1, $2, $4, $6 ); }
      // | VOID IDENT LP RP FunctionBody				{ $$ = alctree( "func defn", 744, 3, $1, $2, $5 ); }
      ;

FunctionBody:
        Block       { $$ = $1; }
      ;

Block:
        LC BlockStatementList RC 		{ $$ = alctree( "Block", 752, 1, $2 ); }
      | LC RC 					{ $$ = alctree( "Empty Block", 753, 1, NULL ); }
      ;

BlockStatementList:
        BlockStatement       			{ $$ = $1; }
      | BlockStatementList BlockStatement		{ $$ = alctree( "Block stmt list", 758, 2, $1, $2 ); }
      ;

BlockStatement:
        LocalVariable    				   { $$ = $1; }
      | Statement       				{ $$ = $1; }
      ;

NoLocalVariableBlock:
        LC NoLocalVariableBlockStatementList RC 		{ $$ = alctree( "No local var block", 767 , 3, $1, $2, $3 ); }
      | LC RC 							{ $$ = alctree( "Empty block", 768, 2, $1, $2 ); }
      ;

NoLocalVariableBlockStatementList:
        NoLocalVariableBlockStatement       					{ $$ = $1; }
      | NoLocalVariableBlockStatementList NoLocalVariableBlockStatement		{ $$ = alctree( "No local var statement list", 773, 2, $1, $2 ); }
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
        StatementExpression Semicolon		{ $$ = alctree( "Expression Stmt", 804, 1, $1 ); }
	    | SwapExpression Semicolon		{ $$ = alctree( "Swap Expr Stmt", 805, 1, $1 ); }
      ;

StatementExpression:
        Assignment       { $$ = $1; }
      | MethodInvocation       { $$ = $1; }
      ;

IfThenStatement:
        IF LP Expression RP Statement	{ $$ = alctree( "If-then", 814, 5, $1, $2, $3, $4, $5 ); }
      ;

IfThenElseStatement:
        IF LP Expression RP StatementNoShortIf 
            ELSE Statement		{ $$ = alctree( "if-then-else stmt", 819, 7, $1, $2, $3, $4, $5, $6 ); }
      ;

IfThenElseStatementNoShortIf:
        IF LP Expression RP StatementNoShortIf
            ELSE StatementNoShortIf		{ $$ = alctree( "if-then-else stmt (no short if)", 824, 7, $1, $2, $3, $4, $5, $6, $7 ); }
      ;

WhileStatement:
        WHILE LP Expression RP Statement	{ $$ = alctree( "while", 828, 5, $1, $2, $3, $4, $5 ); }
	| IDENT NoLocalVariableBlock WHILE LP Expression RP Semicolon				{ yyerror("syntax error"); fprintf(stderr, "do-while loops not supported in g0!\n"); exit(2); }
      ;

WhileStatementNoShortIf:
        WHILE LP Expression RP StatementNoShortIf		{ $$ = alctree( "While (No short if)", 833, 5, $1, $2, $3, $4, $5 ); }
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
        FOR LP ForInit SM ExpressionOpt SM ForUpdate RP Statement	{ $$ = alctree( "for stmt", 852, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9 ); }
      ;

ForStatementNoShortIf:
        FOR LP ForInit SM ExpressionOpt SM ForUpdate RP StatementNoShortIf	{ $$ = alctree( "for stmt (no short if)", 856, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9 ); }
      ;

StatementExpressionList:
        StatementExpression       { $$ = $1; }
      | StatementExpressionList CM StatementExpression		{ $$ = alctree( "expression list", 861, 3, $1, $2, $3 ); }
      ;

EmptyStatement:
        Semicolon       { $$ = $1; }
      ;

BreakStatement:
        BREAK Semicolon		{ $$ = alctree( "break", 869, 2, $1, $2 ); }
      ;

ReturnStatement:
        RETURN Expression Semicolon		{ $$ = alctree( "return stmt", 873, 3, $1, $2, $3 ); }
      | RETURN Semicolon			{ $$ = alctree( "return stmt", 874, 2, $1, $2 ); }
      ;

ArgumentList:
	  Expression       { $$ = alctree( "arg list", 878, 1, $1 ); }
	| ArgumentList CM Expression		{ $$ = alctree( "arg list", 879, 2, $1, $3 ); }
	;

Primary:
     PrimaryNoNewArray  //| SHARP PrimaryNoNewArray { $$ = alctree( "Size of symbol", 883, 1, $2); }
   ;

ListInitializer: PrimaryNoNewArray CM PrimaryNoNewArray			{ $$ = alctree( "List items", 888, 2, $1, $3 ); }
  // |   PrimaryNoNewArray CM ListInitializer { $$ = alctree( "List items", 888, 2, $1, $3 ); }
  | ListInitializer CM PrimaryNoNewArray { $$ = alctree( "List items", 888, 2, $1, $3 ); }
	;

PrimaryNoNewArray:
     Literal       { $$ = $1; }
   | LP Expression RP		{ $$ = alctree( "Paren Expr", 893, 1, $2 ); }
   | MethodInvocation       { $$ = $1; }
   | Assignable       { $$ = $1; }
   | SHARP IDENT    { $$ = alctree( "List Size", 883, 1, $2); } // FIX move this to Primary, or somewhere where it is less common than here.
   | PrimaryNoNewArray LB Expression COLON Expression RB		{ $$ = alctree( "List Substring", 897, 3, $1, $3, $5 ); }
   ;

FieldAccess:
     PrimaryNoNewArray DOT IDENT		{ $$ = alctree( "Field Access", 901, 2, $1, $3 ); }
   ;

MethodInvocation:
     Name LP ArgumentList RP			{ $$ = alctree( "Method call", 905, 2, $1, $3 ); }
   | Name LP RP					{ $$ = alctree( "Method call", 906, 1, $1 ); }
   | PrimaryNoNewArray DOT IDENT LP ArgumentList RP	{ $$ = alctree( "Method call", 907, 3, $1, $3, $5 ); }
   | PrimaryNoNewArray DOT IDENT LP  RP			{ $$ = alctree( "Method call", 908, 2, $1, $3 ); }
   | CLASS_NAME LP RP			{ $$ = alctree( "Class Constructor Call", 909, 1, $1 ); }
   ;

ArrayAccess:
     PrimaryNoNewArray LB Expression RB		{ $$ = alctree( "Array Access", 913, 2, $1, $3 ); }
   ;

DefaultTableMapping:
     PrimaryNoNewArray LB RB		{ $$ = alctree( "Default table mapping", 917, 1, $1 ); }
   ;

PostFixExpression:
     Primary       { $$ = $1; }
   | ConcatentationExpresssion { $$ = $1; }
   ;

UnaryExpression:
     MINUS UnaryExpression		{ $$ = alctree( "UnaryExpression", 926, 1, $2 ); }
   | UnaryExpressionNotPlusMinus       { $$ = $1; }
   ;

UnaryExpressionNotPlusMinus:
     PostFixExpression       { $$ = $1; }
   | BANG UnaryExpression		{ $$ = alctree( "UnaryExpressionNo+-", 932, 1, $2 ); }
   | DROLL UnaryExpression		{ $$ = alctree( "Unary Dice roll", 933, 1, $2 ); }
   ;

MultiplicativeExpression:
     UnaryExpression       { $$ = $1; }
   | MultiplicativeExpression MUL UnaryExpression		{ $$ = alctree( "Multiplicative Expression", 938, 3, $1, $2, $3 ); }
   | MultiplicativeExpression DIV UnaryExpression		{ $$ = alctree( "Division Expression", 939, 3, $1, $2, $3 ); }
   | MultiplicativeExpression MOD UnaryExpression		{ $$ = alctree( "Modulus Expression", 940, 3, $1, $2, $3 ); }
   | MultiplicativeExpression DROLL UnaryExpression		{ $$ = alctree( "Dice Roll Expression", 941, 3, $1, $2, $3 ); }
   ;

ImplicitConcatExpression: PrimaryNoNewArray PrimaryNoNewArray { $$ = alctree( "Imp. Concat Expr (str + var)", 944, 2, $1, $2 ); }
      | ConcatentationExpresssion PrimaryNoNewArray { $$ = alctree( "Imp. Concat Expr (str + var)", 945, 2, $1, $2 ); }
	//   STRINGLITERAL Name				{ $$ = alctree( "Imp. Concat Expr (str + var)", 945, 2, $1, $2 ); }
	// | STRINGLITERAL MethodInvocation			{ $$ = alctree( "Imp. Concat Expr (str + method)", 946, 2, $1, $2 ); }
	// | Name STRINGLITERAL				{ $$ = alctree( "Imp. Concat Expr (var + str)", 947, 2, $1, $2 ); }
	// | MethodInvocation STRINGLITERAL			{ $$ = alctree( "Imp. Concat Expr (method + str)", 948, 2, $1, $2 ); }
	// | Name Name						{ $$ = alctree( "Imp. Concat Expr (var + var)", 949, 2, $1, $2); }
	// | STRINGLITERAL STRINGLITERAL				{ yyerror("syntax error"); fprintf(stderr, "implicit string concatenation not allowed between two string literals\n"); exit(2); }
	// | ConcatentationExpresssion Name			{ $$ = alctree( "Imp. Concat Expr list (+name)", 952, 2, $1, $2 ); }
	// | ConcatentationExpresssion MethodInvocation		{ $$ = alctree( "Imp. Concat Expr list (+method)", 953, 2, $1, $2 ); }
	// | ConcatentationExpresssion STRINGLITERAL		{ $$ = alctree( "Imp. Concat Expr list (+string)", 954, 2, $1, $2 ); }
	;

ConcatentationExpresssion:
	  ImplicitConcatExpression 		{ $$ = $1; }
	;

AdditiveExpression:
     MultiplicativeExpression       { $$ = $1; }
   | AdditiveExpression PLUS MultiplicativeExpression		  { $$ = alctree( "Sum Expr", 964, 3, $1, $2, $3 ); }
   | AdditiveExpression MINUS MultiplicativeExpression		{ $$ = alctree( "Difference Expr", 965, 3, $1, $2, $3 ); }
   ;

RelationalExpression:
     AdditiveExpression       { $$ = $1; }
   | RelationalExpression LT AdditiveExpression		{ $$ = alctree( "less Expr", 970, 3, $1, $2, $3 ); }
   | RelationalExpression LE AdditiveExpression		{ $$ = alctree( "less/eq Expr", 971, 3, $1, $2, $3 ); }
   | RelationalExpression GT AdditiveExpression		{ $$ = alctree( "great Expr", 972, 3, $1, $2, $3 ); }
   | RelationalExpression GE AdditiveExpression		{ $$ = alctree( "great/eq Expr", 973, 3, $1, $2, $3 ); }
   ;

EqualityExpression:
     RelationalExpression       { $$ = $1; }
   | EqualityExpression EQ RelationalExpression		{ $$ = alctree( "'==' Expr", 978, 3, $1, $2, $3 ); }
   | EqualityExpression NE RelationalExpression		{ $$ = alctree( "'!=' Expr", 979, 3, $1, $2, $3 ); }
   ;

ConditionalAndExpression:
     EqualityExpression       { $$ = $1; }
   | ConditionalAndExpression ANDAND EqualityExpression		{ $$ = alctree( "AND", 984, 3, $1, $2, $3 ); }
   ;

ConditionalOrExpression:
     ConditionalAndExpression       { $$ = $1; }
   | ConditionalOrExpression OROR ConditionalAndExpression		{ $$ = alctree( "OR", 989, 3, $1, $2, $3 ); }
   ;

AssignmentExpression:
     ConditionalOrExpression       { $$ = $1; }
   | Assignment       { $$ = $1; }
   ;

Assignment:
     Assignable AssignmentOperator AssignmentExpression		{ $$ = alctree( "Assign", 998, 3, $1, $2, $3 ); }
    | Assignable AssignmentOperator ListLiteral       { $$ = alctree( "List Initializer", 999, 3, $1, $2, $3); }  // Covered by ListLiteral being added to literal.
    //  | Name LB RB ASN AssignmentExpression  { $$ = alctree( "Default table mapping", 1000, 2, $1, $5 ); }
   ;

SwapExpression:
      Assignable SWAP Assignable		{ $$ = alctree( "Swap", 1004, 2, $1, $3 ); }
    | Assignable SWAP Assignment		{ $$ = alctree( "Swap w/ Assign", 1005, 2, $1, $3 ); }
    | Assignable SWAP SwapExpression		{ $$ = alctree( "Multiple Swap", 1006, 2, $1, $3 ); }
    ;

Assignable:
     Name       
   | FieldAccess       
   | ArrayAccess       
   | DefaultTableMapping
  //  | Name LB RB         { $$ = alctree( "Default table mapping", 1014, 1, $1 ); }
   ;

AssignmentOperator:
     ASN		{ $$ = $1; }
   | PLASN		{ $$ = $1; }
   | MIASN		{ $$ = $1; }
   ;

Expression:
        SwapExpression
      | AssignmentExpression
      // | Name LB RB ASN AssignmentExpression  { $$ = alctree( "Default table mapping", 1026, 2, $1, $5 ); }
      // | 
      ;
		

FormalParameterList:
        FormalParameter       { $$ = $1; }
      | FormalParameterList CM FormalParameter		{ $$ = alctree( "Named param list", 1033, 2, $1, $3 ); }
      ;

FormalParameter:
        Type VariableDeclaratorId		{ $$ = alctree( "named parameter", 1037, 2, $1, $2 ); }
      ;

LocalVariable:
        Type VariableDeclaratorList Semicolon		{ $$ = alctree( "local variable", 1041, 2, $1, $2 ); }
      ;

VariableDeclaratorId:
        IDENT 		{ $$ = $1; }
      | VariableDeclaratorId LB RB		{ $$ = alctree( "variable declarator", 1046, 3, $1, $2, $3 ); }
      ;

Name:
        SimpleName       { $$ = $1; }
      | QualifiedName       { $$ = $1; }
      ;

SimpleName:
        IDENT		{ $$ = $1; }
      ;

QualifiedName:
        Name DOT IDENT		{ $$ = alctree( "qualified name", 1059, 2, $1, $3 ); }
      ;

BoolLiteral:
        TRUE		{ $$ = $1; }
      | FALSE		{ $$ = $1; }
      ;

Semicolon:
        SM		{ $$ = $1; }
      | { $$ = NULL; }
      ;
      
ListLiteral:  LB ListInitializer RB { $$ = alctree( "list literal", 1572, 1, $2 ); }
    |   LB PrimaryNoNewArray RB { $$ = alctree( "list literal", 1573, 1, $2 ); }
    ;

Literal:
		  INTLITERAL		{ $$ = $1; }   
		| FLOATLITERAL		{ $$ = $1; }
		| BoolLiteral       { $$ = $1; }
		| STRINGLITERAL		{ $$ = $1; }
		| CHARLITERAL		{ $$ = $1; }
		| NULLLITERAL		{ $$ = $1; }
    // | ListLiteral
		;

/*
 * the start symbol, Goal, may seem to be here for rhetorical purposes,
 * but it is also the ideal spot to insert a semantic action that passes
 * the completed parse tree to a later phase of compilation.

ProductionRule:
        
      | 
      ;

 */

%%

void yyerror( char* s ){
  fprintf(stderr, "Syntax error:\n%s:%d: %s before '%s' token\n",
	   yyfilename, yylineno, s, yylval.node->token->text);
	exit(2);
}