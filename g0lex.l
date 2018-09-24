%{
/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #2: Syntax Analysis
"g0.l"
	Lex Specification
	Used for lexical analysis. (AKA Scanning)
	Creates token nodes for BISON to place into the tree.
*/
%}


O                       [0-7]
D                       [0-9]
L                       [a-zA-Z_]
H                       [a-fA-F0-9]
E                       [Ee][+-]?{D}+
FS                      (f|F|l|L)
IS                      (u|U|l|L)
W                       [ \t\f]*
LIT                     \"(\\.|[^\\"])*\"
ID						{L}({L}|{D})* 

%{
#include "token.h"
#include "tree.h"
#include "symt.h"
#include "y.tab.h"

void comment();
int yytok( int i );
void yyerror(char*);


extern char* filename;	// main.c

struct token* yytoken = NULL;
// int yylineno = 1;
%}

%%

"/*"                    { comment(); }
"//".*					{ ++yylineno; /* Do Nothing, Eat up rest of line. */ }
^{W}#.*\n               { yyerror("Godiva has no #preprocessor direcives. Are you feeding it C/C++ code?"); }

\n                      { ++yylineno; }
[ \t\f]+                { }

"null"                  { return yytok(NULLLITERAL); }
"false"                 { return yytok(FALSE); }
"true"                  { return yytok(TRUE); }
"break"                 { return yytok(BREAK); }
"class"                 { return yytok(CLASS); }
"double"                { return yytok(DOUBLE); }
"else"                  { return yytok(ELSE); }
"for"                   { return yytok(FOR); }
"if"                    { return yytok(IF); }
"int"                   { return yytok(INT); }
"list"						{ return yytok(LIST); }
"return"                { return yytok(RETURN); }
"d"/({ID}|{D}*)			{ return yytok(DROLL); }
"string"						{ return yytok(STRING); }
"table"						{ return yytok(TABLE); }
"void"                  { return yytok(VOID); }
"while"                 { return yytok(WHILE); }
"bool"                  { return yytok(BOOL); }

{ID}          				{ return yytok(IDENT); }

0[xX]{H}+{IS}?          { return yytok(INTLITERAL); }
0{O}+{IS}?              { return yytok(INTLITERAL); }
{D}+{IS}?               { return yytok(INTLITERAL); }

'(\\.|[^\\'])+'         { return yytok(CHARLITERAL); }

{D}+{E}{FS}?            { return yytok(FLOATLITERAL); }
{D}*"."{D}+({E})?{FS}?  { return yytok(FLOATLITERAL); }
{D}+"."{D}*({E})?{FS}?  { return yytok(FLOATLITERAL); }

{LIT}                   { return yytok(STRINGLITERAL); }

"("                     { return yytok(LP); }
")"                     { return yytok(RP); }
"{"                     { return yytok(LC); }
"}"                     { return yytok(RC); }
"["                     { return yytok(LB); }
"]"                     { return yytok(RB); }
";"                     { return yytok(SM); }
","                     { return yytok(CM); }
"."                     { return yytok(DOT); }

"="                     { return yytok(ASN); }
"<"                     { return yytok(LT); }
">"                     { return yytok(GT); }
"!"                     { return yytok(BANG); }
"=="                    { return yytok(EQ); }
"!="                    { return yytok(NE); }
"<="                    { return yytok(LE); }
">="                    { return yytok(GE); }
"&&"                    { return yytok(ANDAND); }
"||"                    { return yytok(OROR); }
"+"                     { return yytok(PLUS); }
"-"                     { return yytok(MINUS); }
"*"                     { return yytok(MUL); }
"/"                     { return yytok(DIV); }
"%"                     { return yytok(MOD); }
"+="                    { return yytok(PLASN); }
"-="                    { return yytok(MIASN); }
"#"                     { return yytok(SHARP); }

.                       {  }

%%

char *yyfilename = NULL;
int yychar;
int errors;

char* convertEscapes( const char* s ) {
	/* 
	This function takes a string, s, which is a raw text of the yytext
		and makes a copy of it, removes quotes and unescaped characters, 
		and returns the final string.
	*/
	int oldLength = strlen(s);
	
	// Dont do it this way, because it just wastes time copying all the data when
	//	we already have to do that. So just calloc the space.
	char* t = strdup(s);
	if (t == NULL) {
		perror("Failed to allocate memory for strdup in function convertEscapes");
		exit(-1);
	}
	int i = 0;	// counter for old string
	int j = 0;	// counter for new string
	for( ; i < oldLength ; ++i ){
		switch (s[i]) {
			case '"':
			case '\'':
				// skip copying quotes
				break;
			case '\\':
				// Escape found, replace with appropriate value
				++i;	// skip past first '\'
				int c = 0;
				switch (s[i]) {
					case 'n': 
						c = '\n';
						break;
					case 't': 
						c = '\t';
						break;
					case '\\': 
						c = '\\';
						break;
					case '"': 
						c = '\"';
						break;
					// Consider adding more escape characters here.
					default:
						perror("Unknown fringe case in function convertEscapes");
						exit(-1);
				}
				// Place character in correct position in new string.
				t[j++] = c;
				break;
			default:
				// Copy characters into new string.
				t[j++] = s[i];
		}
	}
	t[j] = '\0';	// NULL terminate the string
	// Now that copying the string is done, resize the string
	int newLength = strlen(t) + 1;
	
	t = realloc( t, sizeof(char) * newLength );
	if (t == NULL) {
		perror("Failed to reallocate memory for strdup in function convertEscapes");
		exit(-1);
	}
	return t;
}

int yytok( int i )
{
	/* 
	This function takes a token category and creates a token structure to
	hold the new token data. String data is all copied, so the token is
	immediately ready to use. 
	After generating the token structure, the token is placed in the variable
	"yytoken", and the token category is returned. This token should be handled immediately
	to ensure no memory leaks occur by never freeing that memory.
	Main is responsible for putting the token into the list.
	
	Uses:
		yytext
		yylineno
		filename - name of file given by the main.c program,
		yylval
		
	*/
	
	struct token* t = calloc( 1, sizeof(struct token) );
	if ( t == NULL ) 
	{
		perror("Failed to allocate memory for token!");
		exit(-1);
	}

	if ( i == IDENT )
	{
		// Identifier found.
		struct Identifier* p = lookUp( yytext );
		if ( p == NULL )
		{
			// New identifier
			// yylval still holds the tree node for the previous token.
			p = addIdentifier( yytext, yylval.node->code, yylineno );
		}
		// Construct token.
		t->lineno = yylineno;
		t->tval = p;
		t->text = p->name;
		t->filename = filename;
		// If typeCode of token before identifier was first declared
		// was of type "CLASS", then make this variable a class name.
		t->category = (p->typeCode == CLASS ) ? CLASS_NAME : IDENT;

		// Return proper integer code.
		yylval.node = addLeaf(t->category, t);
		return t->category;
	}

	t->category = i;
	t->text = strdup(yytext);
	t->lineno = yylineno;
	t->filename = filename;
	switch (i) {
		case STRINGLITERAL:
		case CHARLITERAL:
			t->sval = convertEscapes( yytext );
			break;
		case INTLITERAL:
			t->ival = atoi(yytext);
			break;
		case FLOATLITERAL:
			t->dval = atof(yytext);
			break;
		// default:
			// yyerror("Unknown yylval for given integer category.");
	}
	
	// Give token to YACC
	// yytoken = t;
	yylval.node = addLeaf(i, t);
	return i;
}


// I feel like theres no way this really works, this also might hurt me because
// yyerror might be defined by lex if i dont provide a definition for it, 
// which this is doing.
void yyerror(char *s)
{
   fprintf(stderr, "%s:%d: %s before '%s' token\n",
	   yyfilename, yylineno, s, yytext);
}

void comment()
{
	
	int splat_seen = 0;

	for(;;) {
	   int ch = input();
	   switch (ch) {
			default:
				splat_seen = 0;
				break;
			case '*':
				splat_seen = 1;
				break;
			case '/':
				if(splat_seen)
					return;
				else
					splat_seen = 0;
				break;
			case '\n':
				++yylineno;
				break;
			case 0: {
			   exit(-(++errors));
			   }
	   }

	}

}

int yywrap() { return 1; }
