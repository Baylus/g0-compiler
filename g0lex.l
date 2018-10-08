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
#include "g0gram.h"

void comment();
int lex( int, int, int);
int yytok( int i );
// void yyerror(char*, int);
void yyerror(char*);

extern int yydebug;

struct token* lastToken = NULL;

extern char* filename;	// main.c

struct token* yytoken = NULL;
// int yylineno = 1;
// Boolean value to store the 

int firstOnLine = 0;

%}

%%

"/*"                    { comment(); }
"//".*					{ /* Do Nothing, Eat up rest of line. */ }
^{W}#.*\n               { yyerror("Godiva has no #preprocessor direcives. Are you feeding it C/C++ code?"); }

\n                      { ++yylineno; firstOnLine = 1;  }
[ \t\f\r]+                { }

"null"                  { return lex( 0, 1, NULLLITERAL); }
"false"                 { return lex( 0, 1, FALSE); }
"true"                  { return lex( 0, 1, TRUE); }
"break"                 { return lex( 0, 1, BREAK); }
"class"                 { return lex( 1, 0, CLASS); }
"double"                { return lex( 1, 0, DOUBLE); }
"else"                  { return lex( 0, 0, ELSE); }
"for"                   { return lex( 1, 0, FOR); }
"if"                    { return lex( 1, 0, IF); }
"int"                   { return lex( 1, 0, INT); }
"list"						{ return lex( 1, 0, LIST); }
"return"                { return lex( 1, 1, RETURN); }
d/({D}*)						{ return lex( 0, 0, DROLL); }
"string"						{ return lex( 1, 0, STRING); }
"table"						{ return lex( 1, 0, TABLE); }
"void"                  { return lex( 1, 0, VOID); }
"while"                 { return lex( 1, 0, WHILE); }
"bool"                  { return lex( 1, 0, BOOL); }

{ID}          				{ return lex( 1, 1, IDENT); }

0[xX]{H}+{IS}?          { return lex( 0, 1, INTLITERAL); }
0{O}+{IS}?              { return lex( 0, 1, INTLITERAL); }
{D}+{IS}?               { return lex( 0, 1, INTLITERAL); }

'(\\.|[^\\'])+'         { return lex( 0, 1, CHARLITERAL); }

{D}+{E}{FS}?            { return lex( 0, 1, FLOATLITERAL); }
{D}*"."{D}+({E})?{FS}?  { return lex( 0, 1, FLOATLITERAL); }
{D}+"."{D}*({E})?{FS}?  { return lex( 0, 1, FLOATLITERAL); }

{LIT}                   { return lex( 0, 1, STRINGLITERAL); }

"("                     { return lex( 0, 0, LP); }
")"                     { return lex( 0, 1, RP); }
"{"                     { return lex( 0, 0, LC); }
"}"                     { return lex( 0, 0, RC); }
"["                     { return lex( 0, 0, LB); }
"]"                     { return lex( 0, 1, RB); }
";"                     { return lex( 0, 0, SM); }
":"                     { return lex( 0, 0, COLON); }
","                     { return lex( 0, 0, CM); }
"."                     { return lex( 0, 0, DOT); }

"="                     { return lex( 0, 0, ASN); }
"<"                     { return lex( 0, 0, LT); }
">"                     { return lex( 0, 0, GT); }
"!"                     { return lex( 0, 0, BANG); }
"=="                    { return lex( 0, 0, EQ); }
"!="                    { return lex( 0, 0, NE); }
"<="                    { return lex( 0, 0, LE); }
">="                    { return lex( 0, 0, GE); }
"&&"                    { return lex( 0, 0, ANDAND); }
"||"                    { return lex( 0, 0, OROR); }
"+"                     { return lex( 0, 0, PLUS); }
"-"                     { return lex( 0, 0, MINUS); }
"*"                     { return lex( 0, 0, MUL); }
"/"                     { return lex( 0, 0, DIV); }
"%"                     { return lex( 0, 0, MOD); }
"+="                    { return lex( 0, 0, PLASN); }
"-="                    { return lex( 0, 0, MIASN); }
":=:"							{ return lex( 0, 0, SWAP); }
"#"                     { return lex( 0, 0, SHARP); }

.                       { yyerror("lexical error"); }

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

int lex(int validBeginner, int validEnder, int type)
{
	/*
		Handles some semi-colon insertion. 


	 */
	
	static int beginner = 0, ender = 0;
	beginner = validBeginner;
	// printf("Scanner: ", yytext);

	// If we saved last token and returned a semi-colon last time
	if(lastToken != NULL)	
	{	// THEN return saved token
		int tcat = lastToken->category;
		yylval.node = addLeaf(tcat, lastToken);
		lastToken = NULL;
		ender = validEnder;
		firstOnLine = 0;
		return tcat;
	}	
	else
	{
		yytok(type);
		// If first-on-a-line AND this iteration's yytoken is a valid expression beginner and last iteration's yytoken was a valid expression ending
		// or if last iterations yytoken is a valid expression ender and this token is a right brace
		if( (firstOnLine && beginner && ender) )
		{
		// THEN save_token: return a semi-colon
			lastToken = yytoken;
			struct token* semiToken = malloc( sizeof( struct token) );
			semiToken->category = SM;
			semiToken->text = ";";
			semiToken->lineno = yylineno - 1;
			semiToken->filename = filename;

			// yylval.tok = semiToken;
			yylval.node = addLeaf( SM, semiToken);
			yyless(0);	// put back the token that yylex() grabbed this iteration
			ender = validEnder;
			firstOnLine = 0;
			return SM;
		}
		else
		{ // no semi-colon insertion, return the actual token grabbed this iteration of yylex
			// yylval.tok = yytoken;
			yylval.node = addLeaf( yytoken->category, yytoken);
			ender = validEnder;
			firstOnLine = 0;
			return yytoken->category;
		}
	}
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
	static int isClass = 0;
	
	// print if yydebug is turned on.
	if (yydebug)
		fprintf(stderr, "token: %s\n", yytext);

	if (i == CLASS) isClass = 1;

	if ( i == IDENT )
	{
		// Identifier found.
		struct Identifier* p = lookUp( yytext );
		if ( p == NULL )
		{
			fprintf(stderr, "New Identifer found!\n");
			// New identifier
			// yylval still holds the tree node for the previous token.
			p = addIdentifier( yytext, isClass ? CLASS_NAME : IDENT, yylineno );
		}
		// Construct token.
		t->lineno = yylineno;
		t->tval = p;
		t->text = p->name;
		t->filename = filename;
		// If typeCode of token before identifier was first declared
		// was of type "CLASS", then make this variable a class name.
		t->category = p->typeCode;

		// Return proper integer code.
		yytoken = t;
		// yylval.node = addLeaf(t->category, t);
		// fprintf(stderr, "%d int code!\n", t->category);
		isClass = 0;
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
	yytoken = t;
	// yylval.node = addLeaf(i, t);
	if ( i != CLASS) isClass = 0;
	return i;
}


// I feel like theres no way this really works, this also might hurt me because
// yyerror might be defined by lex if i dont provide a definition for it, 
// which this is doing.
void yyerror(char *s)
{
   fprintf(stderr, "%s:%d: %s before '%s' token\n",
	   yyfilename, yylineno, s, yytext);

	if (s[0] == 'l')
	{
		// "lexical error"
		exit(1);
	}
	else 
	{
		// other errors.
		exit(2);
	}
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
				firstOnLine = 1;
				break;
			case 0: {
			   exit(-(++errors));
			   }
	   }

	}
}

int yywrap() { return 1; }
