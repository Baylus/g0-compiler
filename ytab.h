/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #1: Lexical Analyzer
"ytab.h"
	Defines integer codes for tokens.
*/

// break  class  d     double  else   false  for    if
// int    list   null  return  string table  true   void   while

#define IDENTIFIER 258
// KEYWORDS
#define NULLLITERAL 259		// null			!
#define BOOLFALSE 260		// false		!
#define BOOLTRUE 261		// true		!
#define BREAK 262			// break		!
#define CLASS 263			// class		!
#define DOUBLE 264			// double		
#define ELSE 265			// else	
#define FOR 266				// for		!
#define IF 267				// if	
#define INT 268				// int	
#define LIST 269			// list	
#define RETURN 270			// return
#define DROLL 271			// d		!
#define STRING 272			// string		!
#define TABLE 273			// table		!
#define VOID 274			// void	
#define WHILE 275			// while	
// OPERATORS 
/*
() 	Parenthesis
[] 	Subscript

-	Unary minus
!   Unary logical negation
d   Unary die roll
#	Unary size

*	Multiplication
/   Division
%   Modulus
d	dice roll

+	Addition
-	Subtraction

<	less than			
<=  less than or equal
>   greater than
>=	greater than or equal

==	is equal to
!=	is not equal to

&&	logical AND
||	logical OR

=	assignment
+=  increment
-=  decrement
:=:	swap
*/
#define LP 276			// (	
#define RP 277			// )	
#define LC 278			// {	
#define RC 279			// }	
#define LB 280			// [	
#define RB 281			// ]	
#define SM 282			// ;	
#define CM 283			// ,	
#define DOT 284			// .	#
#define BANG 285		// !	#
#define MOD 286			// %	#
#define SHARP 287		// #	#
#define PLUS 288		// +	
#define MINUS 289		// -	#
#define MUL 290			// *	
#define DIV 291			// /	
#define LT 292			// <	
#define GT 293			// >	
#define LE 294			// <=
#define GE 295			// >=
#define EQ 296			// ==	
#define NE 297			// !=	
#define ANDAND 298		// &&
#define OROR 299		// ||
#define ASN 300			// =	
#define PLASN 301		// +=
#define MIASN 302		// -=
#define SWAP 303		// :=:
#define BOOL 304		// bool
// #define  305		// 
// #define  306		// 
// #define  307		// 
// #define  308		// 
// #define  309		// 
#define INTLITERAL 310		// 	
#define FLOATLITERAL 311		// 	
#define BOOLLITERAL 312		// 			!				!!! Dont know if i need this or not.
#define CHARLITERAL 313		// '...'		!
#define STRINGLITERAL 314		// "..."	
// #define  315		// 
// #define  316		// 
// #define  317		// 
// #define  318		// 
// #define  319		// 
// #define  320		// 
// #define  321		// 
// #define  322		// 
// #define  323		// 
// #define  324		// 
// #define  325		// 
// #define  326		// 
// #define  327		// 
// #define  328		// 
// #define  329		// 
// #define  330		// 
// #define  331		// 
// #define  332		// 
// #define  333		// 
// #define  334		// 
// #define  335		// 
// #define  336		// 
// #define  337		// 
// #define  338		// 
// #define  339		// 
// #define  340		// 
// #define  341		// 
// #define  342		// 
// #define  343		// 
// #define  344		// 
// #define  345		// 
// #define  346		// 
// #define  347		// 
// #define  348		// 
// #define  349		// 
// #define  350		// 
// #define  351		// 
// #define  352		// 
// #define  353		// 
// #define  354		// 
// #define  355		// 
// #define  356		// 
// #define  357		// 
// #define  358		// 
// #define  359		// 
// #define  360		// 
// #define  361		// 
// #define  362		// 
// #define  363		// 
// #define  364		// 
// #define  365		// 
// #define  366		// 
// #define  367		// 
// #define  368		// 
// #define  369		// 
// #define  370		// 
// #define  371		// 
// #define  372		// 
// #define  373		// 
// #define  374		// 
// #define  375		// 
// #define  376		// 
// #define  377		// 
// #define  378		// 
// #define  379		// 
// #define  380		// 
// #define  381		// 
// #define  382		// 
// #define  383		// 
// #define  384		// 
// #define  385		// 
// #define  386		// 
// #define  387		// 
// #define  388		// 
// #define  389		// 
// #define  390		// 
// #define  391		// 
// #define  392		// 
// #define  393		// 
// #define  394		// 
// #define  395		// 
// #define  396		// 
// #define  397		// 
// #define  398		// 
// #define  399		// 
// #define  400		// 
// #define  401		// 
// #define  402		// 
// #define  403		// 
// #define  404		// 
// #define  405		// 
// #define  406		// 
// #define  407		// 
// #define  408		// 
// #define  409		// 
// #define  410		// 
// #define  411		// 
// #define  412		// 
// #define  413		// 
// #define  414		// 
// #define  415		// 
// #define  416		// 
// #define  417		// 
// #define  418		// 
// #define  419		// 
// #define  420		// 
// #define  421		// 
// #define  422		// 
// #define  423		// 
// #define  424		// 
// #define  425		// 
// #define  426		// 
// #define  427		// 
// #define  428		// 
// #define  429		// 
// #define  430		// 
// #define  431		// 
// #define  432		// 
// #define  433		// 
// #define  434		// 
// #define  435		// 
// #define  436		// 
// #define  437		// 
// #define  438		// 
// #define  439		// 
// #define  440		// 
// #define  441		// 
// #define  442		// 
// #define  443		// 
// #define  444		// 
// #define  445		// 
// #define  446		// 
// #define  447		// 
// #define  448		// 
// #define  449		// 
// #define  450		// 
// #define  451		// 
// #define  452		// 
// #define  453		// 
// #define  454		// 
// #define  455		// 
// #define  456		// 
// #define  457		// 
// #define  458		// 
// #define  459		// 
// #define  460		// 
// #define  461		// 
// #define  462		// 
// #define  463		// 
// #define  464		// 
// #define  465		// 
// #define  466		// 
// #define  467		// 
// #define  468		// 
// #define  469		// 
// #define  470		// 
// #define  471		// 
// #define  472		// 
// #define  473		// 
// #define  474		// 
// #define  475		// 
// #define  476		// 
// #define  477		// 
// #define  478		// 
// #define  479		// 
// #define  480		// 
// #define  481		// 
// #define  482		// 
// #define  483		// 
// #define  484		// 
// #define  485		// 
// #define  486		// 
// #define  487		// 
// #define  488		// 
// #define  489		// 
// #define  490		// 
// #define  491		// 
// #define  492		// 
// #define  493		// 
// #define  494		// 
// #define  495		// 
// #define  496		// 
// #define  497		// 
// #define  498		// 
// #define  499		// 
// #define  500		// 
// #define  501		// 
// #define  502		// 
// #define  503		// 
// #define  504		// 
// #define  505		// 
// #define  506		// 
// #define  507		// 
// #define  508		// 
// #define  509		// 
// #define  510		// 