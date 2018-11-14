#define BOOL 257
#define BREAK 258
#define CLASS 259
#define STRING 260
#define DOUBLE 261
#define ELSE 262
#define FOR 263
#define IF 264
#define INT 265
#define RETURN 266
#define TABLE 267
#define LIST 268
#define THIS 269
#define TRUE 270
#define FALSE 271
#define VOID 272
#define WHILE 273
#define IDENT 274
#define CONTINUE 275
#define NULLLITERAL 276
#define INTLITERAL 277
#define CHARLITERAL 278
#define FLOATLITERAL 279
#define STRINGLITERAL 280
#define LP 281
#define RP 282
#define LC 283
#define RC 284
#define LB 285
#define RB 286
#define SM 287
#define CM 288
#define DOT 289
#define ASN 290
#define LT 291
#define GT 292
#define BANG 293
#define SHARP 294
#define EQ 295
#define NE 296
#define LE 297
#define GE 298
#define ANDAND 299
#define OROR 300
#define PLUS 301
#define MINUS 302
#define MUL 303
#define DIV 304
#define AND 305
#define OR 306
#define MOD 307
#define PLASN 308
#define MIASN 309
#define SWAP 310
#define COLON 311
#define DROLL 312
#define BAD_TOKEN 313
#define CLASS_NAME 314
#define END 0
#define UDROLL 316
#define UNEGATE 317
#define UMINUS 318
#define PAREN 319
#define SUB 320
#ifdef YYSTYPE
#undef  YYSTYPE_IS_DECLARED
#define YYSTYPE_IS_DECLARED 1
#endif
#ifndef YYSTYPE_IS_DECLARED
#define YYSTYPE_IS_DECLARED 1
typedef union {
   tree* node;
   } YYSTYPE;
#endif /* !YYSTYPE_IS_DECLARED */
extern YYSTYPE yylval;
