/*
Baylus Tunnicliff
9/5/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #1: Lexical Analyzer
"tokenlist.c"
*/

#include <stdio.h>
#include <stdlib.h>

#include "token.h"
#include "tokenlist.h"
#include "ytab.h"

struct tokenlist *head = NULL, *tail = NULL;

int addToken(struct token* t){
	/*
	 *
	 *
	 *
	 *
	 *
	 */
	if ( t == NULL ) {
		perror("Trying to add an empty token!");
		exit(-1);
	}
	
	if ( head == NULL ) {
		head = calloc( 1, sizeof(struct tokenlist) );
		head->tok = t;
		head->next = NULL;
		tail = head;
	}
	else {
		tail->next = calloc( 1, sizeof(struct tokenlist) );
		tail = tail->next;
		tail->tok = t;
		tail->next = NULL;
	}
}


void printToken(struct token* t){
	/*
	 *
	 *
	 *
	 *
	 *
	 */
	printf("%d\t\t", t->category);
	printf("%s\t\t", t->text);
	printf("%d\t\t", t->lineno);
	printf("%s\t\t", t->filename);
	switch(t->category){
		case INTLITERAL:
			printf("%d", t->ival );
			break;
		case FLOATLITERAL:
			printf("%f", t->dval );
			break;
		case STRINGLITERAL:
			printf("%s", t->sval );
			break;
		// default:
			// perror("ERROR: tokenlist.c, func: printToken(): unknown category\n\n");
			// exit(-1);
	}
	printf("\n");
}

void printList(){
	/*
	 *
	 *
	 *
	 *
	 *
	 */
	struct tokenlist* p = head;
	if ( p == NULL ) {
		printf("List is empty. File did not contain tokens.\n");
		return;
	}
	printf("Category\t\tText\t\tLineno\t\tFilename\t\tIval/Sval\n");
	while (p != NULL) {
		printToken(p->tok);
		p = p->next;
	}
}

void deleteList(){
	/*
	 *
	 *
	 *
	 *
	 *
	 */
	while(head != NULL){
		struct tokenlist* p = head;
		head = head->next;
		// Free memory stored in token
		if (p->tok->text == NULL) {
			perror("deleteList(): String was found NULL...\n\n");
			exit(-1);
		}
		free(p->tok->text);
		p->tok->text = NULL;
		if (p->tok->category == STRINGLITERAL) {
			free(p->tok->sval);
			p->tok->sval = NULL;
		}
		// Filename doesnt have to be free'd because they are 
		// Shared pointers to the same strings that are malloc'd by argv.
		
	}
	// List destroyed. 
	tail = NULL;
}