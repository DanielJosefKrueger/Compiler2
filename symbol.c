#include <stdlib.h>
#include <string.h>
#include "symbol.h"
#include "error.h"



/* Zeiger auf die aktuelle Symboltabelle */
struct symtable * actsym = NULL;

/*
	Erzeugt eine neue Symboltabelle und gibt einen Zeiger darauf zurueck.
*/
struct symtable * create_newsym()
{
	struct symtable * newsym = malloc(sizeof(struct symtable));
	
	/* falls die globale Symboltabelle gesetzt ist, diese entspr.
	   eintragen
	*/
	if (actsym != NULL) {
		newsym->level = actsym->level + 1;
		newsym->precsym = actsym;
	} else {
		newsym->level = 0;
		newsym->precsym = NULL;
	}
	
	newsym->anzahl = 0;
	
	return newsym;
}

/*
	Sucht nach einem Namen in der aktuellen und allen uebergeordneten
	Symboltabellen und gibt den gefundenen Eintrag zurueck.
	
	Wird kein entspr. Eintrag gefunden, wird NULL zurueckgegeben.
*/
struct st_entry * lookup(char *s)
{
	int i;
	struct symtable * current = actsym;
	
	while (current != NULL) {
		for(i=0; i < current->anzahl; ++i) {
			if (strcmp(s, current->eintrag[i].name) == 0)
				return &current->eintrag[i];
		}
		current = current->precsym;
	}
	
	return NULL;
}

/*
	Sucht nach einem Namen in der aktuellen Symboltabelle (actsym)
	und gibt den gefundenen Eintrag zurueck.
	
	Wird kein entspr. Eintrag gefunden, wird NULL zurueckgegeben.
*/
struct st_entry * lookup_in_actsym(char *s)
{
	int i;
	
	for (i=0; i < actsym->anzahl; ++i) {
		if (strcmp(s, actsym->eintrag[i].name) == 0)
			return &actsym->eintrag[i];
	}
	
	return NULL;
}

/*
	Erzeugt einen neuen Eintrag in der aktuellen Symboltabelle (actsym).
	Alle notwendigen Daten werden aus den globalen Variablen des Parsers
	ermittelt.
	
	Uebergeben wird der Typ des Eintrags.
*/
struct st_entry * insert(int type, char * name, int wert)
{
	struct st_entry * newentry = malloc(sizeof(struct st_entry));
	newentry->type = type;
	newentry->name = malloc(strlen(name) + 1);
	strcpy(newentry->name, name);
	newentry->subsym = NULL;
	newentry->wertaddr = 0;
	
	switch(type) {
		case KONST:
			newentry->wertaddr = wert;
			break;
		case INTIDENT: 
		case REALIDENT:
		case BOOLIDENT:
			break;
		case PROC:
			newentry->subsym = create_newsym();
			break;
		default:
			error(36);
	}
	
	actsym->eintrag[actsym->anzahl++] = *newentry;
	return newentry;
}

/*
	interne Routine zur Einrueckung
*/
void sym_indent(int level, FILE * dest)
{
	while (level-- > 0) {
		fprintf(dest, "    ");
	}
}

/*
	Schreibt die Symboltabelle in das angegebene File.
*/
void printsymtab(struct symtable * symtab, FILE * dest)
{
	int i;
	struct st_entry * act;
	
	sym_indent(symtab->level, dest); fprintf(dest, "Anzahl der Eintraege: %d\n", symtab->anzahl);
	sym_indent(symtab->level, dest); fprintf(dest, "Blockniveau: %d\n", symtab->level);
	
	for (i=0; i < symtab->anzahl; ++i) {
		act = &symtab->eintrag[i];
		sym_indent(symtab->level, dest); fprintf(dest, "Eintrag Nr: %d", i+1);
		sym_indent(symtab->level, dest); fprintf(dest, "\t%s\t%d\t%d\n", act->name, act->type, act->wertaddr);
		
		if (act->type == PROC) {
			sym_indent(symtab->level, dest); fprintf(dest, "Symboltabelle zu %s\n", act->name);
			printsymtab(act->subsym, dest);
			sym_indent(symtab->level, dest); fprintf(dest, "Ende Symboltabelle zu %s\n", act->name);
		}
	}
}


