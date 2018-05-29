/*
	Prototypen fuer Symboltabellenfunktionen
	
	J. Uhrmann, 12.11.2005/  Messerer 24.04.2015
*/

#include <stdio.h>

/* Definition der Art der ST- Eintr�ge  */ 
#define KONST 		310			/* Konstante */ 
#define INTIDENT  	320			/* Identifikator vom Typ int  */ 
#define REALIDENT  	330			/* Identifikator vom Typ real */ 
#define BOOLIDENT  	340			/* Identifikator vom Typ boolean */ 
#define PROC		400			/* Procedure */



#define SYMMAX		20			/* max. Anzahl Eintraege in einer Teilsymboltabelle */

struct symtable;
/* Aufbau eines Symboltabelleneintrags */
struct st_entry 
{  	
	int type;				/* Art des Eintrags (KONST/INTIDENT/REALIDENT/BOOLIDENT/PROC)  */
	char *name;				/* Zeiger auf Namen */
	int wertaddr;				/* Wert bei Konstanten (KONST); 
						   relative Speicheradresse (offset) bei Variablen */
	struct symtable * subsym;		/* Zeiger auf Teil-Symboltabelle bei Art PROC */ 
};


/* Aufbau der (Teil-)Symboltabelle */
struct symtable
{ 
	struct symtable * precsym;		/* Zeiger auf uebergeordnete Symboltabelle; 
										bei oberster NULL */
	int level;				/* Schachtelungstiefe  */
	int anzahl;				/* Anzahl der Symboltabelleneintr�ge */
	struct st_entry eintrag[SYMMAX];	/* Feld fuer Eintraege */ 
};

extern struct symtable * actsym;		/* Zeiger auf die aktuelle Symboltabelle */


/*
	Erzeugt eine neue Symboltabelle und gibt einen Zeiger darauf zurueck.
	Falls die aktuelle Symboltabelle gesetzt ist, dann wird diese als
	uebergeordnete in der neu erzeugten vermerkt.
*/
struct symtable * create_newsym();

/*
	Sucht nach einem Namen in der aktuellen und allen uebergeordneten
	Symboltabellen und gibt den gefundenen Eintrag zurueck.
	
	Wird kein entspr. Eintrag gefunden, wird NULL zurueckgegeben.
*/


struct st_entry * lookup(char *s);

/*
	Sucht nach einem Namen in der aktuellen Symboltabelle (actsym)
	und gibt den gefundenen Eintrag zurueck.
	
	Wird kein entspr. Eintrag gefunden, wird NULL zurueckgegeben.
*/
struct st_entry * lookup_in_actsym(char *s);

/*
	Erzeugt einen neuen Eintrag in der aktuellen Symboltabelle (actsym).
	Alle notwendigen Daten werden aus den globalen Variablen des Parsers
	ermittelt.
	
	Uebergeben wird der Typ des Eintrags, der Name und der Wert.
	Ist laut uebergebenem Typ kein Wert vorgesehen, wird der Inhalt
	des Parameters "wert" ignoriert.
*/
struct st_entry * insert(int type, char * name, int wert);

/*
	Schreibt die Symboltabelle in das angegebene File.
*/
void printsymtab(struct symtable * symtab, FILE * dest);
