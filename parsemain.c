/*
	Hauptprogramm des Parsers
	
	Aufruf:
		parsemain fileToParse 
*/

#include "global.h"
#include <stdio.h>
#include "pla.tab.h"
#include "symbol.h"

int main(int argc, char **argv)
{
	FILE * symbol;
	
	/* Abbruch, wenn kein Dateiname angegeben */
	if (argc < 2) {
		fprintf(stderr, "Aufruf: %s eingabedatei\n", argv[0]);
		return 1;
	}
	
	yyin = fopen(argv[1], "r");
	if (yyin == NULL) {
		perror("Konnte Eingabedatei nicht oeffnen");
		return 1;
	}

	symbol = fopen("symtable.out", "w");
	if (symbol == NULL) {
		perror("Konnte symtable.out nicht oeffnen");
		return 1;
	}
	
	if(yyparse()) {
		fprintf(stderr, "Parserfehler!\n");
	} else {
		fprintf(stderr, "Keine Fehler erkannt.\n");
	}
	
	printsymtab(actsym, symbol);
	fclose(symbol);
	
	return 0;
}
