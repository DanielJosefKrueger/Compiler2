/*
	Hauptprogramm zum Testen des erzeugten Scanners.
	
	Aufruf:
		lexmain fileToScan.txt
*/

#include "global.h"
#include <stdio.h>
#include "pla.tab.h"

int main(int argc, char **argv)
{
	int token;
	FILE * outfile;
	
	/* Abbruch, wenn kein Dateiname angegeben */
	if (argc < 3) {
		fprintf(stderr, "Aufruf: %s eingabedatei ausgabedatei\n", argv[0]);
		return 1;
	}
	
	yyin = fopen(argv[1], "r");
	if (yyin == NULL) {
		perror("Konnte Eingabedatei nicht oeffnen");
		return 1;
	}
	
	outfile = fopen(argv[2], "w");
	if (outfile == NULL) {
		perror("Konnte Ausgabedatei nicht oeffnen");
		return 1;
	}
	
	token = yylex();
	while(token) {
		fprintf(outfile, "Zeile %d:\t Token:%d", yylineno, token);
		switch (token) {
			case IDENT:
				fprintf(outfile, "\t idname=%s\n", yylval.idname);
				break;
			case INTNUMBER:
				fprintf(outfile, "\t num=%d\n", yylval.num);
				break;
			case REALNUMBER: 
				fprintf(outfile, "\t realnum=%f\n", yylval.realnum);
				break; 
			default:
				fprintf(outfile, "\n");
		}
		
		token = yylex();
	}
	
	fprintf(outfile, "Dateiende in Zeile %d erkannt.\n", yylineno);
	fclose(outfile);
	return 0;
}
