#include <stdio.h>
#include "pla.tab.h"

/* Scanner-Methoden und -variablen */
int yylex(void);	/* Scanner-Routine 			*/
extern int yylineno;	/* akt. Zeilennummer 			*/
extern FILE * yyin;	/* Eingabedatei des Scanners		*/


/* Parser-Methoden und -variablen */
extern YYSTYPE yylval;	/* akt. Tokenwert			*/
int yyparse(void);	/* die Parsermethode			*/
