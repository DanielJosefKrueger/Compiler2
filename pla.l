/*
	Scanner fuer PLA
	J. Uhrmann, 12.11.2005
	M.Messerer , 2017
*/

/*	---- Definitionen ---- 		*/

%{
	/* Definitionen in C */
	
	#include "pla.tab.h"	/* Token-Konstanten des Parsers verwenden */
	#include <math.h>	/* fuer atoi */
	#include <stdio.h>
	#include <string.h>	/* fuer strcpy */
%}


/* Zeilennummern sollen ermittelt und in yylineno gespeichert werden */
%option yylineno

/* Definitionen fuer flex */
DIGIT	[0-9]


/*	---- Scanner-Regeln ----	*/
%%

"const"		return CONST;

...........
...........
...........










[ \t\n\r]		/* Whitespaces ignorieren */


.		{	/* alle anderen Zeichen sind Fehler */
			fprintf(stderr, "Zeile %d: ungueltiges Zeichen erkannt: %s\n", yylineno, yytext);
			exit(1);
		}
%%
/*	Ende  ---- */
