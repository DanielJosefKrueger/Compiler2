# Makefile zur Uebersetzung des Parsers bzw. Scanners aus Aufgabe 2
# J. Uhrmann, 12.11.2005

# Variablendeklarationen
CFLAGS=-Wall -g
LDFLAGS=-lfl


# Regeln, die _keine_ Dateien mit dem Namen der Regel erstellen, sind "phony"
.PHONY: clean

parsemain: parsemain.o lex.yy.o pla.tab.o symbol.o error.o
	gcc -o $@ $^ $(LDFLAGS)

lexmain: lexmain.o lex.yy.o pla.tab.o symbol.o error.o
	gcc -o $@ $^ $(LDFLAGS)

# Erzeugung des Parser-Quelltexts und des Headerfiles fuer den Scanner
# mit Bison
pla.tab.c pla.tab.h: pla.y
	bison -d $<
	
# Erzeugung des Scanners aus der Flex-Eingabedatei
lex.yy.c: pla.l pla.tab.h
	flex pla.l
	
# Erzeugung eines Object-Files aus einer C-Datei
%.o: %.c
	gcc $(CFLAGS) -c -o $@ $<


# da C++-Dateien Header-Dateien inkludieren, sollten deren Abhaengigkeiten
# bekannt gemacht werden. Dies geschieht in den Dateien mit der Endung .d
# GCC kann diese automatisch mit dem Schalter -M aus Quelltexten erstellen.
%.d: %.c pla.tab.h
	gcc -M $< > $@
	
# hier werden sie inkludiert
# Falls sie nicht existieren, wird make das zwar melden, diese Dateien
# jedoch sofort mit der obigen Regel erzeugen und einbinden.
include  $(patsubst %.c,%.d,$(wildcard *.c))

clean:
	-rm *.o *.d
	-rm lex.yy.c pla.tab.h pla.tab.c
	
