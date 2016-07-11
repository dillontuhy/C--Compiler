CC = gcc
CFLAGS = -g -O2 -DYYDEBUG

CFILES = semantic.lex.c semantic.tab.c

HFILES = semantic.tab.h semantic.h

OFILES = semantic.lex.o semantic.tab.o

compile: $(OFILES)
	$(CC) $(CFLAGS) $(OFILES) -lfl -o compile

semantic.lex.c : semantic.l semantic.tab.h
	flex -o semantic.lex.c semantic.l

semantic.tab.h : semantic.y
	bison -v -d semantic.y

semantic.tab.c : semantic.y
	bison -v -d semantic.y

clean:
	/bin/rm -f compile semantic.lex.c semantic.tab.c semantic.tab.h *.BAK *.o