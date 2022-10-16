
top: run

run: main.exe Makefile
	./main.exe

main.exe: main.o lex.yy.o
	g++ $^ -o $@

main.o: main.c lexer.h Makefile
	gcc -Wall -Werror $< -c -o $@

lex.yy.c: lexer.lex lexer.h Makefile
	lex $<

lex.yy.o: lex.yy.c Makefile
	gcc -Wall -Werror $< -c -o $@
