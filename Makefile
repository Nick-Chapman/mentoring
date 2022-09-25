
top: run

run: cint.exe Makefile
	./cint.exe -test-lex

cint.exe: cint.o lexer.o
	g++ $^ -o $@

cint.o: cint.C lexer.h
	g++ -Wall -Werror $< -c -o $@

lexer.o: lexer.C lexer.h
	g++ -Wall -Werror $< -c -o $@
