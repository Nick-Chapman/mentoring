
top: run

run: cint.exe Makefile
	./cint.exe -test-lex

cint.exe: cint.o ast.o lexer.o
	g++ $^ -o $@

cint.o: cint.C ast.h lexer.h
	g++ -Wall -Werror $< -c -o $@

ast.o: ast.C ast.h
	g++ -Wall -Werror $< -c -o $@

lexer.o: lexer.C lexer.h
	g++ -Wall -Werror $< -c -o $@
