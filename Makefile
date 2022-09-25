
top: run

run: cint.exe Makefile
	./cint.exe -test-parser

cint.exe: cint.o ast.o lexer.o parser.o
	g++ $^ -o $@

cint.o: cint.C ast.h lexer.h parser.h
	g++ -Wall -Werror $< -c -o $@

ast.o: ast.C ast.h
	g++ -Wall -Werror $< -c -o $@

lexer.o: lexer.C lexer.h
	g++ -Wall -Werror $< -c -o $@

parser.o: parser.C parser.h lexer.h ast.h
	g++ -Wall -Werror $< -c -o $@
