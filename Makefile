
top: run

gen=_build

run: _build cint.exe Makefile
	./cint.exe -test-parser

cint.exe: _build/cint.o _build/ast.o _build/lexer.o _build/parser.o
	g++ $^ -o $@

_build/cint.o: cint.C ast.h lexer.h parser.h
	g++ -Wall -Werror $< -c -o $@

_build/ast.o: ast.C ast.h
	g++ -Wall -Werror $< -c -o $@

_build/lexer.o: lexer.C lexer.h
	g++ -Wall -Werror $< -c -o $@

_build/parser.o: parser.C parser.h lexer.h ast.h
	g++ -Wall -Werror $< -c -o $@

_build: ; @mkdir -p $@
