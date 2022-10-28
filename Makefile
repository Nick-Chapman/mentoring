
top: run

gen=_build

run: _build cint.exe Makefile
	./cint.exe -test-parser

cint.exe: _build/cint.o _build/ast.o _build/lexer.o _build/parser.o
	g++ $^ -o $@

_build/cint.o: src/cint.C src/ast.h src/lexer.h src/parser.h
	g++ -Wall -Werror $< -c -o $@

_build/ast.o: src/ast.C src/ast.h
	g++ -Wall -Werror $< -c -o $@

_build/lexer.o: src/lexer.C src/lexer.h
	g++ -Wall -Werror $< -c -o $@

_build/parser.o: src/parser.C src/parser.h src/lexer.h src/ast.h
	g++ -Wall -Werror $< -c -o $@

_build: ; @mkdir -p $@
