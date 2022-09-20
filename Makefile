
top: run

run: cint.exe
	./cint.exe

cint.exe: cint.o lexer.o
	gcc $^ -l stdc++ -o $@

cint.o: cint.C lexer.h
	gcc -Wall -Werror $< -c -o $@

lexer.o: lexer.C lexer.h
	gcc -Wall -Werror $< -c -o $@
