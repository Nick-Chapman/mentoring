
top: start.o run

start.o: start.C
	gcc -Wall -Werror $< -c -o $@

run: cint.exe
	./cint.exe

cint.exe: cint.o
	gcc $^ -l stdc++ -o $@

cint.o: cint.C
	gcc -Wall -Werror $< -c -o $@
