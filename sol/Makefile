
top: sol.exe
	bash -c 'time ./sol.exe'

sol.exe: sol.o Makefile
	g++ $< -l stdc++ -o $@

sol.o: sol.C Makefile
	g++ -O2 -Wall -Werror $< -c -o $@
