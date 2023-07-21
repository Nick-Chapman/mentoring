
top: cps.exe
	bash -c './cps.exe'

cps.exe: cps.o Makefile
	g++ $< -l stdc++ -o $@

cps.o: cps.C Makefile
	g++ -O2 -Wall -Werror $< -c -o $@
