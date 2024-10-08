
top: loop.S run

run: loop.exe
	bash -c './loop.exe'

loop.exe: loop.o Makefile
	g++ $< -o $@

loop.o: loop.C Makefile
	g++ -c $< -o $@

loop.S: loop.C Makefile
	g++ -S $< -o $@
