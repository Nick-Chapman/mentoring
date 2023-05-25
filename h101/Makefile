
exe = .stack-work/dist/x86_64-linux/Cabal-3.6.3.0/build/main.exe/main.exe

top: solitaire.solution
	git diff solitaire.solution

solitaire.solution: src/*.hs Makefile
	stack build
	bash -c 'time $(exe) > $@'
