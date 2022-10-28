
top: run

run: _build cint.exe Makefile
	./cint.exe -test-parser

units = $(patsubst src/%.C, %, $(wildcard src/*.C))
objs = $(patsubst %, _build/%.o, $(units))
deps = $(patsubst %, _build/%.d, $(units))

cint.exe: $(objs)
	@echo Linking
	@g++ $^ -o $@

_build/%.o: src/%.C Makefile
	@echo Building $<
	@g++ -Wall -Werror $< -c -o $@ -MMD

_build: ; @mkdir -p $@

-include $(deps)
