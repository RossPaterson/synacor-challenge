PROGS := game coins teleporter orb
GENERATED := output $(PROGS) maze.svg

all: $(GENERATED)

clean:
	$(RM) $(GENERATED) *.o *.hi

output: game script challenge.bin
	./game >$@

challenge.bin:
	@echo 'Get the challenge materials from https://challenge.synacor.com/'
	@exit 1

%: %.hs
	/usr/bin/ghc -Wall --make -O $*.hs
	@touch $@

%: %.lhs
	/usr/bin/ghc -Wall --make -O $*.lhs
	@touch $@

%.svg: %.gv
	dot -Tsvg $*.gv >$@

game: State.hs Machine.hs
