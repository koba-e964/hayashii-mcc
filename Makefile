BUILD = build/
DEST = ${PWD}
CMP   = $(DEST)/bin/hmcc
ASM   = $(BUILD)assembler
EXEC  = $(BUILD)executer
MCCFLAGS = --glib lib/fl.ml
SAMPLES = sample/ack.ml sample/add.ml \
sample/cond.ml sample/constexpr.ml \
sample/fib.ml sample/float-easy.ml sample/float.ml sample/gcd.ml \
sample/inprod-loop.ml sample/inprod-rec.ml sample/inprod.ml \
sample/join-reg.ml sample/join-reg2.ml sample/join-stack.ml sample/join-stack2.ml sample/join-stack3.ml \
sample/matmul-flat.ml \
sample/non-tail-if.ml sample/non-tail-if2.ml sample/print.ml \
sample/shuffle.ml sample/spill.ml sample/spill2.ml sample/spill3.ml sample/sum-tail.ml sample/sum.ml
#sample/adder.ml
#sample/even-odd.ml
#sample/funcomp.ml
#sample/cls-bug.ml sample/cls-bug2.ml sample/cls-rec.ml \
#sample/matmul.ml 
.PHONY: test_all clean compiler executer assembler
test_all : $(SAMPLES:%.ml=%.test)
compiler: $(CMP)
executer : $(EXEC)
assembler : $(ASM)


%.s : %.ml $(CMP)
	LANG=C.UTF-8 $(CMP) $(MCCFLAGS) -o $*.s <$*.ml >$*.out
clean : 
	rm -f sample/*.out sample/*.s
	rm -f $(ASM) $(EXEC)
	-cd Zekamashi/asm; omake clean
	-make -C Zekamashi/sim clean
%.test: %.x %.ml $(EXEC) converter
	$(EXEC) $*.x >$*.out 2>$*.err; if test $$? -ne 0 ; then cat $*.err; false; fi
	ocaml $*.ml | tr -d '\n' >$*.out-oc
	./converter <$*.out >$*.out-mc
	diff --ignore-blank-lines --ignore-all-space $*.out-mc $*.out-oc
	rm $*.err
%.testlib: %-main.x %-main.ml %-lib.ml $(EXEC) converter
	$(EXEC) $*-main.x >$*-main.out 2>$*-main.err; if test $$? -ne 0 ; then cat $*-main.err; false; fi
	rm $*-main.err
%.x: %.s $(ASM)
	$(ASM) $*.s -o $*.x >/dev/null
%-main.s: %-lib.ml %-main.ml $(CMP) $(LIB)
	$(CMP) $(MCCFLAGS) $(STDLIB) -glib $*-lib $*-main
$(CMP): 
	cabal install --prefix=$(DEST)
$(ASM): Zekamashi
	cd Zekamashi/asm; omake
	cp Zekamashi/asm/asagumo $(ASM)
Zekamashi/sim/amatsukaze:
	$(MAKE) -C Zekamashi/sim
$(EXEC): Zekamashi/sim/amatsukaze
	cp Zekamashi/sim/amatsukaze $(EXEC)
converter: converter.c
	gcc $< -o $@


