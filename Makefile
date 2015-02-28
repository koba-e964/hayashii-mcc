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
test_all : $(SAMPLES:%.ml=%.s)

%.s : %.ml
	LANG=C.UTF-8 cabal run -- -o $*.s <$*.ml >$*.out
clean : 
	rm -f sample/*.out sample/*.s

