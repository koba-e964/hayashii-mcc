SAMPLES = sample/ack.ml sample/add.ml sample/adder.ml sample/cls-bug.ml sample/cls-bug2.ml sample/cls-rec.ml sample/cond.ml sample/constexpr.ml sample/even-odd.ml sample/fib.ml sample/float-easy.ml sample/float.ml sample/funcomp.ml sample/gcd.ml sample/inprod-loop.ml sample/inprod-rec.ml sample/inprod.ml sample/join-reg.ml sample/join-reg2.ml sample/join-stack.ml sample/join-stack2.ml sample/join-stack3.ml sample/matmul-flat.ml sample/matmul.ml sample/non-tail-if.ml sample/non-tail-if2.ml sample/print.ml sample/shuffle.ml sample/spill.ml sample/spill2.ml sample/spill3.ml sample/sum-tail.ml sample/sum.ml

test_all : $(SAMPLES:%.ml=%.test)

%.test : %.ml
	cabal run <$*.ml >/dev/null

