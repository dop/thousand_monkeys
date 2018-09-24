all: thousand_monkeys.a
	dune build thousand_monkeys.a

.PHONEY: clean check

check:
	dune runtest

clean:
	dune clean
