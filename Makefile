.PHONY: all clean repl test

all:
	jbuilder build --dev

repl:
	jbuilder utop src -- -require ocaml-monadic

test:
	jbuilder runtest -f --dev

clean:
	jbuilder clean

