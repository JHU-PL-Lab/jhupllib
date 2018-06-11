.PHONY: all clean repl test

all:
	jbuilder build --dev

repl:
	jbuilder utop src -- -require jhupllib

test:
	jbuilder runtest -f --dev

clean:
	jbuilder clean

