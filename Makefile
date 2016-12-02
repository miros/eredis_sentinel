.PHONY: all compile run test clean

REBAR=./rebar3

all: $(REBAR) compile

compile:
	$(REBAR) install_deps
	$(REBAR) compile

run:
	erl -pa _build/default/lib/*/ebin

test:
	bash test/run.sh

clean:
	$(REBAR) clean
	rm -rf ./test/*.beam
	rm -rf ./test/TEST*.xml
	rm -rf ./erl_crash.dump
