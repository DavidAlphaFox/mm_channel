.PHONY: deps
.PHONY: rel

all:deps compile

deps:
	./rebar get-deps

clean:
	./rebar clean

compile: deps clean
	./rebar compile

rel:compile
	./rebar generate

start-dev:
	./rel/dog/bin/dog console

start:
	./rel/dog/bin/dog start