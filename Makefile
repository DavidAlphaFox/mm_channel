.PHONY: deps
.PHONY: rel

all:clean rel

deps:
	./rebar get-deps

clean:
	./rebar clean

compile: deps 
	./rebar compile

rel: compile
	./rebar generate
