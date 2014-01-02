NAME=iso8601
VERSION=1.1.1

.PHONY: test clean distclean

all: compile

deps:
	@(./rebar get-deps)
	
compile:
	@(./rebar compile)

test:
	@(./rebar eunit)

doc:
	@(./rebar doc)

clean:
	@(./rebar clean)

distclean: clean
	@(./rebar delete-deps)
