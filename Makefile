NAME=iso8601
VERSION=1.1.1

all: compile

deps:
	@(./rebar get-deps)
	
compile:
	@(./rebar compile)
	
clean:
	@(./rebar clean)

distclean: clean
	@(./rebar delete-deps)

