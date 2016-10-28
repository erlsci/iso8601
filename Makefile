build:
	rebar3 compile

clean:
	rm -rf .rebar .rebar3 deps _build rebar.lock ebin/*

check:
	rebar3 eunit -v

push:
	git push github master
	git push gitlab master

push-tags:
	git push github --tags
	git push gitlab --tags

push-all: push push-tags

publish: clean
	rebar3 hex publish

