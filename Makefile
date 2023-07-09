all: compile

deps:
	make -C c_src deps
	rebar3 get-deps

compile:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 eunit

nif:
	make -C c_src

benchmark:
	@rebar3 as test do get-deps, eunit

mix-benchmark:
	@MIX_ENV=test mix do deps.get, deps.compile
	@MIX_ENV=test rebar3 as test do get-deps, eunit
	@echo
	@MIX_ENV=test mix benchmark

.PHONY: test
