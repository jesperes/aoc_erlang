ifeq (output-sync, $(filter output-sync, $(.FEATURES)))
MAKEFLAGS += --output-sync=none
endif

all:
	env MAKEFLAGS="" rebar3 as prod do compile,dialyzer,xref,eunit,ct

latest:
	env MAKEFLAGS="" rebar3 eunit -m aoc_eunit

clean:
	rebar3 clean

run2021:
	rebar3 escriptize && _build/default/bin/aoc aoc2021_.*
