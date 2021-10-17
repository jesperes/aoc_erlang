ifeq (output-sync, $(filter output-sync, $(.FEATURES)))
MAKEFLAGS += --output-sync=none
endif

all:
	rebar3 as prod do compile,dialyzer,xref,eunit,ct

clean:
	rebar3 clean
