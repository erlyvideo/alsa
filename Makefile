
all: compile

compile:
	rm -f src/._* c_src/._* ._*
	rebar compile

