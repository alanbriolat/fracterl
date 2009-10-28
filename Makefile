# Setup make for Erlang stuff
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean


# Modules to build
MODS = complex \
	   fractal \
	   mandelbrot \


all: build

build: ${MODS:%=%.beam}


