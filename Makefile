# Setup make for Erlang stuff
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean


# Modules to build
MODS = lib_misc \
	   complex \
	   fractal \
	   mandelbrot \


all: build

build: ${MODS:%=%.beam}

clean:
	rm -f *.beam
