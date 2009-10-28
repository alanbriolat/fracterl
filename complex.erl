%% Simple complex numbers library

-module(complex).

-record(complex, {real, imag}).
-export([
        create/1, create/2,
        is_complex/1,
        get_real/1, get_imag/1,
        add/2, sub/2, mult/2, divide/2,
        abs/1,
        format/1
    ]).

create(X) when is_record(X, complex) -> X;
create({R, I}) -> create(R, I);
create(Real) -> create(Real, 0).
create(Real, Imag) -> #complex{real = Real, imag = Imag}.

is_complex(X) when is_record(X, complex) -> true;
is_complex(_) -> false.

get_real(X) -> X#complex.real.
get_imag(X) -> X#complex.imag.

add(X, Y) ->
    A = create(X), B = create(Y),
    create(A#complex.real + B#complex.real,
           A#complex.imag + B#complex.imag).

sub(X, Y) ->
    A = create(X), B = create(Y),
    create(A#complex.real - B#complex.real,
           A#complex.imag - B#complex.imag).

mult(X, Y) ->
    A = create(X), B = create(Y),
    create((A#complex.real*B#complex.real)-(A#complex.imag*B#complex.imag),
           (A#complex.imag*B#complex.real)+(A#complex.real*B#complex.imag)).

divide(X, Y) ->
    A = create(X), B = create(Y),
    Divisor = math:pow(B#complex.real, 2) + math:pow(B#complex.imag, 2),
    create(((A#complex.real*B#complex.real)+(A#complex.imag*B#complex.imag))/Divisor,
           ((A#complex.imag*B#complex.real)-(A#complex.real*B#complex.imag))/Divisor).

abs(X) ->
    A = create(X),
    math:sqrt(math:pow(A#complex.real, 2) + math:pow(A#complex.imag, 2)).

format(#complex{real=R, imag=I}) ->
    io_lib:format("(~f+~fi)", [R, I]).
