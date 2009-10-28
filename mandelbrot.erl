-module(mandelbrot).

-export([
        iterate/3,
        get_fun/2
    ]).

iterate(C, IterLimit, Threshold) ->
    iterate(complex:create(0, 0), C, 0, IterLimit, Threshold).

iterate(_, _, I, IterLimit, _) when I > IterLimit -> IterLimit;
iterate(Z, C, I, IterLimit, Threshold) ->
%    AboveThreshold = complex:abs(Z) > Threshold,
    case (complex:abs(Z) > Threshold) of
        true -> I;
        false -> iterate(complex:add(complex:mult(Z, Z), C), C, I+1, IterLimit, Threshold)
    end.

% 
% Create a fun which wraps around iterate for a given IterLimit and Threshold
%
get_fun(IterLimit, Threshold) ->
    fun(X) -> iterate(X, IterLimit, Threshold) end.
