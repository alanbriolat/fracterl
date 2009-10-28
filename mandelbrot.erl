%
% Mandelbrot fractal generation
%
-module(mandelbrot).
-export([iterate/2, create_fun/1]).

% Iterate a point for the Mandelbrot set
iterate(Initial, IterLimit) ->
    Fun = fun({Z, C}) -> {complex:add(complex:mult(Z, Z), C), C} end,
    ThresholdFun = fun({Z, _C}) -> complex:abs(Z) > 2.0 end,
    fractal:bounded_iteration({Initial, Initial}, Fun, IterLimit, ThresholdFun).

% Create a function for iterating values for the Mandelbrot set (so the
% IterLimit doesn't need to passed around).
create_fun(IterLimit) ->
    fun(X) -> iterate(X, IterLimit) end.
