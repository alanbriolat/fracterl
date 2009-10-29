% 
% General operations useful for creating fractals
%
-module(fractal).
-export([bounded_iteration/4, float_seq/3]).


%
% Iterate a function until either the iteration limit is reached or the 
% threshold function returns true
%
% Fun(V) should return a value which can be passed to a future call to Fun.
%
% bounded_iteration will return the number of iterations performed.
%

% Public interface
bounded_iteration(Initial, Fun, IterLimit, ThresholdFun) ->
    bounded_iteration(Initial, Fun, 0, IterLimit, ThresholdFun).
% Base case - got to iteration limit
bounded_iteration(_Value, _Fun, Iter, IterLimit, _ThresholdFun) when Iter >= IterLimit -> IterLimit;
% Recursive case - check if threshold has passed, if not then recurse
bounded_iteration(Value, Fun, Iter, IterLimit, ThresholdFun) ->
    case ThresholdFun(Value) of
        true -> Iter;
        false -> bounded_iteration(Fun(Value), Fun, Iter+1, IterLimit, ThresholdFun)
    end.


%
% Create a sequence of N equally-spaced floats from First to Last
%
float_seq(X, Y, N) ->
    First = lib_misc:min(X, Y),
    Last = lib_misc:max(X, Y),
    Step = (Last - First) / (N - 1),
    float_seq_gen(First, Step, [], N).

float_seq_gen(_Start, _Step, Acc, 0) -> lists:reverse(Acc);
float_seq_gen(Start, Step, Acc, N) -> float_seq_gen(Start+Step, Step, [Start|Acc], N-1).
