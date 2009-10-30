-module(lib_misc).
-export([min/2, max/2, spawn_n/2]).

min(X, Y) -> lists:min([X, Y]).

max(X, Y) -> lists:max([X, Y]).

spawn_n(0, _Args) -> [];
spawn_n(N, Args) -> [apply(spawn, Args) | spawn_n(N - 1, Args)].
