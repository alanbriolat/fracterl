-module(lib_misc).
-export([min/2, max/2]).

min(X, Y) -> lists:min([X, Y]).

max(X, Y) -> lists:max([X, Y]).
