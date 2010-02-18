%
% Simple map-reduce elastic cloud
%
-module(worker).
-export([worker/1, mapper/0, echo_server/0]).

echo_server() ->
    receive
        X ->
            io:format("~w~n", [X]),
            echo_server()
    end.

% Worker - just apply the supplied function with the supplied args and send
% the result
worker(Mapper) ->
    link(Mapper),
    worker_work(Mapper).

worker_work(Mapper) ->
    Mapper ! {self(), ready},
    receive
        {Reducer, Fun, Data} ->
            Reducer ! Fun(Data),
            worker_work(Mapper)
    end.


% Map a job over all workers that are attached to this mapper
mapper() ->
    % Become a system process (mapper_map/3 handles worker exits)
    process_flag(trap_exit, true),
    receive
        % Got a job to do - start mapping!
        {Reducer, Fun, Data} ->
            mapper_map(Reducer, Fun, Data),
            % Go back to waiting for jobs
            mapper()
    end.


% Return when there is no more data
mapper_map(_Reducer, _Fun, []) -> true;

% Serve up jobs to workers as and when they're ready
mapper_map(Reducer, Fun, [Next|Rest] = Data) ->
    receive
        % A linked worker has died
        {'EXIT', Worker, _Why} ->
            Job = erase(Worker),
            if
                % Worker was idle
                Job =:= undefined ->
                    mapper_map(Reducer, Fun, Data);
                % Add worker's job back to the queue
                true ->
                    {_, _, Item} = Job,
                    mapper_map(Reducer, Fun, [Item|Data])
            end;

        % A worker is ready to do something
        {Worker, ready} ->
            Job = {Reducer, Fun, Next},
            Worker ! Job,
            put(Worker, Job),
            mapper_map(Reducer, Fun, Rest)
    end.
