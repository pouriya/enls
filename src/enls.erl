-module(enls).

-export([
    load/0,
    load/1
]).


load() ->
    load(#{}).


load(Opts) ->
    case enls_input:load(Opts) of
        {ok, Data} ->
            enls_compiler:load(Data, Opts);
        Err ->
            Err
    end.
