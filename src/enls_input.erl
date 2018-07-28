-module(enls_input).

-export([load/2, read_erlang_file/2, check_callback/1, check_data/1]).

load(InitArg, Opts) ->
    {Mod, Func} = enls:value(callback
                            ,Opts
                            ,{?MODULE, read_erlang_file}
                            ,{?MODULE, check_callback}),
    case Mod:Func(InitArg, Opts) of
        {ok, Data} ->
            check_data(Data);
        {error, _}=Err ->
            Err;
        Other ->
            {error, {return, [{returned_value, Other}
                             ,{callback, {Mod, Func}}]}}
    end.


read_erlang_file(File, Opts) ->
    Encoding = enls:value(encoding, Opts, latin1, {?MODULE, check_encoding}),
    case file:open(File, [read, {encoding, Encoding}]) of
        {ok, FD} ->
            case read_erlang_file_loop(FD, 1, []) of
                {ok, _}=Ok ->
                    Ok;
                {_, {Rsn, ErrParams}} -> % {error, _}
                    {error, {Rsn, ErrParams ++ [{file, File}]}}
            end;
        {_, Rsn} -> % {error, _}
            {error, {open_file, [{reason, Rsn}, {file, File}]}}
    end.


read_erlang_file_loop(FD, Location, Data) ->
    case io:read(FD, '', Location) of
        {ok, Term, EndLocation} ->
            read_erlang_file_loop(FD, EndLocation, [Term|Data]);
        {eof, _} ->
            {ok, lists:reverse(Data)};
        {_, {Line, _, Rsn}, ErrLocation} -> % {error, ErrInfo, Location}
            {error, {read_file, [{reason, Rsn}, {line, Line}]}}
    end.
            

check_data(Data) when erlang:is_list(Data) ->
    check_data(Data, []);
% error clause:
check_data(Other) ->
    {error, {data, [{data, Other}]}}.


check_data([{IntId, Text}|Data], Data2) when erlang:is_integer(IntId) andalso
                                             erlang:is_list(Text)          ->
    case lists:keyfind(IntId, 1, Data2) of
        false ->
            check_data(Data, [{IntId, Text}|Data2]);
        {_, Text2} ->
            {error, {duplicate, [{id, IntId}, {previous, Text2}, {current, Text}]}}
    end;
check_data([], Data2) ->
    {ok, lists:reverse(Data2)};
% error clauses:
check_data([{Other, Text}|_], _) when erlang:is_list(Text) ->
    {error, {id, [{id, Other}, {text, Text}]}};
check_data([{IntId, Other}|_], _) when erlang:is_integer(IntId) ->
    {error, {text, [{text, Other}, {id, IntId}]}};
check_data([{Other1, Other2}|_], _) ->
    {error, {element, [{id, Other1}, {text, Other2}]}};
check_data([Other|_], _) ->
    {error, {element, [{element, Other}]}}.


check_callback({Mod, Func}) when erlang:is_atom(Mod) andalso
                                 erlang:is_atom(Func)     ->
    true;
check_callback(_) ->
    false.
