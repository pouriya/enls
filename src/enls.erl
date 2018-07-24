-module(enls).

-export([value/3]).


value(Key, Def, Filter) ->
    case application:get_env(Key, elns, Def) of
        Def ->
            Def;
        Val ->
            case Filter of
                {Mod, Func} ->
                    try Mod:Func(Key, Def, Val) of
                        ok ->
                            Val;
                        true ->
                            Val;
                        {ok, NewVal} ->
                            NewVal;
                        {error, Rsn} ->
                            erlang:error({value, [{reason, Rsn}
                                                 ,{value, Val}
                                                 ,{key, Key}]});
                        false ->
                            erlang:error({value, [{value, Val}, {key, Key}]});
                        Other ->
                            erlang:error({return, [{returned_value, Other}
                                                  ,{value, Val}
                                                  ,{filter, Filter}
                                                  ,{key, key}]})
                    catch
                        Type:Rsn ->
                            erlang:error({exception
                                         ,[{reason, Rsn}
                                          ,{stacktrace, erlang:get_stacktrace()}
                                          ,{value, Val}
                                          ,{filter, Filter}
                                          ,{key, Key}
                                          ,{type, Type}]})
                    end;
                _ -> % undefined
                    Val
            end
    end.