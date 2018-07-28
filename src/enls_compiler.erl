-module(enls_compiler).

-export([load/2, check_module/3]).

-define(SPECIAL_FUNCTION, '$enls_module').

load(Data, Opts) ->
    Mod = enls:value(module, Opts, enls_dynamic, {?MODULE, check_module}),
    case is_nls_module(Mod) of
        false ->
            {error, {not_allowed, [{module, Mod}]}};
        _ -> % false
            load(Data, Opts, Mod)
    end.



load(Data, Opts, Mod) ->
    ModAttr = attribute(module, [atom(Mod)]),
    ExportAllAttr = attribute(compile, [atom(export_all)]),
    SpecialFunc = function(?SPECIAL_FUNCTION, [clause([], [], [atom(ok)])]),
    TranslateFunc = make_translate_function(Data),
    {ok, _, Binary} = compile:forms([ModAttr, ExportAllAttr, SpecialFunc, TranslateFunc], [return_errors]),
    {module, _} = code:load_binary(Mod, erlang:atom_to_list(Mod), Binary),
    ok.



is_nls_module(Mod) ->
    try Mod:?SPECIAL_FUNCTION() of
        _ ->
            true
    catch
        _:_ ->
            false
    end.

make_translate_function(Data) ->
    LastClause = clause([underscore()], [], [atom(not_found)]),
    make_translate_function(Data, [LastClause]).

make_translate_function([{Id, Text}|Data], Clauses) ->
    Pattern = [integer(Id)],
    Body = [string(Text)],
    make_translate_function(Data, [clause(Pattern, [], Body)|Clauses]);
make_translate_function([], Clauses) ->
    function(translate, Clauses).

function(Name, Clauses) when erlang:is_atom(Name) andalso erlang:is_list(Clauses) ->
    abstract(function, [atom(Name), Clauses]);
function(Name, Clauses) when erlang:is_list(Clauses) ->
    abstract(function, [Name, Clauses]).
        
clause(Args, Guards, Bodies) when erlang:is_list(Args)    andalso
                                  (erlang:is_list(Guards) orelse 
                                   Guards =:= none)       andalso
                                  erlang:is_list(Bodies)       ->
    abstract(clause, [Args, Guards, Bodies]).


atom(Name) when erlang:is_atom(Name) ->
    abstract(atom, [Name]).


tuple(Elements) when erlang:is_list(Elements) ->
    abstract(tuple, [Elements]).


attribute(Name, Args) when erlang:is_atom(Name) andalso erlang:is_list(Args) ->
    abstract(attribute, [atom(Name), Args]).


map([]) ->
    abstract(map_expr, [[]]).


list([]) ->
    abstract(nil, []);
list(List) when erlang:is_list(List) ->
    abstract(list, [List]).


integer(Int) when erlang:is_integer(Int) ->
    abstract(integer, [Int]).


string(Str) when erlang:is_list(Str) ->
    abstract(string, [Str]).


binary(Str) when erlang:is_list(Str) ->
    abstract(binary, [[abstract(binary_field, [string(Str)])]]).


underscore() ->
    abstract(underscore, []).


abstract(Type, Args) ->
    MaybeTree = erlang:apply(erl_syntax, Type, Args),
    case erl_syntax:is_tree(MaybeTree) of
        true ->
            erl_syntax:revert(MaybeTree);
        _ -> % false
            MaybeTree
    end.


check_module(_, _, Arg) when erlang:is_atom(Arg) ->
    true;
check_module(_, _, _) ->
    false.
