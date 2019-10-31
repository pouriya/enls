-module(enls_compiler).

-export([load/2, check_module/3]).

-define(SPECIAL_FUNCTION, '$enls_module').

load(Data, Opts) ->
    load(Data, Opts, maps:get(module, Opts, enls_dynamic)).



load(LangsData, Opts, Mod) ->
    NotFoundReturn = maps:get(not_found_return, Opts, "TRANSLATION NOT FOUND"),
    ModAttr = attribute(module, [atom(Mod)]),
    ExportAllAttr = attribute(compile, [atom(export_all)]),
    NoWarnExportAllAttr = attribute(compile, [atom(nowarn_export_all)]),
    TranslateFuncs = [make_translate_function(Data, Lang, string(NotFoundReturn)) || {Lang, Data} <- maps:to_list(LangsData)],
    {ok, _, Binary} = compile:forms([ModAttr, ExportAllAttr, NoWarnExportAllAttr | TranslateFuncs], [return_errors]),
    {module, _} = code:load_binary(Mod, erlang:atom_to_list(Mod), Binary),
    ok.


make_translate_function(Data, Lang, NotFoundReturn) ->
    LastClause = clause([underscore()], [], [NotFoundReturn]),
    do_make_translate_function(Data, Lang, [LastClause]).

do_make_translate_function([{Text1, Text2}|Data], Lang, Clauses) ->
    Pattern = [string(Text1)],
    Body = [string(Text2)],
    do_make_translate_function(Data, Lang, [clause(Pattern, [], Body)|Clauses]);
do_make_translate_function([], Lang, Clauses) ->
    function(Lang, Clauses).

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
