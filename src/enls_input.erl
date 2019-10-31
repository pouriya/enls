-module(enls_input).

-export([get_languages/1, load/1, read_erlang_file/2]).


get_languages(#{shell_env_key := Key}) ->
    case os:getenv(Key) of
        false ->
            case application:get_env(enls_languages) of
                {ok, Langs}=Ok when erlang:length(Langs) > 0 ->
                    Ok;
                _ -> % undefined | {ok, _}
                    {ok, [en]}
            end;
        Lang ->
            parse_language(Lang)
    end;

get_languages(Opts) ->
    get_languages(Opts#{shell_env_key => "LANG"}).


load(#{data := Data}) ->
    case check_data(Data) of
        ok ->
            {ok, Data};
        Err ->
            Err
    end;

load(#{languages := Langs}=Opts) ->
    {Mod, Func} = maps:get(reader, Opts, {?MODULE, read_erlang_file}),
    Path = maps:get(
        path,
        Opts,
        case application:get_application() of
            {ok, App} ->
                code:priv_dir(App);
            undefined ->
                code:priv_dir(enls)
        end
    ),
    load_languages(Langs, Mod, Func, Path, Opts, #{});

load(Opts) when erlang:is_map(Opts) ->
    case get_languages(Opts) of
        {ok, Langs} ->
            load(Opts#{languages => [Langs]});
        Err ->
            Err
    end.



load_languages([Lang | Langs], Mod, Func, Path, Opts, Ret) ->
    File = filename:join([Path, erlang:atom_to_list(Lang) ++ ".msg"]),
    try Mod:Func(File, Opts) of
        {ok, LangData} ->
            case check_language_data(LangData, []) of
                ok ->
                    load_languages(Langs, Mod, Func, Path, Opts, Ret#{Lang => LangData});
                Err ->
                    Err
            end;
        {error, {Reason, ErrParams}} when erlang:is_atom(Reason) andalso
                                          erlang:is_map(ErrParams)    ->
            {error, {Reason, ErrParams#{language => Lang, file => File}}};
        Other ->
            {error, {load_file, #{returned_value => Other, file => File}}}
    catch
        _:Reason ->
            {error, {load_file, #{reason => Reason, file => File}}}
    end;

load_languages([], _, _, _, _, Ret) ->
    {ok, Ret}.


read_erlang_file(File, _) ->
    case file:consult(File) of
        {ok, _}=Ok ->
            Ok;
        {_, Reason} ->
            {
                error,
                {
                    read_file,
                    #{reason => Reason, info => file:format_error(Reason)}
                }
            }
    end.


check_data([{Lang, Data} | Rest]) ->
    case check_language_data(Data, []) of
        ok ->
            check_data(Rest);
        {_, {Reason, ErrorParams}} ->
            {error, {Reason, ErrorParams#{language => Lang}}}
    end;

check_data([]) ->
    ok;

check_data(Map) when erlang:is_map(Map) ->
    check_data(maps:to_list(Map)).


check_language_data(
   [{Text1, Text2}|Data],
   Texts
) when erlang:is_list(Text1) andalso erlang:is_list(Text2) ->
    case lists:member(Text1, Texts) of
        false ->
            check_language_data(Data, [Text1 | Texts]);
        true ->
            {error, {check_data, #{duplicate_text => Text1}}}
    end;

check_language_data([], _) ->
    ok;

% error clauses:
check_language_data([{Other, Text}|_], _) when erlang:is_list(Text) ->
    {error, {check_data, #{text => Other}}};
check_language_data([{Text, Other}|_], _) when erlang:is_list(Text) ->
    {error, {check_data, #{text => Other}}};
check_language_data([Other|_], _) ->
    {error, {check_data, #{element => Other}}};
check_language_data(Other, _) ->
    {error, {check_data, #{list => Other}}}.



parse_language([FirstChar, SecondChar|_]=Value) ->
    case to_atom([FirstChar, SecondChar]) of
        unknown ->
            {error, {parse_language, #{value => Value}}};
        Lang ->
            {ok, Lang}
    end;

parse_language(Value) ->
    {error, {parse_language, #{value => Value}}}.


%  Copy HTML representation of the languages table at
% https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes and save it in a file
% langs.html and run:
% $ cat langs.html | awk 'BEGIN {FS = "[</>]"} $8 == "www.loc.gov" {print "to_atom(\""$13"\") ->\n    " $13 ";"} END {print "to_atom(_) ->\n    unknown."}'

to_atom("ab") ->
    ab;
to_atom("aa") ->
    aa;
to_atom("af") ->
    af;
to_atom("ak") ->
    ak;
to_atom("sq") ->
    sq;
to_atom("am") ->
    am;
to_atom("ar") ->
    ar;
to_atom("an") ->
    an;
to_atom("hy") ->
    hy;
to_atom("as") ->
    as;
to_atom("av") ->
    av;
to_atom("ae") ->
    ae;
to_atom("ay") ->
    ay;
to_atom("az") ->
    az;
to_atom("bm") ->
    bm;
to_atom("ba") ->
    ba;
to_atom("eu") ->
    eu;
to_atom("be") ->
    be;
to_atom("bn") ->
    bn;
to_atom("bh") ->
    bh;
to_atom("bi") ->
    bi;
to_atom("bs") ->
    bs;
to_atom("br") ->
    br;
to_atom("bg") ->
    bg;
to_atom("my") ->
    my;
to_atom("ca") ->
    ca;
to_atom("ch") ->
    ch;
to_atom("ce") ->
    ce;
to_atom("ny") ->
    ny;
to_atom("zh") ->
    zh;
to_atom("cv") ->
    cv;
to_atom("kw") ->
    kw;
to_atom("co") ->
    co;
to_atom("cr") ->
    cr;
to_atom("hr") ->
    hr;
to_atom("cs") ->
    cs;
to_atom("da") ->
    da;
to_atom("dv") ->
    dv;
to_atom("nl") ->
    nl;
to_atom("dz") ->
    dz;
to_atom("en") ->
    en;
to_atom("eo") ->
    eo;
to_atom("et") ->
    et;
to_atom("ee") ->
    ee;
to_atom("fo") ->
    fo;
to_atom("fj") ->
    fj;
to_atom("fi") ->
    fi;
to_atom("fr") ->
    fr;
to_atom("ff") ->
    ff;
to_atom("gl") ->
    gl;
to_atom("ka") ->
    ka;
to_atom("de") ->
    de;
to_atom("el") ->
    el;
to_atom("gn") ->
    gn;
to_atom("gu") ->
    gu;
to_atom("ht") ->
    ht;
to_atom("ha") ->
    ha;
to_atom("he") ->
    he;
to_atom("hz") ->
    hz;
to_atom("hi") ->
    hi;
to_atom("ho") ->
    ho;
to_atom("hu") ->
    hu;
to_atom("ia") ->
    ia;
to_atom("id") ->
    id;
to_atom("ie") ->
    ie;
to_atom("ga") ->
    ga;
to_atom("ig") ->
    ig;
to_atom("ik") ->
    ik;
to_atom("io") ->
    io;
to_atom("is") ->
    is;
to_atom("it") ->
    it;
to_atom("iu") ->
    iu;
to_atom("ja") ->
    ja;
to_atom("jv") ->
    jv;
to_atom("kl") ->
    kl;
to_atom("kn") ->
    kn;
to_atom("kr") ->
    kr;
to_atom("ks") ->
    ks;
to_atom("kk") ->
    kk;
to_atom("km") ->
    km;
to_atom("ki") ->
    ki;
to_atom("rw") ->
    rw;
to_atom("ky") ->
    ky;
to_atom("kv") ->
    kv;
to_atom("kg") ->
    kg;
to_atom("ko") ->
    ko;
to_atom("ku") ->
    ku;
to_atom("kj") ->
    kj;
to_atom("la") ->
    la;
to_atom("lb") ->
    lb;
to_atom("lg") ->
    lg;
to_atom("li") ->
    li;
to_atom("ln") ->
    ln;
to_atom("lo") ->
    lo;
to_atom("lt") ->
    lt;
to_atom("lu") ->
    lu;
to_atom("lv") ->
    lv;
to_atom("gv") ->
    gv;
to_atom("mk") ->
    mk;
to_atom("mg") ->
    mg;
to_atom("ms") ->
    ms;
to_atom("ml") ->
    ml;
to_atom("mt") ->
    mt;
to_atom("mi") ->
    mi;
to_atom("mr") ->
    mr;
to_atom("mh") ->
    mh;
to_atom("mn") ->
    mn;
to_atom("na") ->
    na;
to_atom("nv") ->
    nv;
to_atom("nd") ->
    nd;
to_atom("ne") ->
    ne;
to_atom("ng") ->
    ng;
to_atom("nb") ->
    nb;
to_atom("nn") ->
    nn;
to_atom("no") ->
    no;
to_atom("ii") ->
    ii;
to_atom("nr") ->
    nr;
to_atom("oc") ->
    oc;
to_atom("oj") ->
    oj;
to_atom("cu") ->
    cu;
to_atom("om") ->
    om;
to_atom("or") ->
    'or'; % :-S
to_atom("os") ->
    os;
to_atom("pa") ->
    pa;
to_atom("pi") ->
    pi;
to_atom("fa") ->
    fa;
to_atom("pl") ->
    pl;
to_atom("ps") ->
    ps;
to_atom("pt") ->
    pt;
to_atom("qu") ->
    qu;
to_atom("rm") ->
    rm;
to_atom("rn") ->
    rn;
to_atom("ro") ->
    ro;
to_atom("ru") ->
    ru;
to_atom("sa") ->
    sa;
to_atom("sc") ->
    sc;
to_atom("sd") ->
    sd;
to_atom("se") ->
    se;
to_atom("sm") ->
    sm;
to_atom("sg") ->
    sg;
to_atom("sr") ->
    sr;
to_atom("gd") ->
    gd;
to_atom("sn") ->
    sn;
to_atom("si") ->
    si;
to_atom("sk") ->
    sk;
to_atom("sl") ->
    sl;
to_atom("so") ->
    so;
to_atom("st") ->
    st;
to_atom("es") ->
    es;
to_atom("su") ->
    su;
to_atom("sw") ->
    sw;
to_atom("ss") ->
    ss;
to_atom("sv") ->
    sv;
to_atom("ta") ->
    ta;
to_atom("te") ->
    te;
to_atom("tg") ->
    tg;
to_atom("th") ->
    th;
to_atom("ti") ->
    ti;
to_atom("bo") ->
    bo;
to_atom("tk") ->
    tk;
to_atom("tl") ->
    tl;
to_atom("tn") ->
    tn;
to_atom("to") ->
    to;
to_atom("tr") ->
    tr;
to_atom("ts") ->
    ts;
to_atom("tt") ->
    tt;
to_atom("tw") ->
    tw;
to_atom("ty") ->
    ty;
to_atom("ug") ->
    ug;
to_atom("uk") ->
    uk;
to_atom("ur") ->
    ur;
to_atom("uz") ->
    uz;
to_atom("ve") ->
    ve;
to_atom("vi") ->
    vi;
to_atom("vo") ->
    vo;
to_atom("wa") ->
    wa;
to_atom("cy") ->
    cy;
to_atom("wo") ->
    wo;
to_atom("fy") ->
    fy;
to_atom("xh") ->
    xh;
to_atom("yi") ->
    yi;
to_atom("yo") ->
    yo;
to_atom("za") ->
    za;
to_atom("zu") ->
    zu;
to_atom(_) ->
    unknown.
