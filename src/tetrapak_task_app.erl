-module(tetrapak_task_app).
-export([run/2]).

run("new", _) ->
    case init:get_argument(app) of
        {ok, [[Name]]} ->
            create_app(Name);
        _ ->
            io:format("No name specified, use -app option~n", [])
    end.

create_app(Name) ->
    AtomName = list_to_atom(Name),
    %% create dirs
    Path = tetrapak:path(Name),
    Src = filename:join(Path, "src"),
    TetrapakDir = filename:join(Path, "tetrapak"),
    [create_dir(Dir) || Dir <- [Path, Src, TetrapakDir]],

    %% base name
    BaseName = Src ++ "/" ++ Name,

    %% create files

    AppSrc = BaseName ++ ".app.src",
    write_appsrc(AppSrc, AtomName),
    ErlangMod = BaseName ++ ".erl",
    write_erlmod(ErlangMod, AtomName).

create_dir(Name) ->
    assert_tuple(file:make_dir(Name)),
    io:format("create: ~s~n", [Name]).

write_appsrc(AppSrc, Name) ->
    Fun = fun(File) ->
                  io:format(File, "{application, ~p, [~n    {description, \"\"},
    {vsn, [\"0.0\"]},
    {applications, [kernel, stdlib]},
    {registered, []},
    {env, []}~n]}.", [Name])
           end,
    writer(Fun, AppSrc).

write_erlmod(ErlMod, Name) ->
    writer(fun(File) -> io:format(File, "-module(~p).", [Name]) end, ErlMod).

writer(Fun, Path) ->
    case file:open(Path, [write]) of
        {ok, File} ->
            Fun(File),
            file:close(File),
            io:format("create: ~s~n", [Path]);
        Error ->
            assert_tuple(Error)
    end.

assert_tuple(Name) when is_tuple(Name) ->
    tetrapak:fail("Error: ~p", [Name]);
assert_tuple(_) ->
    ok.
