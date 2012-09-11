-module(tetrapak_task_app).
-export([run/2]).

%% TODO: Possibility to get a new application name
run("create:application", _) ->
    Name = "new_erlang_app",
    AtomName = list_to_atom(Name),
    Path = tetrapak:path(Name),
    assert_tuple(file:make_dir(Name)),
    io:format("create: ~s~n", [Path]),
    Src = filename:join(Path, "src"),
    io:format("create: ~s~n", [Src]),
    assert_tuple(file:make_dir(Src)),
    FileName = Src ++ "/" ++ Name ++ ".app.src",
    case file:open(FileName, [write]) of
        {ok, File} ->
            write_in(File, AtomName),
            file:close(File),
            io:format("create: ~s~n", [FileName]);
        Error ->
            assert_tuple(Error)
    end.

write_in(File, Name) ->
    io:format(File, "{application, ~p, [~n", [Name]),
    io:format(File, "~n    {description, \"\"},
    {applications, [kernel, stdlib]},
    {registered, []},
    {env, []}~n]}.").

assert_tuple(Name) when is_tuple(Name) ->
    tetrapak:fail("Error: ~p", [Name]);
assert_tuple(_) ->
    ok.
