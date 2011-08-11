% Copyright 2010-2011, Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

-module(tetrapak_task_appsrc).
-export([check/1, run/2]).

check("build:appfile") ->
    case tetrapak_task_config:find_app_file("src", ".app.src") of
        {error, no_app_file} ->
            done;
        {ok, AppSrc} ->
            {needs_run, AppSrc}
    end;

check("clean:appfile") ->
    case tetrapak_task_config:find_app_file("src", ".app.src") of
        {error, no_app_file} ->
            done;
        {ok, AppSrc} ->
            AppFile = filename:join(tetrapak:subdir("ebin"), filename:rootname(filename:basename(AppSrc), ".app.src") ++ ".app"),
            {needs_run, AppFile}
    end.

run("build:appfile", AppSrc) ->
    tetrapak:require("build:erlang"),
    AppSrcDisplayPath = tpk_file:rebase_filename(AppSrc, tetrapak:dir(), ""),
    case file:consult(AppSrc) of
        {ok, [{application, AppName, Keys}]} ->
            case {atom_to_list(AppName), filename:rootname(filename:basename(AppSrc), ".app.src")} of
                {AppNameString, AppNameString} ->
                    ok;
                {AppNameString, _} ->
                    tetrapak:fail("application name in ~s (~s) does not match filename", [AppSrcDisplayPath, AppNameString])
            end,
            Vsn = get_app_vsn(AppSrcDisplayPath, proplists:get_value(vsn, Keys), tetrapak:config("build.version")),
            NewKeys1 = lists:keystore(vsn, 1, Keys, {vsn, Vsn}),
            NewKeys2 = lists:keystore(modules, 1, NewKeys1, {modules, get_app_modules()}),
            write_appfile(AppName, NewKeys2);
        {ok, _} ->
            tetrapak:fail("~s has invalid term structure", [AppSrcDisplayPath]);
        {error, Error} when is_atom(Error) ->
            tetrapak:fail("could not open ~s: ~s", [AppSrcDisplayPath, file:format_error(Error)]);
        {error, Error = {_Line, _Mod, _EInfo}} ->
            tpk_util:show_error_info(AppSrcDisplayPath, Error),
            tetrapak:fail()
    end;

run("clean:appfile", AppFile) ->
    tpk_file:delete(AppFile),
    ok.

get_app_vsn(_AppSrc, _AppVsn, CfgVsn) when is_list(CfgVsn) ->
    expand_vsn(CfgVsn);
get_app_vsn(_AppSrc, AppVsn, undefined) when is_list(AppVsn) ->
    expand_vsn(AppVsn);
get_app_vsn(_AppSrc, git, undefined) ->
    %% match what git describe outputs, for rebar compatibility
    expand_vsn("~T{~t}~O{-~o}~T{-g}~c~D{-dirty}");
get_app_vsn(_AppSrc, undefined, AppVsn) when is_list(AppVsn) ->
    expand_vsn(AppVsn);
get_app_vsn(AppSrc, undefined, undefined) ->
    io:format("neither ~s nor the config contain version information~n", [AppSrc]),
    tetrapak:fail("could not determine application version").

expand_vsn([]) ->
    [];
expand_vsn("~~" ++ R) ->
    "~" ++ expand_vsn(R);
expand_vsn("~t" ++ R) ->
    tetrapak:get("config:vcs:last_tag") ++ expand_vsn(R);
expand_vsn("~c" ++ R) ->
    tetrapak:get("config:vcs:commit") ++ expand_vsn(R);
expand_vsn("~o" ++ R) ->
    tetrapak:get("config:vcs:offset") ++ expand_vsn(R);
expand_vsn("~b" ++ R) ->
    tetrapak:get("config:vcs:branch") ++ expand_vsn(R);
expand_vsn("~d" ++ R) ->
    build_timestamp() ++ expand_vsn(R);
expand_vsn("~O{" ++ R) ->
    expand_condition(R, "config:vcs:offset", "0");
expand_vsn("~T{" ++ R) ->
    expand_condition(R, "config:vcs:last_tag", "");
expand_vsn("~D{" ++ R) ->
    expand_condition(R, "config:vcs:dirty", false);
expand_vsn([C | R]) ->
    [C | expand_vsn(R)].

expand_condition(Str, CheckKey, EmptyValue) ->
    case re:run(Str, "([^\\}]*)\\}(.*)", [{capture, all_but_first, list}]) of
        {match, [Insert, Rest]} ->
            case tetrapak:get(CheckKey) of
                EmptyValue ->
                    expand_vsn(Rest);
                _ ->
                    expand_vsn(Insert) ++ expand_vsn(Rest)
            end
    end.

build_timestamp() ->
    {{Y,Mo,D},{H,Min,S}} = calendar:universal_time(),
    tpk_util:f("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B", [Y, Mo, D, H, Min, S]).

get_app_modules() ->
    [list_to_atom(filename:rootname(F, ".beam")) || F <- filelib:wildcard("*.beam", tetrapak:subdir("ebin"))].

write_appfile(AppName, Keys) ->
    OutputFile = filename:join(tetrapak:subdir("ebin"), atom_to_list(AppName) ++ ".app"),
    file:write_file(OutputFile, io_lib:fwrite("{application, ~s,~n  ~p~n}.", [AppName, Keys])).
