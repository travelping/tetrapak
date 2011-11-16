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

-module(tetrapak_task_config).
-behaviour(tetrapak_task).
-export([find_app_file/2]).
-export([run/2, check/1]).

check("config:vcs") ->
    {needs_run, tetrapak:config("build.vcs_type")}.

run("config:appfile", _) ->
    tetrapak:require("build:appfile"),
    case find_app_file("ebin", ".app") of
        {ok, Appfile} ->
            case file:consult(Appfile) of
                {ok, [Attrs]} ->
                    {done, appfile_info(Appfile, Attrs)};
                {error, SyntaxError} ->
                    EDesc = file:format_error(SyntaxError),
                    tetrapak:fail("syntax error in application resource file:~n    ~s~n", [EDesc])
            end;
        {error, FileError} ->
            tetrapak:fail("app file could not be found: ~p", [FileError])
    end;

run("config:vcs", git) ->
    case run_git("rev-parse", ["--symbolic-full-name", "HEAD"]) of
        <<"refs/heads/", Branch/binary>>   -> ok;
        <<"refs/remotes/", Branch/binary>> -> ok;
        <<"HEAD\n">>                       -> Branch = <<"UNKNOWN">>
    end,
    Describe = run_git("describe", ["--tags", "--dirty=-dirty", "--long", "--always"]),
    case re:run(Describe, "^(.*)-([0-9]+)-g([0-9a-f]+)(-dirty)?\n$", [{capture, all_but_first, list}]) of
        {match, [LastTag, Offset, LastCommit]} ->
            Data = [{last_tag, LastTag}, {offset, Offset}, {commit, LastCommit}, {dirty, false}];
        {match, [LastTag, Offset, LastCommit, _Dirty]} ->
            Data = [{last_tag, LastTag}, {offset, Offset}, {commit, LastCommit}, {dirty, true}];
        nomatch ->
            case re:run(Describe, "^([0-9a-f]+)(-dirty)?\n$", [{capture, all_but_first, list}]) of
                {match, [LastCommit]} ->
                    Data = [{last_tag, ""}, {offset, "0"}, {commit, LastCommit}, {dirty, false}];
                {match, [LastCommit, _Dirty]} ->
                    Data = [{last_tag, ""}, {offset, "0"}, {commit, LastCommit}, {dirty, true}];
                nomatch ->
                    Data = [],
                    tetrapak:fail("failed to parse the output of 'git describe'")
            end
    end,
    {done, [{branch, string:strip(binary_to_list(Branch), right, $\n)} | Data]};

run("config:vcs", hg) ->
    IdentOutput = run_hg("identify", ["-i", "-n", "-b", "-t"]),
    case re:split(IdentOutput, " ", [{return, list}])  of
        [Rev, Num, Branch, Tag] ->
            Data = [{branch, Branch}, {last_tag, Tag}],
            case strip_plus(Rev) of
                Rev ->
                    %% Rev is unchanged, no '+' at the end
                    {done, [{commit, Rev}, {offset, Num}, {dirty, false} | Data]};
                RevHash ->
                    {done, [{commit, RevHash}, {offset, strip_plus(Num)}, {dirty, true} | Data]}
            end;
        _Other ->
            tetrapak:fail("could not parse the output of 'hg identify'")
    end;

run("config:vcs", Unknown) ->
    tetrapak:fail("unknown VCS type: ~p", [Unknown]).

%% ------------------------------------------------------------
%% -- Project info
appfile_info(File, {application, Name, Attrs}) ->
    [{"name",    Name},
     {"path",    File},
     {"vsn",     app_attr(File, vsn, Attrs)},
     {"deps",    app_attr(File, applications, Attrs) -- [stdlib,kernel]},
     {"desc",    proplists:get_value(description, Attrs, "")},
     {"modules", app_attr(File, modules, Attrs)}].

app_attr(File, Key, Attrs) ->
    case proplists:get_value(Key, Attrs) of
        undefined ->
            io:format("~s: required property missing: ~s~n", [Key, File]),
            throw({error, application_prop_missing});
        Val -> Val
    end.

find_app_file(Dir, Extension) ->
    OrigDir = tetrapak:dir(),
    Candidates = filelib:wildcard(filename:join(tetrapak:path(Dir), "*" ++ Extension)),
    case {Candidates, dir_to_appname(OrigDir)} of
        {[], _} ->
            {error, no_app_file};
        {Files, nomatch} ->
            io:format("Warning: project directory name not OTP-compliant~n", []),
            {ok, hd(Files)};
        {Files, Appname} ->
            case lists:filter(fun (F) -> filename:rootname(F) =:= Appname end, Files) of
                [] -> {ok, hd(Files)};
                [File|_] -> {ok, File}
            end
    end.

dir_to_appname(Dir) ->
    Base = tpk_file:basename(Dir),
    case re:run(Base,"([a-z_])(-.*)?", [caseless,{capture,first,list}]) of
        {match, [Appname]} -> Appname;
        nomatch            -> nomatch
    end.

strip_plus(String) ->
    lists:filter(fun (C) -> C /= $+ end, String).

run_git(Cmd, Args) ->
    DirArg = "--git-dir=" ++ tetrapak:path(".git"),
    tetrapak:cmd("git", [DirArg, Cmd | Args]).

run_hg(Cmd, Args) ->
    tetrapak:cmd("hg", ["--noninteractive", "--repository", tetrapak:dir(), Cmd | Args]).
