%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_pass_package).
-behaviour(tep_pass).

-include("tetrapak.hrl").

-export([pass_options/1, pass_run/3]).

-passinfo({pkg, [
    {deb, "Create a binary debian package"}
]}).

%% ------------------------------------------------------------
%% -- Pass API
pass_options(pkg) ->
    [#option{name = outdir, type = directory}].

pass_run({pkg, Template}, Project, Options) ->
    tep_pass:require_all([build, check]),

    OutDir = case proplists:get_value(outdir, Options) of
                 undefined -> dist_dir(Project);
                 Dir       -> filename:absname(Dir)
             end,

    InDir = Project#tep_project.directory,
    case find_template_mod(Template) of
        {ok, TMod} ->
            tep_log:info("applying packaging template ~s", [Template]),
            Job = #tep_job{source_dir = InDir,
                           template = TMod,
                           template_dir = template_dir(TMod),
                           files = otp_related_files(InDir),
                           output_dir = OutDir},
            case TMod:create_package(Project, Job) of  
                {ok, File} -> 
                    tep_log:info("packaging done, package is at: ~s", File);
                {error, Error} -> 
                    tep_pass:fail("packaging failed with error: ~p", [Error])
            end;
        {error, _} ->
            tep_pass:fail("template ~s not found", [Template]) 
    end.

%% ------------------------------------------------------------
%% -- Implementation
find_template_mod(Name) when is_atom(Name) ->
    find_template_mod(atom_to_list(Name)).

template_dir(Module) ->
    "tetrapak_tpl_" ++ Name = atom_to_list(Module),
    filename:join([code:priv_dir(tetrapak), "templates", Name]).

dist_dir(#tep_project{directory = Dir}) ->
    Dist = filename:join(Dir, "dist"),
    case filelib:is_dir(Dist) of
        true -> Dist;
        false ->
            case filelib:is_regular(Dist) of
                true  -> tep_pass:fail("package output directory ~s is a regular file", [Dist]);
                false -> 
                    file:make_dir(Dist),
                    Dist 
            end
    end.

otp_related_files(D) ->
    tep_file:wildcard(D, "ebin/*.beam") ++
    tep_file:wildcard(D, "ebin/*.app") ++
    tep_file:wildcard(D, "ebin/*.appup") ++
    tep_file:wildcard(D, "include/*.hrl") ++
    tep_file:filter_useless(tep_file:dir_contents(filename:join(D, "bin"))) ++
    tep_file:filter_useless(tep_file:dir_contents(filename:join(D, "priv"))).
