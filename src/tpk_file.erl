%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tpk_file).
-export([size/1, mtime/1, md5sum/1, is_useless/1, filter_useless/1, basename/1, rebase_filename/3]).
-export([temp_name/0, temp_name/1, mkdir/1, with_temp_dir/1,
         dir_contents/1, dir_contents/2, dir_contents/3,
         wildcard/2]).
-export([copy/2, delete/1, delete/2, walk/3, walk/4]).
-export([tarball_create/1, tarball_add_file/4, tarball_add_binary/4, tarball_add_link/4, tarball_mkdir/3,  tarball_close/1]).

-include_lib("kernel/include/file.hrl").

basename(Filename) ->
    Abs = filename:absname(Filename),
    case filename:basename(Abs) of
        "." -> filename:dirname(Abs);
        Other -> Other
    end.

size(Filename) ->
    {ok, #file_info{size = Size}} = file:read_file_info(Filename),
    Size.

mtime(Filename) ->
    {ok, #file_info{mtime = MTime}} = file:read_file_info(Filename),
    MTime.

rebase_filename(FName, FromDir, ToDir) ->
    FromDirPath = filename:split(FromDir),
    FPath = filename:split(FName),
    case lists:prefix(FromDirPath, FPath) of
        true ->
            RP = FPath -- FromDirPath,
            Joined = filename:join([ToDir|RP]),
            case ToDir of
                "" -> tl(Joined);
                _  -> Joined
            end;
        false ->
            exit(bad_filename)
    end.

is_useless(Filename) ->
    Name = basename(Filename),
    tpk_util:match(".*~$", Name) or tpk_util:match("^\\..*", Name) or tpk_util:match("^.*/\\.git/.*$", Filename).

filter_useless(Files) ->
    lists:filter(fun (X) -> not is_useless(X) end, Files).

temp_name() -> temp_name("/tmp").
temp_name(Dir) ->
    {A,B,C} = now(),
    Pid = re:replace(erlang:pid_to_list(self()), "<|>", "", [global, {return, list}]),
    filename:join(Dir, tpk_util:f("tetrapak-tmp-~p-~p-~p-~s", [A,B,C,Pid])).

with_temp_dir(DoSomething) ->
    Temp = temp_name(),
    file:make_dir(Temp),
    try DoSomething(Temp)
    after
        tpk_log:debug("deleting directory ~s", [Temp]),
        delete(Temp)
    end.

dir_contents(Dir) -> dir_contents(Dir, ".*").
dir_contents(Dir, Mask) -> dir_contents(Dir, Mask, no_dir).
dir_contents(Dir, Mask, DirOpt) ->
    AddL = fun (F, Acc) ->
                   case tpk_util:match(Mask, F) of
                       true -> [F|Acc];
                       false -> Acc
                   end
           end,
    case filelib:is_dir(Dir) of
        true -> lists:reverse(walk(AddL, [], Dir, DirOpt));
        false -> []
    end.

wildcard(Dir, Wildcard) ->
    WC = filename:join(filename:absname(Dir), Wildcard),
    filelib:wildcard(WC).

mkdir(Path) ->
    filelib:ensure_dir(filename:join(Path, ".")).

copy(From, To) ->
    CP = fun (F, _) ->
                case not is_useless(F) of
                    true ->
                        T = rebase_filename(F, From, To),
                        ok = filelib:ensure_dir(T),
                        case file:copy(F, T) of
                            {ok, _} ->
                                {ok, #file_info{mode = Mode}} = file:read_file_info(F),
                                file:change_mode(T, Mode);
                            {error, Reason} -> throw({file_copy_error, Reason})
                        end;
                    false -> nomatch
                end
        end,
    walk(CP, [], From, no_dir).

delete(Filename) ->
    delete(".*", Filename).
delete(Mask, Filename) ->
    walk(fun (F, _) -> delete_if_match(Mask, F) end, [], Filename, dir_last).

delete_if_match(Mask, Path) ->
    case tpk_util:match(Mask, filename:basename(Path)) of
        true ->
            case filelib:is_dir(Path) of
                true  ->
                    tpk_log:debug("rmdir ~s", [Path]),
                    file:del_dir(Path);
                false ->
                    tpk_log:debug("rm ~s", [Path]),
                    file:delete(Path)
            end;
        false -> ok
    end.

walk(Fun, AccIn, Path) -> walk(Fun, AccIn, Path, no_dir).
walk(Fun, AccIn, Path, DirOpt) when (DirOpt == no_dir) or
                                    (DirOpt == dir_first) or
                                    (DirOpt == dir_last) ->
    walk(Fun, {walk, Path}, AccIn, [], DirOpt).
walk(Fun, {Walk, Path}, Acc, Queue, DirOpt) ->
    case {Walk, filelib:is_dir(Path)} of
        {walk, true} ->
            {ok, List} = file:list_dir(Path),
            AddPaths = lists:map(fun (Name) -> {walk, filename:join(Path, Name)} end, List),
            [Next|Rest] = case DirOpt of
                              no_dir -> AddPaths ++ Queue;
                              dir_first -> [{nowalk, Path}|AddPaths] ++ Queue;
                              dir_last -> AddPaths ++ [{nowalk, Path}|Queue]
                          end,
            walk(Fun, Next, Acc, Rest, DirOpt);
        {_, _} ->
            case Queue of
                [] -> Fun(Path,Acc);
                [Next|Rest] -> walk(Fun, Next, Fun(Path, Acc), Rest, DirOpt)
            end
    end.

%% ---------------------------------------------------------
%% -- MD5
md5sum(File) ->
    case file:open(File, [binary,raw,read]) of
        {ok, P} ->
            Digest = md5_loop(P, erlang:md5_init()),
            {ok, << <<(nibble2hex(N))>> || <<N:4>> <= Digest >>};
        {error, Error} ->
            {error, Error}
    end.

md5_loop(P, C) ->
    case file:read(P, 4096) of
        {ok, Bin} ->
            md5_loop(P, erlang:md5_update(C, Bin));
        eof ->
            file:close(P),
            erlang:md5_final(C)
    end.

nibble2hex(X) when X < 10 -> X + $0;
nibble2hex(X)             -> X - 10 + $a.

%% ----------------------------------------------------------
%% Tarballs (erl_tar doesn't cut it...)
-define(TAR_BLOCKSIZE, 512).

tarball_create(Filename) ->
    case file:open(Filename, [write, binary, raw, compressed]) of
        {ok, Device} ->
            {ok, {tar, Device}};
        {error, Error} ->
            {error, Error}
    end.

tarball_add_file({tar, TarFile}, RealPath, VirtualPath, Options) ->
    case file:read_link_info(RealPath) of
        {ok, Info} when Info#file_info.type == regular ->
            tar_write_regular(TarFile, RealPath, tar_options(Info, VirtualPath, Options));
        {ok, Info} when Info#file_info.type == symlink ->
            {ok, LinkTarget} = file:read_link(RealPath),
            case proplists:get_bool(dereference, Options) of
                true ->
                    tarball_add_file({tar, TarFile}, LinkTarget, VirtualPath, Options);
                false ->
                    if length(LinkTarget) > 100 -> {error, link_size};
                       true ->
                           NewOptions = [{link_target, LinkTarget}, {size, 0} | tar_options(Info, VirtualPath, Options)],
                           file:write(TarFile, ustar_header(NewOptions))
                    end
            end;
        {ok, _Info} ->
            {error, bad_type};
        {error, Error} ->
            {error, Error}
    end.

tarball_add_binary({tar, TarFile}, VirtualPath, Binary, Options) when is_binary(Binary) ->
    Size = byte_size(Binary),
    FileInfo = #file_info{type = regular, size = Size, mtime = calendar:local_time(), mode = 8#664},
    file:write(TarFile, ustar_header(tar_options(FileInfo, VirtualPath, Options))),
    file:write(TarFile, Binary),
    Padding  = ?TAR_BLOCKSIZE - (Size rem ?TAR_BLOCKSIZE),
    (Padding > 0) andalso file:write(TarFile, <<0:(8 * Padding)>>),
    ok.

tarball_add_link({tar, TarFile}, VirtualPath, LinkTarget, Options) ->
    FileInfo = #file_info{type = symlink, size = 0, mtime = calendar:local_time(), mode = 8#664},
    if length(LinkTarget) > 100 -> {error, link_size};
       true ->
           NewOptions = [{link_target, LinkTarget} | tar_options(FileInfo, VirtualPath, Options)],
           file:write(TarFile, ustar_header(NewOptions))
    end.

tarball_mkdir({tar, TarFile}, VirtualPath, Options) ->
    FileInfo = #file_info{type = directory, size = 0, mtime = calendar:local_time(), mode = 8#664},
    file:write(TarFile, ustar_header(tar_options(FileInfo, VirtualPath, Options))).

tarball_close({tar, Device}) ->
    NullBlocks = <<0:8192>>,
    file:write(Device, NullBlocks),
    file:close(Device).

tar_options(Info = #file_info{}, VirtualPath, Options) ->
    Base = [{size,  Info#file_info.size},
            {mtime, tpk_util:unix_time(erlang:localtime_to_universaltime(Info#file_info.mtime))},
            {mode,  Info#file_info.mode},
            {type,  tar_ftype(Info#file_info.type)},
            {name,  VirtualPath},
            {owner, <<>>},
            {group, <<>>}],
    lists:foldl(fun ({mode, Mode}, Acc) when is_integer(Mode)  -> [{mode, Mode} | Acc];
                    ({owner, Owner}, Acc) when is_list(Owner)  -> [{owner, list_to_binary(Owner)} | Acc];
                    ({group, Group}, Acc) when is_list(Group)  -> [{group, list_to_binary(Group)} | Acc];
                    ({mtime, Mtime}, Acc) when is_tuple(Mtime) ->
                        [{mtime, tpk_util:unix_time(erlang:localtime_to_universaltime(Mtime))} | Acc];
                    ({link_target, T}, Acc) when is_list(T) ->
                        length(T) > 100 andalso error(link_size),
                        [{link_target, T} | Acc];
                    (_Other, Acc) ->
                        Acc
                end, Base, proplists:compact(Options)).

tar_ftype(regular)   -> "0";
tar_ftype(symlink)   -> "1";
tar_ftype(directory) -> "5".

tar_write_regular(TarFile, FilePath, Options) ->
    case file:open(FilePath, [read, raw, binary]) of
        {ok, File} ->
            file:write(TarFile, ustar_header(lists:keydelete(link_target, 1, Options))),
            file:copy(File, TarFile),
            Padding = ?TAR_BLOCKSIZE - (proplists:get_value(size, Options) rem ?TAR_BLOCKSIZE),
            (Padding > 0) andalso file:write(TarFile, <<0:(8 * Padding)>>),
            file:close(File);
        {error, Error} ->
            {error, Error}
    end.

pad0(Size, Binary) when is_binary(Binary) ->
    BSize = byte_size(Binary),
    if BSize >= Size -> Binary;
       true          -> <<Binary/bytes, 0:((Size - BSize) * 8)>>
    end;
pad0(Size, List) when is_list(List) ->
    pad0(Size, list_to_binary(List)).

ustar_header(Fields) ->
    FullName = proplists:get_value(name,  Fields),
    {NamePrefix, NameSuffix} = if length(FullName) =< 100 -> {<<>>, FullName};
                                  true                    -> lists:split(100, FullName)
                               end,
    Header = <<(pad0(100, NameSuffix))/bytes,
               (list_to_binary(io_lib:fwrite("~6.8.0b \0", [proplists:get_value(mode,  Fields)])))/bytes,
               (list_to_binary(io_lib:fwrite("~6.8.0b \0", [0])))/bytes,
               (list_to_binary(io_lib:fwrite("~6.8.0b \0", [0])))/bytes,
               (list_to_binary(io_lib:fwrite("~11.8.0b ",  [proplists:get_value(size,  Fields)])))/bytes,
               (list_to_binary(io_lib:fwrite("~11.8.0b ",  [proplists:get_value(mtime, Fields)])))/bytes,
               "        ", %% dummy checksum
               (list_to_binary(proplists:get_value(type, Fields)))/bytes,
               (pad0(100, proplists:get_value(link_target, Fields, <<>>)))/bytes,
               "ustar\0",
               "00",
               (pad0(32, proplists:get_value(owner, Fields)))/bytes,
               (pad0(32, proplists:get_value(group, Fields)))/bytes,
               "000000 \0",
               "000000 \0",
               (pad0(155, NamePrefix))/bytes>>,
    Checksum = list_to_binary(io_lib:fwrite("~6.8.0b\0 ", [sumbytes(Header, 0)])),
    <<BeforeChecksum:148/bytes, _DummyChecksum:8/bytes, AfterChecksum:344/bytes>> = Header,
    <<BeforeChecksum:148/bytes, Checksum:8/bytes, AfterChecksum:344/bytes, 0:96>>.

sumbytes(<<B1, B2, B3, B4, B5, B6, B7, B8, Rest/binary>>, Sum) ->
    sumbytes(Rest, Sum + B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8);
sumbytes(<<B1, Rest/binary>>, Sum) ->
    sumbytes(Rest, Sum + B1);
sumbytes(<<>>, Sum) ->
    Sum.
