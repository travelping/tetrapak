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

-module(tpk_file).
-export([size/1, mtime/1, md5sum/1, basename/1, relative_path/2, rebase_filename/3]).
-export([temp_name/0, temp_name/1, mkdir/1, with_temp_dir/1,
         dir_contents/1, dir_contents/2, dir_contents/3,
         wildcard/2]).
-export([copy/2, delete/1, delete/2, walk/3, walk/4]).
-export([tarball_create/1, tarball_add_file/4, tarball_add_binary/4, tarball_add_link/4, tarball_mkdir/3,
         tarball_mkdir_parents/3, tarball_close/1]).

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

relative_path(Filename, Dir) ->
    rebase_filename(Filename, Dir, "").

rebase_filename(FName, FromDir, ToDir) ->
    FromDirPath = filename:split(FromDir),
    FPath = filename:split(FName),
    case lists:prefix(FromDirPath, FPath) of
        true ->
            RP = FPath -- FromDirPath,
            Joined = filename:join([ToDir|RP]),
            case {Joined, ToDir} of
                {"", _} -> "";
                {_, ""} -> tl(Joined);
                {_, _}  -> Joined
            end;
        false ->
            FName
    end.

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
                 T = rebase_filename(F, From, To),
                 ok = filelib:ensure_dir(T),
                 case file:copy(F, T) of
                     {ok, _} ->
                         {ok, #file_info{mode = Mode}} = file:read_file_info(F),
                         file:change_mode(T, Mode);
                     {error, Reason} ->
                         throw({file_copy_error, Reason})
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
                    file:del_dir(Path), ok;
                false ->
                    file:delete(Path), ok
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
            AddPaths   = lists:map(fun (Name) -> {walk, filename:join(Path, Name)} end, List),
            case DirOpt of
                no_dir    -> NewQueue = AddPaths ++ Queue;
                dir_first -> NewQueue = [{nowalk, Path} | AddPaths] ++ Queue;
                dir_last  -> NewQueue = AddPaths ++ [{nowalk, Path} | Queue]
            end,
            case NewQueue of
                []            -> Acc;
                [Next | Rest] -> walk(Fun, Next, Acc, Rest, DirOpt)
            end;
        {_, _} ->
            case Queue of
                []          -> Fun(Path,Acc);
                [Next|Rest] -> walk(Fun, Next, Fun(Path, Acc), Rest, DirOpt)
            end
    end.

%% ---------------------------------------------------------
%% -- MD5
md5sum(File) ->
    case file:open(File, [binary, raw, read_ahead]) of
        {ok, P} ->
            Digest = md5_loop(P, erlang:md5_init()),
            {ok, tpk_util:to_hex(Digest)};
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
    tar_padding(TarFile, Size).

tarball_add_link({tar, TarFile}, VirtualPath, LinkTarget, Options) ->
    FileInfo = #file_info{type = symlink, size = 0, mtime = calendar:local_time(), mode = 8#664},
    if length(LinkTarget) > 100 -> {error, link_size};
       true ->
           NewOptions = [{link_target, LinkTarget} | tar_options(FileInfo, VirtualPath, Options)],
           file:write(TarFile, ustar_header(NewOptions))
    end.

tarball_mkdir({tar, TarFile}, VirtualPath, Options) ->
    FileInfo = #file_info{type = directory, size = 0, mtime = calendar:local_time(), mode = 8#755},
    file:write(TarFile, ustar_header(tar_options(FileInfo, VirtualPath, Options))).

tarball_mkdir_parents(Ball = {tar, _TarFile}, VirtualPath, Options) ->
    [First | Rest] = filename:split(VirtualPath),
    Parents = lists:foldl(fun (C, [Prefix | Acc]) ->
                                  [Prefix ++ "/" ++ C, Prefix | Acc]
                          end, [First], Rest),
    lists:foreach(fun (P) -> tarball_mkdir(Ball, P, Options) end, lists:reverse(Parents)).

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
tar_ftype(symlink)   -> "2";
tar_ftype(directory) -> "5".

tar_write_regular(TarFile, FilePath, Options) ->
    case file:open(FilePath, [read, raw, read_ahead, binary]) of
        {ok, File} ->
            file:write(TarFile, ustar_header(lists:keydelete(link_target, 1, Options))),
            {ok, Size} = file:copy(File, TarFile),
            tar_padding(TarFile, Size),
            file:close(File);
        {error, Error} ->
            {error, Error}
    end.

tar_padding(TarFile, Size) ->
    Padding = (?TAR_BLOCKSIZE - Size rem ?TAR_BLOCKSIZE) rem ?TAR_BLOCKSIZE,
    (Padding > 0) andalso file:write(TarFile, <<0:(8 * Padding)>>).

pad0(Size, Binary) when is_binary(Binary) ->
    BSize = byte_size(Binary),
    if BSize >= Size -> Binary;
       true          -> <<Binary/bytes, 0:((Size - BSize) * 8)>>
    end;
pad0(Size, List) when is_list(List) ->
    pad0(Size, list_to_binary(List)).

ustar_header(Fields) ->
    FullName = proplists:get_value(name,  Fields),
    {NamePrefix, NameSuffix} = ustar_split_filename(FullName),
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

ustar_split_filename(Name) when length(Name) < 100 ->
    {<<>>, unicode:characters_to_binary(string:join(filename:split(Name), "/"))};
ustar_split_filename(Name) ->
    {_L, Prefix, Suffix} =
        lists:foldr(fun (C, {Sum, Prefix, Suffix}) ->
                            BinC = unicode:characters_to_binary(C),
                            NewSum = Sum + byte_size(BinC) + 1,
                            if
                                NewSum < 100 ->
                                    {Sum + byte_size(BinC) + 1, Prefix, [BinC | Suffix]};
                                NewSum > 255 ->
                                    error(ustar_filename_too_long, [Name]);
                                true ->
                                    {Sum + byte_size(BinC) + 1, [BinC | Prefix], Suffix}
                            end
                    end, {0, [], []}, filename:split(Name)),
    {join_filename(Prefix), join_filename(Suffix)}.

join_filename([<<"/">>]) ->
    <<>>;
join_filename([P]) ->
    P;
join_filename([P | R]) ->
    <<P/binary, "/", (join_filename(R))/binary>>;
join_filename([]) ->
    <<>>.

sumbytes(<<B1, B2, B3, B4, B5, B6, B7, B8, Rest/binary>>, Sum) ->
    sumbytes(Rest, Sum + B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8);
sumbytes(<<B1, Rest/binary>>, Sum) ->
    sumbytes(Rest, Sum + B1);
sumbytes(<<>>, Sum) ->
    Sum.
