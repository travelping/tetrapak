%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

Definitions.
D     = [0-9]

Rules.
\.|=|,|\{|\}|\[|\]                 : {token, {list_to_atom(TokenChars), TokenLine}}.
[-]?{D}+                           : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
[-]?{D}+\.{D}+((E|e)(\+|\-)?{D}+)? : {token, {number, TokenLine, list_to_float(TokenChars)}}.
[a-z][0-9a-zA-Z_]*                 : {token, {atom, TokenLine, TokenChars}}.
"([^\\"]*(\\.)?)*"                 : {token, {string, TokenLine, do_string(tl(TokenChars), [])}}.
[\s\n\r\t]+                        : skip_token.
\%[^\n]*                           : skip_token.

Erlang code.
-export([file/1]).

file(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            case string(binary_to_list(Content)) of
                {error, Error, _} -> {error, Error};
                Other             -> Other
            end;
        {error, Error} ->
            {error, {file, Error}}
    end.

do_string([$\\, $n | R], Acc) -> do_string(R, [$\n | Acc]);
do_string([$\\, $t | R], Acc) -> do_string(R, [$\t | Acc]);
do_string([$\\, $b | R], Acc) -> do_string(R, [$\b | Acc]);
do_string([$\\, $f | R], Acc) -> do_string(R, [$\f | Acc]);
do_string([$\\, $s | R], Acc) -> do_string(R, [$\s | Acc]);
do_string([$\\, C  | R], Acc) -> do_string(R, [C | Acc]);
do_string([$"], Acc) -> lists:reverse(Acc);
do_string([C | R], Acc) -> do_string(R, [C | Acc]).
