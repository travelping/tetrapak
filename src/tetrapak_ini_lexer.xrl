%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

Definitions.
D = [0-9]

Rules.
\.|=|,|\{|\}|\[|\]                 : {token, {list_to_atom(TokenChars), TokenLine}}.
[-]?{D}+                           : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
[-]?{D}+\.{D}+((E|e)(\+|\-)?{D}+)? : {token, {number, TokenLine, list_to_float(TokenChars)}}.
[a-z][0-9a-zA-Z_]*                 : {token, {atom, TokenLine, TokenChars}}.
"([^\\"]*(\\.)?)*"                 : to_erlang(string, TokenLine, TokenChars).
'([^\\']*(\\.)?)*'                 : to_erlang(quoted_atom, TokenLine, TokenChars).
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

to_erlang(Type, TokenLine, Chars) ->
    case erl_scan:string(Chars, TokenLine) of
        {ok, [{_EType, Line, Content}], _Endl} ->
            {token, {Type, Line, Content}};
        {error, {_Line, EMod, EDesc}, _Endl} ->
            {error, EMod:format_error(EDesc)}
    end.
