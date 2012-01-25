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