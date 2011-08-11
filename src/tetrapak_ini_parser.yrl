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

Nonterminals config section sectionhdr confobj confobjhdr assignment assignments identifier value list tuple elements.
Terminals '=' '[' ']' '{' '}' ',' '.' atom quoted_atom string number.
Rootsymbol config.

config      -> '$empty'                    : [].
config      -> assignments config          : [{section, "", '$1'} | '$2'].
config      -> section config              : ['$1' | '$2'].
config      -> confobj config              : ['$1' | '$2'].
section     -> sectionhdr assignments      : {section, '$1', '$2'}.
sectionhdr  -> '[' identifier ']'          : '$2'.
confobj     -> confobjhdr assignments      : {object, '$1', '$2'}.
confobjhdr  -> '[' identifier string ']'   : {'$2', value_of('$3')}.
assignments -> assignment                  : ['$1'].
assignments -> assignment assignments      : ['$1' | '$2'].
assignment  -> identifier '=' value        : {'$1', '$3'}.
identifier  -> atom                        : value_of('$1').
identifier  -> atom '.' identifier         : value_of('$1') ++ "." ++ '$3'.
value       -> atom                        : list_to_atom(value_of('$1')).
value       -> string                      : value_of('$1').
value       -> number                      : value_of('$1').
value       -> quoted_atom                 : value_of('$1').
value       -> list                        : '$1'.
value       -> tuple                       : '$1'.
list        -> '[' ']'                     : [].
list        -> '[' elements ']'            : '$2'.
tuple       -> '{' '}'                     : {}.
tuple       -> '{' elements '}'            : list_to_tuple('$2').
elements    -> value                       : ['$1'].
elements    -> value ',' elements          : ['$1' | '$3'].

Erlang code.
-export([file/1]).
value_of(Token) ->
    element(3, Token).

file(Filename) ->
    case tetrapak_ini_lexer:file(Filename) of
        {ok, Tokens, _Endl} -> ?MODULE:parse(Tokens);
        Error               -> Error
    end.
