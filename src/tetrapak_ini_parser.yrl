% vim: ft=erlang
%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

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
