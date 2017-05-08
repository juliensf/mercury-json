%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015-2016, Julien Fischer.
% See the file COPYING for license details.
%
% This sub-module implements JSON pointer operations.
%
%-----------------------------------------------------------------------------%

:- module json.pointer.
:- interface.

:- pred string_to_reference_tokens(string::in, list(string)::out) is semidet.

:- func reference_tokens_to_string(list(string)) = string.

:- pred do_resolve(pointer::in, value::in, value::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
%
% String -> pointer conversion.
%

string_to_reference_tokens(PointerStr, RefTokens) :-
    string.to_char_list(PointerStr, PointerChars),
    (
        PointerChars = [],
        RefTokens = []
    ;
        PointerChars = [FirstChar | PointerCharsPrime],
        FirstChar = ('/'),  % semidet.
        next_reference_token(PointerCharsPrime, [], [], RevRefTokens),
        list.reverse(RevRefTokens, RefTokens)
    ).

:- pred next_reference_token(list(char)::in, list(char)::in,
    list(string)::in, list(string)::out) is semidet.

next_reference_token(PointerChars, !.NextTokenChars, !RevRefTokens) :-
    (
        PointerChars = [],
        NextTokenStr = string.from_rev_char_list(!.NextTokenChars),
        UnescapedNextTokenStr = unescape_string(NextTokenStr),
        !:RevRefTokens = [UnescapedNextTokenStr | !.RevRefTokens]
    ;
        PointerChars = [NextPointerChar | PointerCharsPrime],
        ( if NextPointerChar = ('/') then
            NextTokenStr = string.from_rev_char_list(!.NextTokenChars),
            UnescapedNextTokenStr = unescape_string(NextTokenStr),
            !:RevRefTokens = [UnescapedNextTokenStr | !.RevRefTokens],
            next_reference_token(PointerCharsPrime, [], !RevRefTokens)
        else
            !:NextTokenChars = [NextPointerChar | !.NextTokenChars],
            next_reference_token(PointerCharsPrime, !.NextTokenChars, !RevRefTokens)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Pointer -> string conversion.
%

reference_tokens_to_string(Tokens) = String :-
    (
        Tokens = [],
        String = ""
    ;
        Tokens = [_ | _],
        EscapedTokens = list.map(escape_string, Tokens),
        String = "/" ++ string.join_list("/", EscapedTokens)
    ).

%-----------------------------------------------------------------------------%

do_resolve(Pointer, Value, Result) :-
    Pointer = pointer(RefTokens),
    do_resolve_token(RefTokens, Value, Result).

:- pred do_resolve_token(list(string)::in, value::in, value::out) is semidet.

do_resolve_token(RefTokens, Value0, Result) :-
    (
        RefTokens = [],
        Result = Value0
    ;
        RefTokens = [RefToken | RefTokensPrime],
        % XXX POST 14.01 -- Mercury 14.01 does not support state variables in
        % require_complete_switch scope heads -- for compatibility we avoid
        % using state variables for Value here.
        require_complete_switch [Value0] (
            Value0 = array(Elements),
            string.to_int(RefToken, Index),       % semidet.
            list.index0(Elements, Index, Value),  % semidet.
            do_resolve_token(RefTokensPrime, Value, Result)
        ;
            Value0 = object(Members),
            map.search(Members, RefToken, Value), % semidet.
            do_resolve_token(RefTokensPrime, Value, Result)
        ;
            ( Value0 = null
            ; Value0 = bool(_)
            ; Value0 = number(_)
            ; Value0 = string(_)
            ),
            false
        )
    ).

%-----------------------------------------------------------------------------%

:- func unescape_string(string) = string.

unescape_string(!.S) = !:S :-
    string.replace_all(!.S, "~1", "/", !:S),
    string.replace_all(!.S, "~0", "~", !:S).

:- func escape_string(string) = string.

escape_string(!.S) = !:S :-
    string.replace_all(!.S, "~", "~0", !:S),
    string.replace_all(!.S, "/", "~1", !:S).

%-----------------------------------------------------------------------------%
:- end_module json.pointer.
%-----------------------------------------------------------------------------%
