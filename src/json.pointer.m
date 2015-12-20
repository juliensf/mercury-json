%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015, Julien Fischer.
% See the file COPYING for license details.
%
% This sub-module implements JSON pointer operations.
%
%-----------------------------------------------------------------------------%

:- module json.pointer.
:- interface.

:- func do_resolve(pointer, value) = pointer_result.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

do_resolve(Pointer, Value) = Result :-
    string.to_char_list(Pointer, PointerChars),
    (
        PointerChars = [],
        Result = ok(Value)
    ;
        PointerChars = [FirstChar | PointerCharsPrime],
        ( if FirstChar = ('/')
        then next_reference_token(PointerCharsPrime, [], Value, Result)
        else Result = error(invalid_first_char(FirstChar))
        )
    ).

:- pred next_reference_token(list(char)::in, list(char)::in, value::in,
     pointer_result::out) is det.

next_reference_token(PointerChars, NextTokenChars, !.Value, Result) :-
    (
        PointerChars = [],
        resolve_token(NextTokenChars, !.Value, Result)
    ;
        PointerChars = [NextPointerChar | PointerCharsPrime],
        ( if NextPointerChar = ('/') then
            resolve_token(NextTokenChars, !.Value, NextTokenResult),
            (
                NextTokenResult = ok(!:Value),
                next_reference_token(PointerCharsPrime, [], !.Value, Result)
            ;
                NextTokenResult = cannot_resolve_pointer,
                Result = NextTokenResult
            ;
                NextTokenResult = error(_),
                Result = NextTokenResult
            )
        else
            NextTokenCharsPrime = [NextPointerChar | NextTokenChars],
            next_reference_token(PointerCharsPrime, NextTokenCharsPrime, !.Value, Result)
        )
    ).

:- pred resolve_token(list(char)::in, json.value::in, pointer_result::out) is det.

resolve_token(RevTokenChars, !.Value, Result) :-
    (
        !.Value = array(Elements),
        ( if RevTokenChars = ['-'] then
            Result = cannot_resolve_pointer
        else if
            TokenStr = string.from_rev_char_list(RevTokenChars),
            string.to_int(TokenStr, Index)
        then
            ( if list.index0(Elements, Index, !:Value)
            then Result = ok(!.Value)
            else Result = cannot_resolve_pointer
            )
        else
            TokenStr = string.from_rev_char_list(RevTokenChars),
            Result = error(invalid_array_index(TokenStr))
        )
    ;
        !.Value = object(Members),
        TokenStr = string.from_rev_char_list(RevTokenChars),
        UnescapedTokenStr = unescape_string(TokenStr),
        ( if map.search(Members, UnescapedTokenStr, !:Value)
        then Result = ok(!.Value)
        else Result = cannot_resolve_pointer
        )
    ;
        ( !.Value = null
        ; !.Value = bool(_)
        ; !.Value = number(_)
        ; !.Value = string(_)
        ),
        Result = cannot_resolve_pointer
    ).

%-----------------------------------------------------------------------------%

:- func unescape_string(string) = string.

unescape_string(!.S) = !:S :-
    string.replace_all(!.S, "~1", "/", !:S),
    string.replace_all(!.S, "~0", "~", !:S).

%-----------------------------------------------------------------------------%
:- end_module json.pointer.
%-----------------------------------------------------------------------------%
