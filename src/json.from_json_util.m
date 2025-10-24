%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2025 Julien Fischer.
% See the file COPYING for license details.
%----------------------------------------------------------------------------%
%
% This submodule contains utility functions that convert JSON values into
% values of Mercury types. These functions are intended for use by type class
% instances of the from_json/1 type class.
%
%----------------------------------------------------------------------------%

:- module json.from_json_util.
:- interface.

:- import_module maybe.

%----------------------------------------------------------------------------%

:- func string_value_to_type(json.pointer::in, json.value::in,
    pred(string, T)::in(pred(in, out) is semidet))
    = (maybe_error(T)::out) is det.

:- func string_to_type(json.pointer::in, string::in,
    pred(string, T)::in(pred(in, out) is semidet))
    = (maybe_error(T)::out) is det.

%----------------------------------------------------------------------------%

:- func object_value_to_type(json.pointer, json.value, string, func(M) = T)
    = maybe_error(T) <= from_json(M).

:- func object_value_to_type2(json.pointer, json.value, string, string,
    func(M1, M2) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2)).

:- func object_value_to_type3(json.pointer, json.value, string, string,
    string, func(M1, M2, M3) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3)).

:- func object_value_to_type4(json.pointer, json.value,
    string, string, string, string,
    func(M1, M2, M3, M4) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4)).

:- func object_value_to_type5(json.pointer, json.value,
    string, string, string, string, string,
    func(M1, M2, M3, M4, M5) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5)).

:- func object_value_to_type6(json.pointer, json.value,
    string, string, string, string, string,
    string, func(M1, M2, M3, M4, M5, M6) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5), from_json(M6)).

%----------------------------------------------------------------------------%

:- func object_to_type(json.pointer, json.object, string,
    func(M1) = T) = maybe_error(T) <= from_json(M1).

:- func object_to_type2(json.pointer, json.object, string, string,
    func(M1, M2) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2)).

:- func object_to_type3(json.pointer, json.object, string, string, string,
    func(M1, M2, M3) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3)).

:- func object_to_type4(json.pointer, json.object,
    string, string, string, string,
    func(M1, M2, M3, M4) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4)).

:- func object_to_type5(json.pointer, json.object,
    string, string, string, string, string,
    func(M1, M2, M3, M4, M5) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5)).

:- func object_to_type6(json.pointer, json.object,
    string, string, string, string, string, string,
    func(M1, M2, M3, M4, M5, M6) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5), from_json(M6)).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.

%----------------------------------------------------------------------------%

string_value_to_type(Pointer, JValue, ConvPred) = Result :-
    (
        JValue = string(String),
        Result = string_to_type(Pointer, String, ConvPred)
    ;
        ( JValue = null
        ; JValue = bool(_)
        ; JValue = number(_)
        ; JValue = array(_)
        ; JValue = object(_)
        ),
        Result = error("expected a string")
    ).

string_to_type(Pointer, String, ConvPred) = Result :-
    ( if ConvPred(String, Term) then
        Result = ok(Term)
    else
        string.format("at %s: unrecognised string: \"%s\".",
            [s(pointer_to_string(Pointer)), s(String)], Msg),
        Result = error(Msg)
    ).

%----------------------------------------------------------------------------%

object_value_to_type(Pointer, JValue, MemberName, Combine) = Result :-
    (
        JValue = object(Object),
        Result = object_to_type(Pointer, Object, MemberName, Combine)
    ;
        ( JValue = null
        ; JValue = bool(_)
        ; JValue = string(_)
        ; JValue = number(_)
        ; JValue = array(_)
        ),
        Result = error("expected an object")
    ).

object_value_to_type2(Pointer, JValue, MemberName1, MemberName2, Combine)
        = Result :-
    (
        JValue = object(Object),
        Result = object_to_type2(Pointer, Object, MemberName1, MemberName2,
            Combine)
    ;
        ( JValue = null
        ; JValue = bool(_)
        ; JValue = string(_)
        ; JValue = number(_)
        ; JValue = array(_)
        ),
        Result = error("expected an object")
    ).

object_value_to_type3(Pointer, JValue, MemberName1, MemberName2, MemberName3,
        Combine) = Result :-
    (
        JValue = object(Object),
        Result = object_to_type3(Pointer, Object, MemberName1, MemberName2,
            MemberName3, Combine)
    ;
        ( JValue = null
        ; JValue = bool(_)
        ; JValue = string(_)
        ; JValue = number(_)
        ; JValue = array(_)
        ),
        % XXX say what we have.
        Result = error("expected an object")
    ).

object_value_to_type4(Pointer, JValue, MemberName1, MemberName2, MemberName3,
        MemberName4, Combine) = Result :-
    (
        JValue = object(Object),
        Result = object_to_type4(Pointer, Object, MemberName1, MemberName2,
            MemberName3, MemberName4, Combine)
    ;
        ( JValue = null
        ; JValue = bool(_)
        ; JValue = string(_)
        ; JValue = number(_)
        ; JValue = array(_)
        ),
        % XXX say what we have.
        Result = error("expected an object")
    ).

object_value_to_type5(Pointer, JValue, MemberName1, MemberName2, MemberName3,
        MemberName4, MemberName5, Combine) = Result :-
    (
        JValue = object(Object),
        Result = object_to_type5(Pointer, Object, MemberName1, MemberName2,
            MemberName3, MemberName4, MemberName5, Combine)
    ;
        ( JValue = null
        ; JValue = bool(_)
        ; JValue = string(_)
        ; JValue = number(_)
        ; JValue = array(_)
        ),
        % XXX say what we have.
        Result = error("expected an object")
    ).

object_value_to_type6(Pointer, JValue, MemberName1, MemberName2, MemberName3,
        MemberName4, MemberName5, MemberName6, Combine) = Result :-
    (
        JValue = object(Object),
        Result = object_to_type6(Pointer, Object, MemberName1, MemberName2,
            MemberName3, MemberName4, MemberName5, MemberName6, Combine)
    ;
        ( JValue = null
        ; JValue = bool(_)
        ; JValue = string(_)
        ; JValue = number(_)
        ; JValue = array(_)
        ),
        % XXX say what we have.
        Result = error("expected an object")
    ).

%----------------------------------------------------------------------------%

object_to_type(Pointer, Object, MemberName, Combine) = Result :-
    ( if
        map.search(Object, MemberName, MemberJValue)
    then
        Result = member_to_type(Pointer,
            MemberName - MemberJValue,
            Combine)
    else
        Result = make_missing_member_error(MemberName)
    ).

object_to_type2(Pointer, Object, MemberName1, MemberName2, Combine) = Result :-
    ( if
        map.search(Object, MemberName1, MemberJValue1)
    then
        ( if
            map.search(Object, MemberName2, MemberJValue2)
        then
            Result = members_to_type2(Pointer,
                MemberName1 - MemberJValue1,
                MemberName2 - MemberJValue2,
                Combine)
        else
            Result = make_missing_member_error(MemberName2)
        )
    else
        Result = make_missing_member_error(MemberName1)
    ).

object_to_type3(Pointer, Object, MemberName1, MemberName2, MemberName3, Combine)
        = Result :-
    ( if
        map.search(Object, MemberName1, MemberJValue1)
    then
        ( if
            map.search(Object, MemberName2, MemberJValue2)
        then
            ( if
                map.search(Object, MemberName3, MemberJValue3)
            then
                Result = members_to_type3(Pointer,
                    MemberName1 - MemberJValue1,
                    MemberName2 - MemberJValue2,
                    MemberName3 - MemberJValue3, Combine)
            else
                Result = make_missing_member_error(MemberName3)
            )
        else
            Result = make_missing_member_error(MemberName2)
        )
    else
        Result = make_missing_member_error(MemberName1)
    ).

object_to_type4(Pointer, Object, MemberName1, MemberName2, MemberName3,
        MemberName4, Combine) = Result :-
    ( if
        map.search(Object, MemberName1, MemberJValue1)
    then
        ( if
            map.search(Object, MemberName2, MemberJValue2)
        then
            ( if
                map.search(Object, MemberName3, MemberJValue3)
            then
                ( if
                    map.search(Object, MemberName4, MemberJValue4)
                then
                    Result = members_to_type4(Pointer,
                        MemberName1 - MemberJValue1,
                        MemberName2 - MemberJValue2,
                        MemberName3 - MemberJValue3,
                        MemberName4 - MemberJValue4,
                        Combine)
                else
                    Result = make_missing_member_error(MemberName4)
                )
            else
                Result = make_missing_member_error(MemberName3)
            )
        else
            Result = make_missing_member_error(MemberName2)
        )
    else
        Result = make_missing_member_error(MemberName1)
    ).


object_to_type5(Pointer, Object, MemberName1, MemberName2, MemberName3,
        MemberName4, MemberName5, Combine) = Result :-
    ( if
        map.search(Object, MemberName1, MemberJValue1)
    then
        ( if
            map.search(Object, MemberName2, MemberJValue2)
        then
            ( if
                map.search(Object, MemberName3, MemberJValue3)
            then
                ( if
                    map.search(Object, MemberName4, MemberJValue4)
                then
                    ( if
                        map.search(Object, MemberName5, MemberJValue5)
                    then
                        Result = members_to_type5(Pointer,
                            MemberName1 - MemberJValue1,
                            MemberName2 - MemberJValue2,
                            MemberName3 - MemberJValue3,
                            MemberName4 - MemberJValue4,
                            MemberName5 - MemberJValue5,
                            Combine)
                    else
                        Result = make_missing_member_error(MemberName5)
                    )
                else
                    Result = make_missing_member_error(MemberName4)
                )
            else
                Result = make_missing_member_error(MemberName3)
            )
        else
            Result = make_missing_member_error(MemberName2)
        )
    else
        Result = make_missing_member_error(MemberName1)
    ).

object_to_type6(Pointer, Object, MemberName1, MemberName2, MemberName3,
        MemberName4, MemberName5, MemberName6, Combine) = Result :-
    ( if
        map.search(Object, MemberName1, MemberJValue1)
    then
        ( if
            map.search(Object, MemberName2, MemberJValue2)
        then
            ( if
                map.search(Object, MemberName3, MemberJValue3)
            then
                ( if
                    map.search(Object, MemberName4, MemberJValue4)
                then
                    ( if
                        map.search(Object, MemberName5, MemberJValue5)
                    then
                        ( if
                            map.search(Object, MemberName6, MemberJValue6)
                        then
                            Result = members_to_type6(Pointer,
                                MemberName1 - MemberJValue1,
                                MemberName2 - MemberJValue2,
                                MemberName3 - MemberJValue3,
                                MemberName4 - MemberJValue4,
                                MemberName5 - MemberJValue5,
                                MemberName6 - MemberJValue6,
                                Combine)
                        else
                            Result = make_missing_member_error(MemberName6)
                        )
                    else
                        Result = make_missing_member_error(MemberName5)
                    )
                else
                    Result = make_missing_member_error(MemberName4)
                )
            else
                Result = make_missing_member_error(MemberName3)
            )
        else
            Result = make_missing_member_error(MemberName2)
        )
    else
        Result = make_missing_member_error(MemberName1)
    ).

:- func make_missing_member_error(string) = maybe_error(T).

make_missing_member_error(MemberName) = Result :-
    string.format("object has no member named '%s'", [s(MemberName)], Msg),
    Result = error(Msg).

%----------------------------------------------------------------------------%

:- func member_to_type(json.pointer,
    pair(string, json.value),
    func(M) = T) = maybe_error(T) <= from_json(M).

member_to_type(Pointer, MemberName - JValue, CombineFunc) = Result :-
    MemberPointer = append_token(Pointer, MemberName),
    MemberResult = from_json(MemberPointer, JValue),
    (
        MemberResult = ok(Member),
        Value = CombineFunc(Member),
        Result = ok(Value)
    ;
        MemberResult = error(ErrorMsg),
        Result = error(ErrorMsg)
    ).

:- func members_to_type2(json.pointer,
    pair(string, json.value),
    pair(string, json.value),
    func(M1, M2) = T) = maybe_error(T) <= (from_json(M1), from_json(M2)).

members_to_type2(Pointer, MemberName1 - JValue1, MemberName2 - JValue2,
        CombineFunc) = Result :-
    Pointer1 = append_token(Pointer, MemberName1),
    MemberResult1 = from_json(Pointer1, JValue1),
    (
        MemberResult1 = ok(Member1),
        Pointer2 = append_token(Pointer, MemberName2),
        MemberResult2 = from_json(Pointer2, JValue2),
        (
            MemberResult2 = ok(Member2),
            Value = CombineFunc(Member1, Member2),
            Result = ok(Value)
        ;
            MemberResult2 = error(ErrorMsg2),
            Result = error(ErrorMsg2)
        )
    ;
        MemberResult1 = error(ErrorMsg1),
        Result = error(ErrorMsg1)
    ).

:- func members_to_type3(pointer,
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    func(M1, M2, M3) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3)).

members_to_type3(Pointer, MemberName1 - JValue1,
        MemberName2 - JValue2, MemberName3 - JValue3,
        CombineFunc) = Result :-
    Pointer1 = append_token(Pointer, MemberName1),
    MemberResult1 = from_json(Pointer1, JValue1),
    (
        MemberResult1 = ok(Member1),
        Pointer2 = append_token(Pointer, MemberName2),
        MemberResult2 = from_json(Pointer2, JValue2),
        (
            MemberResult2 = ok(Member2),
            Pointer3 = append_token(Pointer, MemberName3),
            MemberResult3 = from_json(Pointer3, JValue3),
            (
                MemberResult3 = ok(Member3),
                Value = CombineFunc(Member1, Member2, Member3),
                Result = ok(Value)
            ;
                MemberResult3 = error(ErrorMsg3),
                Result = error(ErrorMsg3)
            )
        ;
            MemberResult2 = error(ErrorMsg2),
            Result = error(ErrorMsg2)
        )
    ;
        MemberResult1 = error(ErrorMsg1),
        Result = error(ErrorMsg1)
    ).

:- func members_to_type4(json.pointer,
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    func(M1, M2, M3, M4) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4)).

members_to_type4(Pointer,
        MemberName1 - JValue1,
        MemberName2 - JValue2,
        MemberName3 - JValue3,
        MemberName4 - JValue4,
        CombineFunc) = Result :-
    Pointer1 = append_token(Pointer, MemberName1),
    MemberResult1 = from_json(Pointer1, JValue1),
    (
        MemberResult1 = ok(Member1),
        Pointer2 = append_token(Pointer, MemberName2),
        MemberResult2 = from_json(Pointer2, JValue2),
        (
            MemberResult2 = ok(Member2),
            Pointer3 = append_token(Pointer, MemberName3),
            MemberResult3 = from_json(Pointer3, JValue3),
            (
                MemberResult3 = ok(Member3),
                Pointer4 = append_token(Pointer, MemberName4),
                MemberResult4 = from_json(Pointer4, JValue4),
                (
                    MemberResult4 = ok(Member4),
                    Value = CombineFunc(Member1, Member2, Member3, Member4),
                    Result = ok(Value)
                ;
                    MemberResult4 = error(ErrorMsg4),
                    Result = error(ErrorMsg4)
                )
            ;
                MemberResult3 = error(ErrorMsg3),
                Result = error(ErrorMsg3)
            )
        ;
            MemberResult2 = error(ErrorMsg2),
            Result = error(ErrorMsg2)
        )
    ;
        MemberResult1 = error(ErrorMsg1),
        Result = error(ErrorMsg1)
    ).

:- func members_to_type5(json.pointer,
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    func(M1, M2, M3, M4, M5) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5)).

members_to_type5(Pointer,
        MemberName1 - JValue1,
        MemberName2 - JValue2,
        MemberName3 - JValue3,
        MemberName4 - JValue4,
        MemberName5 - JValue5,
        CombineFunc) = Result :-
    Pointer1 = append_token(Pointer, MemberName1),
    MemberResult1 = from_json(Pointer1, JValue1),
    (
        MemberResult1 = ok(Member1),
        Pointer2 = append_token(Pointer, MemberName2),
        MemberResult2 = from_json(Pointer2, JValue2),
        (
            MemberResult2 = ok(Member2),
            Pointer3 = append_token(Pointer, MemberName3),
            MemberResult3 = from_json(Pointer3, JValue3),
            (
                MemberResult3 = ok(Member3),
                Pointer4 = append_token(Pointer, MemberName4),
                MemberResult4 = from_json(Pointer4, JValue4),
                (
                    MemberResult4 = ok(Member4),
                    Pointer5 = append_token(Pointer, MemberName5),
                    MemberResult5 = from_json(Pointer5, JValue5),
                    (
                        MemberResult5 = ok(Member5),
                        Value = CombineFunc(Member1, Member2, Member3, Member4,
                            Member5),
                        Result = ok(Value)
                    ;
                        MemberResult5 = error(ErrorMsg5),
                        Result = error(ErrorMsg5)
                    )
                ;
                    MemberResult4 = error(ErrorMsg4),
                    Result = error(ErrorMsg4)
                )
            ;
                MemberResult3 = error(ErrorMsg3),
                Result = error(ErrorMsg3)
            )
        ;
            MemberResult2 = error(ErrorMsg2),
            Result = error(ErrorMsg2)
        )
    ;
        MemberResult1 = error(ErrorMsg1),
        Result = error(ErrorMsg1)
    ).

:- func members_to_type6(json.pointer,
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    pair(string, json.value),
    func(M1, M2, M3, M4, M5, M6) = T) = maybe_error(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5), from_json(M6)).

members_to_type6(Pointer,
        MemberName1 - JValue1,
        MemberName2 - JValue2,
        MemberName3 - JValue3,
        MemberName4 - JValue4,
        MemberName5 - JValue5,
        MemberName6 - JValue6,
        CombineFunc) = Result :-
    Pointer1 = append_token(Pointer, MemberName1),
    MemberResult1 = from_json(Pointer1, JValue1),
    (
        MemberResult1 = ok(Member1),
        Pointer2 = append_token(Pointer, MemberName2),
        MemberResult2 = from_json(Pointer2, JValue2),
        (
            MemberResult2 = ok(Member2),
            Pointer3 = append_token(Pointer, MemberName3),
            MemberResult3 = from_json(Pointer3, JValue3),
            (
                MemberResult3 = ok(Member3),
                Pointer4 = append_token(Pointer, MemberName4),
                MemberResult4 = from_json(Pointer4, JValue4),
                (
                    MemberResult4 = ok(Member4),
                    Pointer5 = append_token(Pointer, MemberName5),
                    MemberResult5 = from_json(Pointer5, JValue5),
                    (
                        MemberResult5 = ok(Member5),
                        Pointer6 = append_token(Pointer, MemberName6),
                        MemberResult6 = from_json(Pointer6, JValue6),
                        (
                            MemberResult6 = ok(Member6),
                            Value = CombineFunc(Member1, Member2, Member3,
                                Member4, Member5, Member6),
                            Result = ok(Value)
                        ;
                            MemberResult6 = error(ErrorMsg6),
                            Result = error(ErrorMsg6)
                        )
                    ;
                        MemberResult5 = error(ErrorMsg5),
                        Result = error(ErrorMsg5)
                    )
                ;
                    MemberResult4 = error(ErrorMsg4),
                    Result = error(ErrorMsg4)
                )
            ;
                MemberResult3 = error(ErrorMsg3),
                Result = error(ErrorMsg3)
            )
        ;
            MemberResult2 = error(ErrorMsg2),
            Result = error(ErrorMsg2)
        )
    ;
        MemberResult1 = error(ErrorMsg1),
        Result = error(ErrorMsg1)
    ).

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
:- end_module json.from_json_util.
%----------------------------------------------------------------------------%
