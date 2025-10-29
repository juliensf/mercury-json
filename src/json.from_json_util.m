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

%----------------------------------------------------------------------------%

    % map_value_to_type(Pointer, Value, ToType) = Result:
    %
    % Unmarshal Value to a term of type V and then apply the function
    % ToType to that to yield a term of type T.
    %
:- func map_value_to_type(json.pointer, json.value,
    func(V) = T) = from_json_result(T) <= from_json(V).

%----------------------------------------------------------------------------%

:- func int_value_to_type(json.pointer::in, json.value::in,
    pred(int, T)::in(pred(in, out) is semidet))
    = (from_json_result(T)::out) is det.

:- func int_to_type(json.pointer::in, int::in,
    pred(int, T)::in(pred(in, out) is semidet))
    = (from_json_result(T)::out) is det.

%----------------------------------------------------------------------------%

:- func string_value_to_type(json.pointer::in, json.value::in,
    pred(string, T)::in(pred(in, out) is semidet))
    = (from_json_result(T)::out) is det.

:- func string_to_type(json.pointer::in, string::in,
    pred(string, T)::in(pred(in, out) is semidet))
    = (from_json_result(T)::out) is det.

%----------------------------------------------------------------------------%

:- func object_value_to_type(json.pointer, json.value, string, func(M) = T)
    = from_json_result(T) <= from_json(M).

:- func object_value_to_type2(json.pointer, json.value, string, string,
    func(M1, M2) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2)).

:- func object_value_to_type3(json.pointer, json.value, string, string,
    string, func(M1, M2, M3) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2), from_json(M3)).

:- func object_value_to_type4(json.pointer, json.value,
    string, string, string, string,
    func(M1, M2, M3, M4) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4)).

:- func object_value_to_type5(json.pointer, json.value,
    string, string, string, string, string,
    func(M1, M2, M3, M4, M5) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5)).

:- func object_value_to_type6(json.pointer, json.value,
    string, string, string, string, string,
    string, func(M1, M2, M3, M4, M5, M6) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5), from_json(M6)).

%----------------------------------------------------------------------------%

:- func object_to_type(json.pointer, json.object, string,
    func(M1) = T) = from_json_result(T) <= from_json(M1).

:- func object_to_type2(json.pointer, json.object, string, string,
    func(M1, M2) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2)).

:- func object_to_type3(json.pointer, json.object, string, string, string,
    func(M1, M2, M3) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2), from_json(M3)).

:- func object_to_type4(json.pointer, json.object,
    string, string, string, string,
    func(M1, M2, M3, M4) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4)).

:- func object_to_type5(json.pointer, json.object,
    string, string, string, string, string,
    func(M1, M2, M3, M4, M5) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5)).

:- func object_to_type6(json.pointer, json.object,
    string, string, string, string, string, string,
    func(M1, M2, M3, M4, M5, M6) = T) = from_json_result(T)
    <= (from_json(M1), from_json(M2), from_json(M3), from_json(M4),
        from_json(M5), from_json(M6)).

%----------------------------------------------------------------------------%

:- func make_value_type_mismatch_error(pointer, string, json.value) =
    from_json_result(T).

:- func make_from_string_failed_error(pointer, string) = from_json_result(T).

:- func make_missing_member_error(pointer, string) = from_json_result(T).

:- func make_out_of_bounds_number_error(pointer) = from_json_result(T).

:- func make_non_finite_number_error(pointer) = from_json_result(T).

:- func make_other_error(pointer, string) = from_json_result(T).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.
:- import_module type_desc.

%----------------------------------------------------------------------------%

map_value_to_type(Pointer, JValue, ToType) = Result :-
    ValueResult = from_json(Pointer, JValue),
    (
        ValueResult = ok(Value),
        Type = ToType(Value),
        Result = ok(Type)
    ;
        ValueResult = error(Error),
        Result = error(Error)
    ).

%----------------------------------------------------------------------------%

int_value_to_type(Pointer, JValue, ConvPred) = Result :-
    IntResult = from_json(Pointer, JValue),
    (
        IntResult = ok(Int),
        Result = int_to_type(Pointer, Int, ConvPred)
    ;
        IntResult = error(Error),
        Result = error(Error)
    ).

int_to_type(Pointer, Int, ConvPred) = Result :-
    ( if ConvPred(Int, Type) then
        Result = ok(Type)
    else
        string.format("invalid int: %d", [i(Int)], Msg),
        Result = make_other_error(Pointer, Msg)
    ).

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
        Result = make_value_type_mismatch_error(Pointer, "string", JValue)
    ).

string_to_type(Pointer, String, ConvPred) = Result :-
    ( if ConvPred(String, Type) then
        Result = ok(Type)
    else
        string.format("unrecognised string: \"%s\"", [s(String)], Msg),
        Result = make_other_error(Pointer, Msg)
    ).

%----------------------------------------------------------------------------%

object_value_to_type(Pointer, JValue, MemberName, Combine) = Result :-
    ( if JValue = object(Object) then
        Result = object_to_type(Pointer, Object, MemberName, Combine)
    else
        Result = make_value_type_mismatch_error(Pointer, "object", JValue)
    ).

object_value_to_type2(Pointer, JValue, MemberName1, MemberName2, Combine)
        = Result :-
    ( if JValue = object(Object) then
        Result = object_to_type2(Pointer, Object, MemberName1, MemberName2,
            Combine)
    else
        Result = make_value_type_mismatch_error(Pointer, "object", JValue)
    ).

object_value_to_type3(Pointer, JValue, MemberName1, MemberName2, MemberName3,
        Combine) = Result :-
    ( if JValue = object(Object) then
        Result = object_to_type3(Pointer, Object, MemberName1, MemberName2,
            MemberName3, Combine)
    else
        Result = make_value_type_mismatch_error(Pointer, "object", JValue)
    ).

object_value_to_type4(Pointer, JValue, MemberName1, MemberName2, MemberName3,
        MemberName4, Combine) = Result :-
    ( if JValue = object(Object) then
        Result = object_to_type4(Pointer, Object, MemberName1, MemberName2,
            MemberName3, MemberName4, Combine)
    else
        Result = make_value_type_mismatch_error(Pointer, "object", JValue)
    ).

object_value_to_type5(Pointer, JValue, MemberName1, MemberName2, MemberName3,
        MemberName4, MemberName5, Combine) = Result :-
    ( if JValue = object(Object) then
        Result = object_to_type5(Pointer, Object, MemberName1, MemberName2,
            MemberName3, MemberName4, MemberName5, Combine)
    else
        Result = make_value_type_mismatch_error(Pointer, "object", JValue)
    ).

object_value_to_type6(Pointer, JValue, MemberName1, MemberName2, MemberName3,
        MemberName4, MemberName5, MemberName6, Combine) = Result :-
    ( if JValue = object(Object) then
        Result = object_to_type6(Pointer, Object, MemberName1, MemberName2,
            MemberName3, MemberName4, MemberName5, MemberName6, Combine)
    else
        Result = make_value_type_mismatch_error(Pointer, "object", JValue)
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
        Result = make_missing_member_error(Pointer, MemberName)
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
            Result = make_missing_member_error(Pointer, MemberName2)
        )
    else
        Result = make_missing_member_error(Pointer, MemberName1)
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
                Result = make_missing_member_error(Pointer, MemberName3)
            )
        else
            Result = make_missing_member_error(Pointer, MemberName2)
        )
    else
        Result = make_missing_member_error(Pointer, MemberName1)
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
                    Result = make_missing_member_error(Pointer, MemberName4)
                )
            else
                Result = make_missing_member_error(Pointer, MemberName3)
            )
        else
            Result = make_missing_member_error(Pointer, MemberName2)
        )
    else
        Result = make_missing_member_error(Pointer, MemberName1)
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
                        Result = make_missing_member_error(Pointer, MemberName5)
                    )
                else
                    Result = make_missing_member_error(Pointer, MemberName4)
                )
            else
                Result = make_missing_member_error(Pointer, MemberName3)
            )
        else
            Result = make_missing_member_error(Pointer, MemberName2)
        )
    else
        Result = make_missing_member_error(Pointer, MemberName1)
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
                            Result = make_missing_member_error(Pointer,
                                MemberName6)
                        )
                    else
                        Result = make_missing_member_error(Pointer, MemberName5)
                    )
                else
                    Result = make_missing_member_error(Pointer, MemberName4)
                )
            else
                Result = make_missing_member_error(Pointer, MemberName3)
            )
        else
            Result = make_missing_member_error(Pointer, MemberName2)
        )
    else
        Result = make_missing_member_error(Pointer, MemberName1)
    ).

%----------------------------------------------------------------------------%

:- func member_to_type(json.pointer,
    pair(string, json.value),
    func(M) = T) = from_json_result(T) <= from_json(M).

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
    func(M1, M2) = T) = from_json_result(T) <= (from_json(M1), from_json(M2)).

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
    func(M1, M2, M3) = T) = from_json_result(T)
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
    func(M1, M2, M3, M4) = T) = from_json_result(T)
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
    func(M1, M2, M3, M4, M5) = T) = from_json_result(T)
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
    func(M1, M2, M3, M4, M5, M6) = T) = from_json_result(T)
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

make_value_type_mismatch_error(Pointer, ExpectedDesc, Value) = Result :-
    TypeDesc = type_desc_from_result(Result),
    HaveDesc = to_value_desc(Value),
    ErrorDesc = value_type_mismatch(ExpectedDesc, HaveDesc),
    Error = from_json_error(Pointer, TypeDesc, ErrorDesc),
    Result = error(Error).

make_from_string_failed_error(Pointer, String) = Result :-
    TypeDesc = type_desc_from_result(Result),
    ErrorDesc = from_string_failed(String),
    Error = from_json_error(Pointer, TypeDesc, ErrorDesc),
    Result = error(Error).

make_missing_member_error(Pointer, MemberName) = Result :-
    TypeDesc = type_desc_from_result(Result),
    ErrorDesc = missing_member(MemberName),
    Error = from_json_error(Pointer, TypeDesc, ErrorDesc),
    Result = error(Error).

make_out_of_bounds_number_error(Pointer) = Result :-
    TypeDesc = type_desc_from_result(Result),
    ErrorDesc = out_of_bounds_number,
    Error = from_json_error(Pointer, TypeDesc, ErrorDesc),
    Result = error(Error).

make_non_finite_number_error(Pointer) = Result :-
    TypeDesc = type_desc_from_result(Result),
    ErrorDesc = non_finite_number,
    Error = from_json_error(Pointer, TypeDesc, ErrorDesc),
    Result = error(Error).

make_other_error(Pointer, Msg) = Result :-
    TypeDesc = type_desc_from_result(Result),
    ErrorDesc = other(Msg),
    Error = from_json_error(Pointer, TypeDesc, ErrorDesc),
    Result = error(Error).

:- func type_desc_from_result(from_json_result(T)::unused) = (type_desc::out).

type_desc_from_result(Result) = TypeDesc :-
    ResultTypeDesc = type_of(Result),
    type_ctor_and_args(ResultTypeDesc, _, Args),
    (
        Args = [],
        unexpected($file, $pred, "no argument type_descs")
    ;
        Args = [_],
        unexpected($file, $pred, "one argument type_desc")
    ;
        Args = [TypeDesc, _ErrorTypeDesc]
    ;
        Args = [_, _, _| _],
        unexpected($file, $pred, "> 2 argument type_descs")
    ).

%----------------------------------------------------------------------------%
:- end_module json.from_json_util.
%----------------------------------------------------------------------------%
