%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013, Julien Fischer.
% All rights reserved.
%
% Author: Julien Fischer <jfischer@opturion.com>
%
% This module implements marshaling of Mercury values to JSON.
%
%-----------------------------------------------------------------------------%

:- module json.marshal.
:- interface.

%-----------------------------------------------------------------------------%

:- func marshal_from_type(T) = maybe_error(json.value).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module integer.
:- import_module pair.
:- import_module set_ctree234.
:- import_module set_ordlist.
:- import_module set_tree234.
:- import_module set_unordlist.

%-----------------------------------------------------------------------------%

marshal_from_type(Term) = Result :-
    ( if
        dynamic_cast(Term, Int)
    then
        Number = float.float(Int),
        Result = ok(number(Number))
    else if
        dynamic_cast(Term, Float)
    then
        Result = ok(number(Float))
    else if
        dynamic_cast(Term, String)
    then
        Value = string(String),
        Result = ok(Value)
    else if
        dynamic_cast(Term, Char)
    then
        Value = string(string.from_char(Char)),
        Result = ok(Value)
    else if
        dynamic_cast(Term, Bool)
    then
        Value = bool(Bool),
        Result = ok(Value)
    else if
        dynamic_cast(Term, Integer)
    then
        IntegerString : string = integer.to_string(Integer),
        Result = ok(string(IntegerString))
    else if
        dynamic_cast_to_pair(Term, Pair)
    then
        Pair = Fst - Snd,
        FstResult = marshal_from_type(Fst),
        (
            FstResult = ok(FstValue),
            SndResult = marshal_from_type(Snd),
            (
                SndResult = ok(SndValue),
                Object = map.from_assoc_list(
                    ["fst" - FstValue, "snd" - SndValue]),
                Value = object(Object),
                Result = ok(Value)
            ;
                SndResult = error(Msg),
                Result = error(Msg)
            )
        ;
            FstResult = error(Msg),
            Result = error(Msg)
        )
    else if
        dynamic_cast_to_list(Term, List)
    then
        list_to_values(List, [], ValuesResult),
        (
            ValuesResult = ok(RevValues),
            list.reverse(RevValues, Values),
            Result = ok(array(Values))
        ;
            ValuesResult = error(Msg),
            Result = error(Msg)
        )
    else if
        dynamic_cast_to_set_ordlist(Term, Set)
    then
        set_ordlist.to_sorted_list(Set, List),
        Result = marshal_from_type(List)
    else if
        dynamic_cast_to_set_unordlist(Term, Set)
    then
        set_unordlist.to_sorted_list(Set, List),
        Result = marshal_from_type(List)
    else if
        dynamic_cast_to_set_tree234(Term, Set)
    then
        set_tree234.to_sorted_list(Set, List),
        Result = marshal_from_type(List)
    else if
        dynamic_cast_to_set_ctree234(Term, Set)
    then
        List = set_ctree234.to_sorted_list(Set),
        Result = marshal_from_type(List)
    else if
        dynamic_cast_to_maybe(Term, Maybe)
    then
        (
            Maybe = no,
            Result = ok(null)
        ;
            Maybe = yes(Arg),
            Result = marshal_from_type(Arg)
        )
    else if
        dynamic_cast_to_maybe_error(Term, MaybeError)
    then
        (
            MaybeError = ok(OkArg),
            MaybeOkValue = marshal_from_type(OkArg),
            (
                MaybeOkValue = ok(OkValue),
                Object = map.singleton("ok", OkValue),
                Value = object(Object),
                Result = ok(Value)
            ;
                MaybeOkValue = error(Msg),
                Result = error(Msg)
            )
        ;
            MaybeError = error(ErrorArg),
            MaybeErrorValue = marshal_from_type(ErrorArg),
            (
                MaybeErrorValue = ok(ErrorValue),
                Object = map.singleton("error", ErrorValue),
                Result = ok(object(Object))
            ;
                MaybeErrorValue = error(Msg),
                Result = error(Msg)
            )
        )
    else if
        dynamic_cast_to_map(Term, Map)
    then
        map.to_assoc_list(Map, KVs),
        Result = marshal_from_type(KVs)
    else if
        dynamic_cast_to_bimap(Term, Bimap)
    then
        bimap.to_assoc_list(Bimap, KVs),
        Result = marshal_from_type(KVs)
    else if
        TypeDesc = type_of(Term),
        NumFunctors = num_functors(TypeDesc),
        all_functors_have_arity_zero(TypeDesc, 0, NumFunctors)
    then
        functor(Term, do_not_allow, Name, _),
        Value = string(Name),
        Result = ok(Value)
    else if
        deconstruct_du(Term, do_not_allow, FunctorNumLex, Arity, Args),
        TypeDesc = type_of(Term),
        construct.get_functor_with_names(TypeDesc, FunctorNumLex, Name, Arity,
            _ArgTypes, MaybeArgNames)
    then
        ( if Arity = 0 then
            Object = map.singleton(Name, null),
            Value = object(Object),
            Result = ok(Value)
        else
            map.init(Object0),
            add_members(Args, MaybeArgNames, Object0, MaybeObjectResult),
            (
                MaybeObjectResult = ok(Object),
                Value = object(Object),
                Result = ok(Value)
            ;
                MaybeObjectResult = error(Msg),
                Result = error(Msg)
            )
        )
    else
        Result = error("cannot convert type to JSON")
    ).

:- some [T2] pred dynamic_cast_to_list(T1::in, list(T2)::out) is semidet.

dynamic_cast_to_list(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : list(ArgType)).

:- pred list_to_values(list(T)::in, list(value)::in,
    maybe_error(list(value))::out) is det.

list_to_values([], Values, ok(Values)).
list_to_values([T | Ts], !.Values, Result) :-
    ValueResult = marshal_from_type(T),
    (
        ValueResult = ok(Value),
        !:Values = [Value | !.Values],
        list_to_values(Ts, !.Values, Result)
    ;
        ValueResult = error(Msg),
        Result = error(Msg)
    ).

:- some [T2] pred dynamic_cast_to_set_ordlist(T1::in, set_ordlist(T2)::out)
    is semidet.

dynamic_cast_to_set_ordlist(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : set_ordlist(ArgType)).

:- some [T2] pred dynamic_cast_to_set_unordlist(T1::in, set_unordlist(T2)::out) is semidet.

dynamic_cast_to_set_unordlist(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : set_unordlist(ArgType)).

:- some [T2] pred dynamic_cast_to_set_tree234(T1::in, set_tree234(T2)::out)
    is semidet.

dynamic_cast_to_set_tree234(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : set_tree234(ArgType)).

:- some [T2] pred dynamic_cast_to_set_ctree234(T1::in, set_ctree234(T2)::out) is semidet.

dynamic_cast_to_set_ctree234(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : set_ctree234(ArgType)).

:- some [T2] pred dynamic_cast_to_maybe(T1::in, maybe(T2)::out) is semidet.

dynamic_cast_to_maybe(X, M) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, M : maybe(ArgType)).

:- pred add_members(list(univ)::in, list(maybe(string))::in,
    json.object::in, maybe_error(json.object)::out) is det.

add_members([], [], Object, ok(Object)).
add_members([], [_ | _], _, _) :-
    unexpected($file, $pred, "mismatched argument lengths (1)").
add_members([_ | _], [], _, _) :-
    unexpected($file, $pred, "mismatched argument lengths (2)").
add_members([Univ | Univs], [MaybeArgName | MaybeArgNames], !.Object,
        Result) :-
    (
        MaybeArgName = no,
        % XXX what type, which field no?
        Result = error("missing field name")
    ;
        MaybeArgName = yes(FieldName),
        Arg = univ_value(Univ),
        ValueResult = marshal_from_type(Arg),
        (
            ValueResult = ok(Value),
            map.det_insert(FieldName, Value, !Object),
            add_members(Univs, MaybeArgNames, !.Object, Result)
        ;
            ValueResult = error(Msg),
            Result = error(Msg)
        )
    ).

:- some [T2, T3] pred dynamic_cast_to_pair(T1::in, pair(T2, T3)::out)
    is semidet.

dynamic_cast_to_pair(X, Pair) :-
    [FstTypeDesc, SndTypeDesc] = type_args(type_of(X)),
    (_ : FstType) `has_type` FstTypeDesc,
    (_ : SndType) `has_type` SndTypeDesc,
    dynamic_cast(X, Pair : pair(FstType, SndType)).

:- some [T2, T3] pred dynamic_cast_to_maybe_error(T1::in,
    maybe_error(T2, T3)::out) is semidet.

dynamic_cast_to_maybe_error(X, M) :-
    [OkTypeDesc, ErrorTypeDesc] = type_args(type_of(X)),
    (_ : OkType) `has_type` OkTypeDesc,
    (_ : ErrorType) `has_type` ErrorTypeDesc,
    dynamic_cast(X, M : maybe_error(OkType, ErrorType)).

:- some [T2, T3] pred dynamic_cast_to_map(T1::in, map(T2, T3)::out)
    is semidet.

dynamic_cast_to_map(X, M) :-
    [KeyTypeDesc, ValueTypeDesc] = type_args(type_of(X)),
    (_ : KeyType) `has_type` KeyTypeDesc,
    (_ : ValueType) `has_type` ValueTypeDesc,
    dynamic_cast(X, M : map(KeyType, ValueType)).

:- some [T2, T3] pred dynamic_cast_to_bimap(T1::in, bimap(T2, T3)::out)
    is semidet.

dynamic_cast_to_bimap(X, M) :-
    [KeyTypeDesc, ValueTypeDesc] = type_args(type_of(X)),
    (_ : KeyType) `has_type` KeyTypeDesc,
    (_ : ValueType) `has_type` ValueTypeDesc,
    dynamic_cast(X, M : bimap(KeyType, ValueType)).

%-----------------------------------------------------------------------------%
:- end_module json.marshal.
%-----------------------------------------------------------------------------%
