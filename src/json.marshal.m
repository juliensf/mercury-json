%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013, Julien Fischer.
% All rights reserved.
%
% Author: Julien Fischer <jfischer@www.opturion.com>
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

marshal_from_type(Term) = Result :-
    ( if
        dynamic_cast(Term, Int)
    then
        Value = number(float(Int)),
        Result = ok(Value)
    else if
        dynamic_cast(Term, Float)
    then
        Value = number(Float),
        Result = ok(Value)
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
        dynamic_cast_to_list(Term, List)
    then
        list_to_values(List, [], ValuesResult),
        (
            ValuesResult = ok(RevValues),
            list.reverse(RevValues, Values),
            Value = array(Values),
            Result = ok(Value)
        ;
            ValuesResult = error(Msg),
            Result = error(Msg)
        )
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
        construct.get_functor_with_names(TypeDesc, FunctorNumLex, Name,  Arity,
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

%-----------------------------------------------------------------------------%
:- end_module json.marshal.
%-----------------------------------------------------------------------------%
