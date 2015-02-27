%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Julien Fischer.
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

:- import_module array.
:- import_module bimap.
:- import_module bitmap.
:- import_module calendar.
:- import_module cord.
:- import_module integer.
:- import_module pair.
:- import_module set_bbbtree.
:- import_module set_ctree234.
:- import_module set_ordlist.
:- import_module set_tree234.
:- import_module set_unordlist.
:- import_module version_array.

%-----------------------------------------------------------------------------%

marshal_from_type(Term) = Result :-
    ( if
        dynamic_cast(Term, Int)
    then
        Result = int_to_json(Int)
    else if
        dynamic_cast(Term, Float)
    then
        Result = float_to_json(Float)
    else if
        dynamic_cast(Term, String)
    then
        Result = string_to_json(String)
    else if
        dynamic_cast(Term, Char)
    then
        Result = char_to_json(Char)
    else if
        dynamic_cast(Term, Bool)
    then
        Result = bool_to_json(Bool)
    else if
        dynamic_cast(Term, Integer)
    then
        Result = integer_to_json(Integer)
    else if
        dynamic_cast(Term, DateTime)
    then
        Result = date_time_to_json(DateTime)
    else if
        dynamic_cast(Term, Duration)
    then
        Result = duration_to_json(Duration)
    else if
        dynamic_cast_to_pair(Term, Pair)
    then
        Result = pair_to_json(Pair)
    else if
        dynamic_cast_to_list(Term, List)
    then
        Result = list_to_json(List)
    else if
        dynamic_cast_to_cord(Term, Cord)
    then
        Result = cord_to_json(Cord)
    else if
        % NOTE: dynamic_cast_to_array/2 is *not* in the array module's
        % documented interface.
        array.dynamic_cast_to_array(Term, Array)
    then
        Result = array_to_json(Array)
    else if
        dynamic_cast_to_version_array(Term, VersionArray)
    then
        Result = version_array_to_json(VersionArray)
    else if
        dynamic_cast(Term, Bitmap)
    then
        Result = bitmap_to_json(Bitmap)
    else if
        dynamic_cast_to_set_ordlist(Term, Set)
    then
        Result = set_ordlist_to_json(Set)
    else if
        dynamic_cast_to_set_unordlist(Term, Set)
    then
        Result = set_unordlist_to_json(Set)
    else if
        dynamic_cast_to_set_tree234(Term, Set)
    then
        Result = set_tree234_to_json(Set)
    else if
        dynamic_cast_to_set_ctree234(Term, Set)
    then
        Result = set_ctree234_to_json(Set)
    else if
        dynamic_cast_to_set_bbbtree(Term, Set)
    then
        Result = set_bbbtree_to_json(Set)
    else if
        dynamic_cast_to_maybe(Term, Maybe)
    then
        Result = maybe_to_json(Maybe)
    else if
        dynamic_cast_to_map(Term, Map)
    then
        Result = map_to_json(Map)
    else if
        dynamic_cast_to_bimap(Term, Bimap)
    then
        Result = bimap_to_json(Bimap)
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
            _ArgTypes, _MaybeArgNames)
    then
        Object0 = map.singleton("functor", string(Name)),
        ( if Arity = 0 then
            % Omit the 'args' field if the functor has no arguments.
            Object = Object0,
            Value = object(Object),
            Result = ok(Value)
        else
            gather_du_args(Args, [], MaybeArgsResult),
            (
                MaybeArgsResult = ok(RevJSONArgs),
                list.reverse(RevJSONArgs, JSONArgs),
                map.det_insert("args", array(JSONArgs), Object0, Object),
                Value = object(Object),
                Result = ok(Value)
            ;
                MaybeArgsResult = error(Msg),
                Result = error(Msg)
            )
        )
    else
        TypeDesc = type_of(Term),
        type_ctor_and_args(TypeDesc, TypeCtorDesc, _),
        type_ctor_name_and_arity(TypeCtorDesc, ModuleName, TypeName,
            TypeArity),
        string.format("cannot convert type '%s.%s'/%d to JSON",
            [s(ModuleName), s(TypeName), i(TypeArity)], Msg),
        Result = error(Msg)
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

:- pred array_to_values(array(T)::in, maybe_error(list(value))::out) is det.

array_to_values(Array, Result) :-
    do_array_to_values(Array, array.min(Array), array.max(Array), [], Result).

:- pred do_array_to_values(array(T)::in, int::in, int::in,
    list(value)::in, maybe_error(list(value))::out) is det.

do_array_to_values(Array, Min, I, !.Values, Result) :-
    ( if I < Min then
        Result = ok(!.Values)
    else
        Elem = Array ^ unsafe_elem(I),
        ValueResult = marshal_from_type(Elem),
        (
            ValueResult = ok(Value),
            !:Values = [Value | !.Values],
            do_array_to_values(Array, Min, I - 1, !.Values, Result)
        ;
            ValueResult = error(Msg),
            Result = error(Msg)
        )
    ).

:- pred version_array_to_values(version_array(T)::in,
    maybe_error(list(value))::out) is det.

version_array_to_values(Array, Result) :-
    do_version_array_to_values(Array, 0, version_array.max(Array), [], Result).

:- pred do_version_array_to_values(version_array(T)::in, int::in, int::in,
    list(value)::in, maybe_error(list(value))::out) is det.

do_version_array_to_values(Array, Min, I, !.Values, Result) :-
    ( if I < Min then
        Result = ok(!.Values)
    else
        Elem = version_array.lookup(Array, I),
        ValueResult = marshal_from_type(Elem),
        (
            ValueResult = ok(Value),
            !:Values = [Value | !.Values],
            do_version_array_to_values(Array, Min, I - 1, !.Values, Result)
        ;
            ValueResult = error(Msg),
            Result = error(Msg)
        )
    ).

:- some [T2] pred dynamic_cast_to_cord(T1::in, cord(T2)::out) is semidet.

dynamic_cast_to_cord(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : cord(ArgType)).

:- some [T2] pred dynamic_cast_to_version_array(T1::in, version_array(T2)::out) is semidet.

dynamic_cast_to_version_array(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : version_array(ArgType)).

:- some [T2] pred dynamic_cast_to_set_ordlist(T1::in, set_ordlist(T2)::out)
    is semidet.

dynamic_cast_to_set_ordlist(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : set_ordlist(ArgType)).

:- some [T2] pred dynamic_cast_to_set_unordlist(T1::in, set_unordlist(T2)::out)
    is semidet.

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

:- some [T2] pred dynamic_cast_to_set_ctree234(T1::in, set_ctree234(T2)::out)
    is semidet.

dynamic_cast_to_set_ctree234(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : set_ctree234(ArgType)).

:- some [T2] pred dynamic_cast_to_set_bbbtree(T1::in, set_bbbtree(T2)::out)
    is semidet.

dynamic_cast_to_set_bbbtree(X, L) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L : set_bbbtree(ArgType)).

:- some [T2] pred dynamic_cast_to_maybe(T1::in, maybe(T2)::out) is semidet.

dynamic_cast_to_maybe(X, M) :-
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ : ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, M : maybe(ArgType)).

:- pred gather_du_args(list(univ)::in,
    list(json.value)::in, maybe_error(list(json.value))::out) is det.

gather_du_args([], JArgs, ok(JArgs)).
gather_du_args([Univ | Univs], !.JArgs, Result) :-
    Arg = univ_value(Univ),
    ValueResult = marshal_from_type(Arg),
    (
        ValueResult = ok(Value),
        !:JArgs = [Value | !.JArgs],
        gather_du_args(Univs, !.JArgs, Result)
    ;
        ValueResult = error(Msg),
        Result = error(Msg)
    ).

:- some [T2, T3] pred dynamic_cast_to_pair(T1::in, pair(T2, T3)::out)
    is semidet.

dynamic_cast_to_pair(X, Pair) :-
    [FstTypeDesc, SndTypeDesc] = type_args(type_of(X)),
    (_ : FstType) `has_type` FstTypeDesc,
    (_ : SndType) `has_type` SndTypeDesc,
    dynamic_cast(X, Pair : pair(FstType, SndType)).

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

:- func int_to_json(int) = maybe_error(json.value).
:- func float_to_json(float) = maybe_error(json.value).
:- func string_to_json(string) = maybe_error(json.value).
:- func char_to_json(char) = maybe_error(json.value).
:- func bool_to_json(bool) = maybe_error(json.value).
:- func integer_to_json(integer) = maybe_error(json.value).
:- func date_time_to_json(date_time) = maybe_error(json.value).
:- func duration_to_json(duration) = maybe_error(json.value).
:- func pair_to_json(pair(A, B)) = maybe_error(json.value).
:- func list_to_json(list(T)) = maybe_error(json.value).
:- func cord_to_json(cord(T)) = maybe_error(json.value).
:- func array_to_json(array(T)) = maybe_error(json.value).
:- func version_array_to_json(version_array(T)) = maybe_error(json.value).
:- func bitmap_to_json(bitmap) = maybe_error(json.value).
:- func set_ordlist_to_json(set_ordlist(T)) = maybe_error(json.value).
:- func set_unordlist_to_json(set_unordlist(T)) = maybe_error(json.value).
:- func set_tree234_to_json(set_tree234(T)) = maybe_error(json.value).
:- func set_ctree234_to_json(set_ctree234(T)) = maybe_error(json.value).
:- func set_bbbtree_to_json(set_bbbtree(T)) = maybe_error(json.value).
:- func maybe_to_json(maybe(T)) = maybe_error(json.value).
:- func map_to_json(map(K, V)) = maybe_error(json.value).
:- func bimap_to_json(bimap(K, V)) = maybe_error(json.value).

int_to_json(Int) = ok(number(float(Int))).

float_to_json(Float) =
    ( if is_nan_or_inf(Float)
    then error("cannot convert non-finite float to JSON")
    else ok(number(Float))
    ).

string_to_json(String) = ok(string(String)).

char_to_json(Char) = ok(string(string.from_char(Char))).

bool_to_json(Bool) = ok(bool(Bool)).

integer_to_json(Integer) = Result :-
    IntegerString : string = integer.to_string(Integer),
    Result = ok(string(IntegerString)).

date_time_to_json(DateTime) = ok(string(date_to_string(DateTime))).

duration_to_json(Duration) = ok(string(duration_to_string(Duration))).

pair_to_json(Pair) = Result :-
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
    ).

list_to_json(List) = Result :-
    list_to_values(List, [], ValuesResult),
    (
        ValuesResult = ok(RevValues),
        list.reverse(RevValues, Values),
        Result = ok(array(Values))
    ;
        ValuesResult = error(Msg),
        Result = error(Msg)
    ).

cord_to_json(Cord) = Result :-
    List = cord.list(Cord),
    Result = list_to_json(List).

array_to_json(Array) = Result :-
    array_to_values(Array, ValuesResult),
    (
        ValuesResult = ok(Values),
        Result = ok(array(Values))
    ;
        ValuesResult = error(Msg),
        Result = error(Msg)
    ).

version_array_to_json(VersionArray) = Result :-
    version_array_to_values(VersionArray, ValuesResult),
    (
        ValuesResult = ok(Values),
        Result = ok(array(Values))
    ;
        ValuesResult = error(Msg),
        Result = error(Msg)
    ).

bitmap_to_json(Bitmap) = Result :-
    String = bitmap.to_string(Bitmap),
    Result = ok(string(String)).

set_ordlist_to_json(Set) = Result :-
    set_ordlist.to_sorted_list(Set, List),
    Result = list_to_json(List).

set_unordlist_to_json(Set) = Result :-
    set_unordlist.to_sorted_list(Set, List),
    Result = list_to_json(List).

set_tree234_to_json(Set) = Result :-
    set_tree234.to_sorted_list(Set, List),
    Result = list_to_json(List).

set_ctree234_to_json(Set) = Result :-
    List = set_ctree234.to_sorted_list(Set),
    Result = list_to_json(List).

set_bbbtree_to_json(Set) = Result :-
    set_bbbtree.to_sorted_list(Set, List),
    Result = list_to_json(List).

maybe_to_json(Maybe) = Result :-
    (
        Maybe = no,
        Result = ok(null)
    ;
        Maybe = yes(Arg),
        Result = marshal_from_type(Arg)
    ).

map_to_json(Map) = Result :-
    map.to_assoc_list(Map, KVs),
    Result = marshal_from_type(KVs).

bimap_to_json(Bimap) = Result :-
    bimap.to_assoc_list(Bimap, KVs),
    Result = marshal_from_type(KVs).

%-----------------------------------------------------------------------------%
:- end_module json.marshal.
%-----------------------------------------------------------------------------%
