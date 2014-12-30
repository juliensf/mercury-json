%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Julien Fischer.
% All rights reserved.
%
% Author: Julien Fischer <juliens@gmail.com>
%
% This module implements unmarshaling of Mercury values from JSON.
%
%-----------------------------------------------------------------------------%

:- module json.unmarshal.
:- interface.

%-----------------------------------------------------------------------------%

:- func unmarshal_to_type(value) = maybe_error(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module assoc_list.
:- import_module bimap.
:- import_module bitmap.
:- import_module calendar.
:- import_module construct.
:- import_module cord.
:- import_module integer.
:- import_module pair.
:- import_module set_bbbtree.
:- import_module set_ctree234.
:- import_module set_ordlist.
:- import_module set_tree234.
:- import_module set_unordlist.
:- import_module type_desc.
:- import_module version_array.

%-----------------------------------------------------------------------------%

unmarshal_to_type(Value) = Result :-
    TypeArgs = type_args(type_of(Result)),
    TypeDesc = list.det_head(TypeArgs),
    Result0 = unmarshal_to_type_2(TypeDesc, Value),
    (
        Result0 = ok(Univ),
        det_univ_to_type(Univ, Type),
        Result = ok(Type)
    ;
        Result0 = error(Msg),
        Result = error(Msg)
    ).

:- func unmarshal_to_type_2(type_desc, value) = maybe_error(univ).

unmarshal_to_type_2(TypeDesc, Value) = Result :-
    type_ctor_and_args(TypeDesc, TypeCtor, TypeArgs),
    type_ctor_name_and_arity(TypeCtor, ModuleName, TypeName, Arity),
    % The following if-then-else should be ordered so that common types occur
    % first.
    ( if
        % The value is a builtin type.
        ModuleName = "builtin",
        Arity = 0
    then
        Result = to_builtin_type(TypeName, Value)
    else if
        % Is this type a Mercury Boolean?
        ModuleName = "bool",
        TypeName = "bool",
        Arity = 0,
        TypeArgs = []
    then
        Result = to_bool_type(Value)
    else if
        % Is this type a Mercury list?
        ModuleName = "list",
        TypeName = "list",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_list_type(ElemTypeDesc, Value)
    else if
        ModuleName = "set_ordlist",
        TypeName = "set_ordlist",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_set_ordlist_type(ElemTypeDesc, Value)
    else if
        % Is this type a Mercury array?
        ModuleName = "array",
        TypeName = "array",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_array_type(ElemTypeDesc, Value)
    else if
        % Is this type a Mercury integer?
        ModuleName = "integer",
        TypeName = "integer",
        Arity = 0,
        TypeArgs = []
    then
        Result = to_integer_type(Value)
    else if
        % Is this type a Mercury date/0 type?
        ModuleName = "calendar",
        TypeName = "date",
        Arity = 0,
        TypeArgs = []
    then
        Result = to_date_type(Value)
    else if
        % Is this type a Mercury duration/0 type?
        ModuleName = "calendar",
        TypeName = "duration",
        Arity = 0,
        TypeArgs = []
    then
        Result = to_duration_type(Value)
    else if
        % Is this type a Mercury cord?
        ModuleName = "cord",
        TypeName = "cord",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_cord_type(ElemTypeDesc, Value)
    else if
        % Is this type a Mercury version array?
        ModuleName = "version_array",
        TypeName = "version_array",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_version_array_type(ElemTypeDesc, Value)
    else if
        % Is this type a Mercury bitmap?
        ModuleName = "bitmap",
        TypeName = "bitmap",
        Arity = 0,
        TypeArgs = []
    then
        Result = to_bitmap_type(Value)
    else if
        % Is this type a Mercury maybe/1?
        ModuleName = "maybe",
        TypeName = "maybe",
        Arity = 1,
        TypeArgs = [ArgTypeDesc]
    then
        Result = to_maybe_type(ArgTypeDesc, Value)
    else if
        % Is this type a Mercury pair/2?
        ModuleName = "pair",
        TypeName = "pair",
        Arity = 2,
        TypeArgs = [FstTypeDesc, SndTypeDesc]
    then
        Result = to_pair_type(FstTypeDesc, SndTypeDesc, Value)
    else if
        ModuleName = "tree234",
        TypeName = "tree234",
        Arity = 2,
        TypeArgs = [KeyTypeDesc, ValueTypeDesc]
    then
        Result = to_tree234_type(KeyTypeDesc, ValueTypeDesc, Value)
    else if
        ModuleName = "bimap",
        TypeName = "bimap",
        Arity = 2,
        TypeArgs = [KeyTypeDesc, ValueTypeDesc]
    then
        Result = to_bimap_type(KeyTypeDesc, ValueTypeDesc, Value)
    else if
        ModuleName = "set_unordlist",
        TypeName = "set_unordlist",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_set_unordlist_type(ElemTypeDesc, Value)
    else if
        ModuleName = "set_tree234",
        TypeName = "set_tree234",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_set_tree234_type(ElemTypeDesc, Value)
    else if
        ModuleName = "set_ctree234",
        TypeName = "set_ctree234",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_set_ctree234_type(ElemTypeDesc, Value)
    else if
        ModuleName = "set_bbbtree",
        TypeName = "set_bbbtree",
        Arity = 1,
        TypeArgs = [ElemTypeDesc]
    then
        Result = to_set_bbbtree_type(ElemTypeDesc, Value)
    else if
        % Check if this is a (non-list) d.u. type.
        NumFunctors = num_functors(TypeDesc)
    then
        % Is this type an enumeration?
        ( if all_functors_have_arity_zero(TypeDesc, 0, NumFunctors)
        then Result = to_enum_type(TypeDesc, Value)
        else Result = to_du_type(TypeDesc, NumFunctors, Value)
        )
    else
        string.format("cannot convert JSON for type '%s.%s'/%d",
            [s(ModuleName), s(TypeName), i(Arity)], Msg),
        Result = error(Msg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> builtin types.
%

:- func to_builtin_type(string, value) = maybe_error(univ).

to_builtin_type(TypeName, Value) = Result :-
    ( if
        TypeName = "int",
        Value = number(Number)
    then
        % XXX check that Number does not have a fractional part.
        Univ = univ(round_to_int(Number)),
        Result = ok(Univ)
    else if
        TypeName = "float",
        Value = number(Number)
    then
        Univ = univ(Number),
        Result = ok(Univ)
    else if
        TypeName = "string",
        Value = string(String)
    then
        Univ = univ(String),
        Result = ok(Univ)
    else if
        TypeName = "character",
        Value = string(String),
        string.length(String, 1),
        Char = String ^ elem(0)
    then
        Univ = univ(Char),
        Result = ok(Univ)
    else
        string.format("cannot convert builtin type `%s' from JSON",
            [s(TypeName)], Msg),
        Result = error(Msg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bool/0 type.
%

:- func to_bool_type(value) = maybe_error(univ).

to_bool_type(Value) = Result :-
    (
        Value = bool(Bool),
        Univ = univ(Bool),
        Result = ok(Univ)
    ;
        ( Value = null
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        Result = error("expected JSON Boolean for bool/1 conversion")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> integer/0 type.
%

:- func to_integer_type(value) = maybe_error(univ).

to_integer_type(Value) = Result :-
    (
        Value = string(String),
        ( if Integer = integer.from_string(String)
        then Result = ok(univ(Integer))
        else Result = error("string is not an integer/0")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        Result = error("expected JSON string for integer/0 conversion")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> date/0 type.
%

:- func to_date_type(value) = maybe_error(univ).

to_date_type(Value) = Result :-
    (
        Value = string(String),
        ( if calendar.date_from_string(String, Date)
        then Result = ok(univ(Date))
        else Result = error("string is not a date/0")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        Result = error("expected JSON string for date/0 conversion")
    ).

%-----------------------------------------------------------------------------%
%
% Json -> duration/0 types.
%

:- func to_duration_type(value) = maybe_error(univ).

to_duration_type(Value) = Result :-
    (
        Value = string(String),
        ( if calendar.duration_from_string(String, Duration)
        then Result = ok(univ(Duration))
        else Result = error("string is not a duration/0")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        Result = error("expected JSON string for duration/0 conversion")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bitmap/0 types.
%

:- func to_bitmap_type(value) = maybe_error(univ).

to_bitmap_type(Value) = Result :-
    (
        Value = string(String),
        ( if Bitmap = bitmap.from_string(String)
        then Result = ok(univ(Bitmap))
        else Result = error("string is not a bitmap/0")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        Result = error("expected JSON string for bitmap/0 conversion")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> list/1 types.
%

:- func to_list_type(type_desc, value) = maybe_error(univ).

to_list_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(RevElems),
            list.reverse(RevElems, Elems),
            Univ = univ(Elems),
            Result = ok(Univ)
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for list/1 conversion")
    ).

:- pred unmarshal_list_elems(list(value)::in, list(T)::in,
    maybe_error(list(T))::out) is det.

unmarshal_list_elems([], Ts, ok(Ts)).
unmarshal_list_elems([V | Vs], !.Ts, Result) :-
    MaybeT = unmarshal_to_type(V),
    (
        MaybeT = ok(T),
        !:Ts = [T | !.Ts],
        unmarshal_list_elems(Vs, !.Ts, Result)
    ;
        MaybeT = error(Msg),
        Result = error(Msg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> cord/1 types.
%

:- func to_cord_type(type_desc, value) = maybe_error(univ).

to_cord_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(RevElems),
            list.reverse(RevElems, Elems),
            Cord = cord.from_list(Elems),
            Univ = univ(Cord),
            Result = ok(Univ)
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for cord/1 conversion")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> array/1 types.
%

:- func to_array_type(type_desc, value) = maybe_error(univ).

to_array_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(RevElems),
            Array = array.from_reverse_list(RevElems),
            Univ = univ(Array),
            Result = ok(Univ)
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for array/1 conversion")
    ).   

%-----------------------------------------------------------------------------%
%
% JSON -> version_array/1 types.
%

:- func to_version_array_type(type_desc, value) = maybe_error(univ).

to_version_array_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(RevElems),
            list.reverse(RevElems, Elems),
            % XXX the version_array module does not have from_reverse_list.
            Array = version_array.from_list(Elems),
            Univ = univ(Array),
            Result = ok(Univ)
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for version_array/1 conversion")
    ).   

%-----------------------------------------------------------------------------%
%
% JSON -> set_ordlist/1 types.
%

:- func to_set_ordlist_type(type_desc, value) = maybe_error(univ).

to_set_ordlist_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            set_ordlist.list_to_set(Elems, Set),
            Result = ok(univ(Set))
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for set_ordlist/1 conversion")
    ).

:- func to_set_unordlist_type(type_desc, value) = maybe_error(univ).

to_set_unordlist_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            set_unordlist.list_to_set(Elems, Set),
            Result = ok(univ(Set))
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for set_unordlist/1 conversion")
    ).

:- func to_set_tree234_type(type_desc, value) = maybe_error(univ).

to_set_tree234_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            set_tree234.list_to_set(Elems, Set),
            Result = ok(univ(Set))
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for set_tree234/1 conversion")
    ).

:- func to_set_ctree234_type(type_desc, value) = maybe_error(univ).

to_set_ctree234_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            Set = set_ctree234.list_to_set(Elems),
            Result = ok(univ(Set))
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for set_ctree234/1 conversion")
    ).

:- func to_set_bbbtree_type(type_desc, value) = maybe_error(univ).

to_set_bbbtree_type(ElemTypeDesc, Value) = Result :-
    (
        Value = array(Values),
        (_ : ElemType) `has_type` ElemTypeDesc,
        unmarshal_list_elems(Values, [] : list(ElemType), ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            set_bbbtree.list_to_set(Elems, Set),
            Result = ok(univ(Set))
        ;
            ListElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON array for set_bbbtree/1 conversion")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> maybe/1 types.
%

:- func to_maybe_type(type_desc, value) = maybe_error(univ).

to_maybe_type(ArgTypeDesc, Value) = Result :-
    (_ : ArgType) `has_type` ArgTypeDesc,
    (
        Value = null,
        Univ = univ(no : maybe(ArgType)),
        Result = ok(Univ)
    ;
        ( Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        MaybeArgTerm : maybe_error(ArgType) = unmarshal_to_type(Value),
        (
            MaybeArgTerm = ok(ArgTerm),
            Univ = univ(yes(ArgTerm)),
            Result = ok(Univ)
        ;
            MaybeArgTerm = error(Msg),
            Result = error(Msg)
        )
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> pair/2 types.
%

:- func to_pair_type(type_desc, type_desc, value) = maybe_error(univ).

to_pair_type(FstTypeDesc, SndTypeDesc, Value) = Result :-
    (
        Value = object(Object),
        ( if
            map.count(Object) = 2,
            map.search(Object, "fst", FstValue),
            map.search(Object, "snd", SndValue)
        then
            (_ : FstType) `has_type` FstTypeDesc,
            MaybeFst : maybe_error(FstType) = unmarshal_to_type(FstValue),
            (
                MaybeFst = ok(Fst),
                (_ : SndType) `has_type` SndTypeDesc,
                MaybeSnd : maybe_error(SndType) = unmarshal_to_type(SndValue),
                (
                    MaybeSnd = ok(Snd),
                    Pair = Fst - Snd,
                    Univ = univ(Pair),
                    Result = ok(Univ)
                ;
                    MaybeSnd = error(Msg),
                    Result = error(Msg)
                )
            ;
                MaybeFst = error(Msg),
                Result = error(Msg)
            )
        else
            Result = error("object is not a pair/2")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = string(_)
        ; Value = array(_)
        ),
        Result = error("expected JSON object for pair/2")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> tree234/2 (map) types.
%

:- func to_tree234_type(type_desc, type_desc, value) = maybe_error(univ).

to_tree234_type(KeyTypeDesc, ValueTypeDesc, Value) = Result :-
    (
        Value = array(_),
        (_ : KeyType) `has_type` KeyTypeDesc,
        (_ : ValueType) `has_type` ValueTypeDesc,
        MaybeKVs : maybe_error(assoc_list(KeyType, ValueType)) =
            unmarshal_to_type(Value),
        (
            MaybeKVs = ok(KVs),
            map.from_assoc_list(KVs, Map),
            Result = ok(univ(Map))
        ;
            MaybeKVs = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = string(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON object for tree234/2")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bimap/2 types.
%

:- func to_bimap_type(type_desc, type_desc, value) = maybe_error(univ).

to_bimap_type(KeyTypeDesc, ValueTypeDesc, Value) = Result :-
    (
        Value = array(_),
        (_ : KeyType) `has_type` KeyTypeDesc,
        (_ : ValueType) `has_type` ValueTypeDesc,
        MaybeKVs : maybe_error(assoc_list(KeyType, ValueType)) =
            unmarshal_to_type(Value),
        (
            MaybeKVs = ok(KVs),
            ( if bimap.from_assoc_list(KVs, Bimap)
            then Result = ok(univ(Bimap))
            else Result = error("cannot create bimap: not a bijection")
            )
        ;
            MaybeKVs = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = string(_)
        ; Value = object(_)
        ),
        Result = error("expected JSON object for bimap/2")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> enum types.
%

:- func to_enum_type(type_desc, value) = maybe_error(univ).

to_enum_type(TypeDesc, Value) = Result :-
    (
        Value = string(Name),
        ( if
            find_functor(TypeDesc, Name, 0, FunctorNumLex, []),
            Univ = construct(TypeDesc, FunctorNumLex, [])
        then
            Result = ok(Univ)
        else
            string.format("cannot convert '%s' to enum", [s(Name)], Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        Result = error("expected JSON string for enum conversion")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> du types.

:- func to_du_type(type_desc, int, value) = maybe_error(univ).

to_du_type(TypeDesc, _NumFunctors, Value) = Result :-
    (
        Value = object(Object),
        NumMembers = map.count(Object),
        ( if
            map.search(Object, "functor", FunctorValue),
            FunctorValue = string(FunctorName)
        then
            ( if NumMembers = 1 then
                Result = to_du_type_2(TypeDesc, FunctorName, 0, [])
            else
                ( if
                    map.search(Object, "args", ArgsValue),
                    ArgsValue = array(ArgValues)
                then
                    list.length(ArgValues, Arity),
                    Result = to_du_type_2(TypeDesc, FunctorName, Arity, ArgValues)
                else
                    Result = error("expected array member named 'args' in d.u. object")
                )
            )
        else
            Result = error("expected string member named 'functor' in d.u. object")
        )    
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = string(_)
        ; Value = array(_)
        ),
        Result = error("expected JSON object for d.u. conversion")
    ).

:- func to_du_type_2(type_desc, string, int, list(json.value)) =
    maybe_error(univ).

to_du_type_2(TypeDesc, FunctorName, Arity, Args) = Result :-
    find_matching_functor(TypeDesc, FunctorName,
        Arity, MaybeMatchingFunctor),
    (
        MaybeMatchingFunctor = ok(MatchingFunctor),
        MatchingFunctor = matching_functor(FunctorNumLex, ArgTypes),
        du_args_to_types(Args, ArgTypes, [], ArgsResult),
        (
            ArgsResult = ok(RevArgUnivs),
            list.reverse(RevArgUnivs, ArgUnivs),
            ( if Univ = construct(TypeDesc, FunctorNumLex, ArgUnivs)
            then Result = ok(Univ)
            else Result = error("cannot construct term")    % XXX better error message.
            )
        ;
            ArgsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        MaybeMatchingFunctor = error(Msg),
        Result = error(Msg)
    ).

:- type matching_functor
    --->    matching_functor(functor_number_lex, list(type_desc)).

:- pred find_matching_functor(type_desc::in, string::in, int::in,
    maybe_error(matching_functor)::out) is det.

find_matching_functor(TypeDesc, FunctorName, Arity, MaybeMatchingFunctor) :-
    ( if NumFunctors = construct.num_functors(TypeDesc) then
        find_matching_functor_2(TypeDesc, FunctorName, Arity, NumFunctors,
            MaybeMatchingFunctor)
    else
        unexpected($file, $pred, "not a d.u. type")
    ).

:- pred find_matching_functor_2(type_desc::in, string::in, int::in,
    int::in, maybe_error(matching_functor)::out) is det.

find_matching_functor_2(TypeDesc, FunctorName, Arity, !.Num,
        MaybeMatchingFunctor) :-
    ( if !.Num < 0 then
        unexpected($file, $pred, "not a d.u. type")
    else
        !:Num = !.Num - 1,
        ( if
            get_functor(TypeDesc, !.Num, FunctorName, Arity, ArgPsuedoTypes)
        then
            % XXX the stdlib should have a predicate version of
            % ground_pseudo_type_desc_to_type_desc/1.
            ToTypeDescPred = (pred(PTD::in, TD::out) is semidet :-
                TD = ground_pseudo_type_desc_to_type_desc(PTD)
            ),
            ( if
                list.map(ToTypeDescPred, ArgPsuedoTypes, ArgTypes)
            then
                MatchingFunctor = matching_functor(!.Num, ArgTypes),
                MaybeMatchingFunctor = ok(MatchingFunctor)
            else
                % XXX we could be more specific about 
                % what the problem is here (e.g. one or more of the
                % arguments is an existentially quantified type variable.
                string.format("%s/%d is not a ground functor",
                    [s(FunctorName), i(Arity)], Msg),
                MaybeMatchingFunctor = error(Msg)
            )
        else
            find_matching_functor_2(TypeDesc, FunctorName, Arity,
                !.Num, MaybeMatchingFunctor)
        )
    ).

:- pred du_args_to_types(list(json.value)::in, list(type_desc)::in,
    list(univ)::in, maybe_error(list(univ))::out) is det.

du_args_to_types([], [], RevArgUnivs, ok(RevArgUnivs)).
du_args_to_types([], [_ | _], _, _) :-
    unexpected($file, $pred, "argument length mismatch (1)").
du_args_to_types([_ | _], [], _, _) :-
    unexpected($file, $pred, "argument length mismatch (2)").
du_args_to_types([Value | Values], [TypeDesc | TypeDescs], !.RevArgUnivs, Result) :-
    MaybeUniv = unmarshal_to_type_2(TypeDesc, Value),
    (
        MaybeUniv = ok(Univ),
        !:RevArgUnivs = [Univ | !.RevArgUnivs],
        du_args_to_types(Values, TypeDescs, !.RevArgUnivs, Result)
    ;
        MaybeUniv = error(Msg),
        Result = error(Msg)
    ).

%-----------------------------------------------------------------------------%
:- end_module json.unmarshal.
%-----------------------------------------------------------------------------%
