%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013, Julien Fischer.
% All rights reserved.
%
% Author: Julien Fischer <jfischer@www.opturion.com>
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
        % Is this type a Mercury maybe/1?
        ModuleName = "maybe",
        TypeName = "maybe",
        Arity = 1,
        TypeArgs = [ArgTypeDesc]
    then
        Result = to_maybe_type(ArgTypeDesc, Value)
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
        string.format("cannot convert JSON for type %s.%s/%d",
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
        Char = String ^ elem(1)
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
            string.format("cannot convert `%s' to enum", [s(Name)], Msg),
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

to_du_type(TypeDesc, NumFunctors, Value) = Result :-
    (
        Value = object(Object),
        NumMembers = map.count(Object),
        find_matching_functor(TypeDesc, Object, NumMembers, 0, NumFunctors,
            MatchingFunctorResult),
        (
            MatchingFunctorResult = mfres_match(FunctorNumLex, ArgUnivs),
            ( if Univ = construct(TypeDesc, FunctorNumLex, ArgUnivs)
            then Result = ok(Univ)
            else Result = error("cannot construct term")
            )
        ;
            MatchingFunctorResult = mfres_no_match,
            Result = error("cannot find functor matching JSON object")
        ;
            MatchingFunctorResult = mfres_error(Msg),
            Result = error(Msg)
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

:- type matching_functor_result
    --->    mfres_match(functor_number_lex, list(univ))
    ;       mfres_no_match 
    ;       mfres_error(string). 

:- pred find_matching_functor(type_desc::in, object::in, int::in,
    functor_number_lex::in, functor_number_lex::in,
    matching_functor_result::out) is det.

find_matching_functor(TypeDesc, Object, NumMembers, FunctorNumLex,
        NumFunctors, Result) :-
    ( if FunctorNumLex < NumFunctors then
        ( if 
            get_functor_with_names(TypeDesc, FunctorNumLex, FunctorName,
                Arity, ArgTypes, ArgNames)
        then
            ( if
                % Zero-arity constructors are represented by the following JSON:
                %
                %    { "<functor-name>" : null }
                %
                Arity = 0,
                NumMembers = 1,
                map.search(Object, FunctorName, Value),
                Value = null
            then
                Result = mfres_match(FunctorNumLex, [])
            else if
                Arity = NumMembers,
                % Because field names need to be unique within a module we only
                % need to check the first one to see if we have a match.
                ArgNames = [MaybeFirstArgName | _],
                MaybeFirstArgName = yes(FirstArgName),  % XXX Maybe abort if no?
                map.contains(Object, FirstArgName)
            then
                du_args_to_types(Object, ArgTypes, ArgNames, [], ArgResult),
                (
                    ArgResult = ok(RevArgUnivs),
                    list.reverse(RevArgUnivs, ArgUnivs),
                    Result = mfres_match(FunctorNumLex, ArgUnivs)
                ;
                    ArgResult = error(Msg),
                    Result = mfres_error(Msg)
                )
            else
                find_matching_functor(TypeDesc, Object, NumMembers, FunctorNumLex + 1,
                    NumFunctors, Result)
            )
        else
            Result = mfres_error("type-desc for non-d.u. type")
        )        
    else
        Result = mfres_no_match
    ).

:- pred du_args_to_types(object::in,
    list(pseudo_type_desc)::in, list(maybe(string))::in,
    list(univ)::in, maybe_error(list(univ))::out) is det.

du_args_to_types(_, [], [], RevArgUnivs, ok(RevArgUnivs)).
du_args_to_types(_, [], [_ | _], _, _) :-
    unexpected($file, $pred, "argument length mismatch (1)").
du_args_to_types(_, [_ | _], [], _, _) :-
    unexpected($file, $pred, "argument length mismatch (2)").
du_args_to_types(Object, [PTD | PTDs], [MaybeFN | MaybeFNs],
        !.RevArgUnivs, Result) :-
    ( if TypeDesc = ground_pseudo_type_desc_to_type_desc(PTD) then
        (
            MaybeFN = yes(FieldName),
            % XXX should report an error if field is missing from object.
            FieldValue = map.lookup(Object, FieldName),
            MaybeUniv = unmarshal_to_type_2(TypeDesc, FieldValue),
            (
                MaybeUniv = ok(Univ),
                !:RevArgUnivs = [Univ | !.RevArgUnivs],
                du_args_to_types(Object, PTDs, MaybeFNs,
                    !.RevArgUnivs, Result)
            ;
                MaybeUniv = error(Msg),
                Result = error(Msg)
            )
        ;
            MaybeFN = no,
            % XXX report which one.
            Result = error("missing field name")
        )
    else
        % Say for which field.
        Result = error("pseudo-type-desc is not ground") 
    ).

%-----------------------------------------------------------------------------%
:- end_module json.unmarshal.
%-----------------------------------------------------------------------------%
