%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2015 Julien Fischer.
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

:- func int_from_json(value) = maybe_error(int).
:- func float_from_json(value) = maybe_error(float).
:- func char_from_json(value) = maybe_error(char).
:- func string_from_json(value) = maybe_error(string).
:- func bool_from_json(value) = maybe_error(bool).
:- func integer_from_json(value) = maybe_error(integer).
:- func date_time_from_json(value) = maybe_error(date).
:- func duration_from_json(value) = maybe_error(duration).
:- func bitmap_from_json(value) = maybe_error(bitmap).
:- func list_from_json(value) = maybe_error(list(T)) <= from_json(T).
:- func cord_from_json(value) = maybe_error(cord(T)) <= from_json(T).
:- func array_from_json(value) = maybe_error(array(T)) <= from_json(T).
:- func version_array_from_json(value) = maybe_error(version_array(T))
    <= from_json(T).
:- func set_ordlist_from_json(value) = maybe_error(set_ordlist(T))
    <= from_json(T).
:- func set_unordlist_from_json(value) = maybe_error(set_unordlist(T))
    <= from_json(T).
:- func set_tree234_from_json(value) = maybe_error(set_tree234(T))
    <= from_json(T).
:- func set_ctree234_from_json(value) = maybe_error(set_ctree234(T))
    <= from_json(T).
:- func set_bbbtree_from_json(value) = maybe_error(set_bbbtree(T))
    <= from_json(T).
:- func pair_from_json(value) = maybe_error(pair(A, B)) <=
    (from_json(A), from_json(B)).
:- func maybe_from_json(value) = maybe_error(maybe(T)) <= from_json(T).
:- func map_from_json(value) = maybe_error(map(K, V))
    <= (from_json(K), from_json(V)).
:- func bimap_from_json(value) = maybe_error(bimap(K, V))
    <= (from_json(K), from_json(V)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

int_from_json(Value) = Result :-
    ( if Value = number(Number) then
        % XXX check that Number does not have a fractional part.
        Result = ok(round_to_int(Number))
    else
        Result = error("cannot convert builtin type 'int' from JSON")
    ).

float_from_json(Value) = Result :-
    ( if Value = number(Number)
    then Result = ok(Number)
    else Result = error("cannot convert builtin type 'float' from JSON")
    ).

char_from_json(Value) = Result :-
    ( if
        Value = string(String),
        string.length(String, 1),
        Char = String ^ elem(0)
    then
        Result = ok(Char)
    else
        Result = error("cannot convert builtin type 'char' from JSON")
    ).

string_from_json(Value) = Result :-
    ( if Value = string(String)
    then Result = ok(String)
    else Result = error("cannot convert builtin type 'string' from JSON")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bool/0 type.
%

bool_from_json(Value) = Result :-
    (
        Value = bool(Bool),
        Result = ok(Bool)
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

integer_from_json(Value) = Result :-
    (
        Value = string(String),
        ( if Integer : integer = integer.from_string(String)
        then Result = ok(Integer)
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

date_time_from_json(Value) = Result :-
    (
        Value = string(String),
        ( if calendar.date_from_string(String, Date)
        then Result = ok(Date)
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
% JSON -> duration/0 types.
%

duration_from_json(Value) = Result :-
    (
        Value = string(String),
        ( if calendar.duration_from_string(String, Duration)
        then Result = ok(Duration)
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

bitmap_from_json(Value) = Result :-
    (
        Value = string(String),
        ( if Bitmap = bitmap.from_string(String)
        then Result = ok(Bitmap)
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

list_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(RevElems),
            list.reverse(RevElems, Elems),
            Result = ok(Elems)
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
    maybe_error(list(T))::out) is det <= from_json(T).

unmarshal_list_elems([], Ts, ok(Ts)).
unmarshal_list_elems([V | Vs], !.Ts, Result) :-
    MaybeT = from_json(V),
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

cord_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(RevElems),
            list.reverse(RevElems, Elems),
            Cord = cord.from_list(Elems),
            Result = ok(Cord)
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

array_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(RevElems),
            Array = array.from_reverse_list(RevElems),
            Result = ok(Array)
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

version_array_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(RevElems),
            list.reverse(RevElems, Elems),
            % XXX the version_array module does not have from_reverse_list.
            Array = version_array.from_list(Elems),
            Result = ok(Array)
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

set_ordlist_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            set_ordlist.list_to_set(Elems, Set),
            Result = ok(Set)
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

set_unordlist_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            set_unordlist.list_to_set(Elems, Set),
            Result = ok(Set)
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

set_tree234_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            set_tree234.list_to_set(Elems, Set),
            Result = ok(Set)
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

set_ctree234_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            Set = set_ctree234.list_to_set(Elems),
            Result = ok(Set)
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

set_bbbtree_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], ListElemsResult),
        (
            ListElemsResult = ok(Elems),
            set_bbbtree.list_to_set(Elems, Set),
            Result = ok(Set)
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

maybe_from_json(Value) = Result :-
    (
        Value = null,
        Result = ok(no)
    ;
        ( Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        MaybeArgTerm = from_json(Value),
        (
            MaybeArgTerm = ok(ArgTerm),
            Result = ok(yes(ArgTerm))
        ;
            MaybeArgTerm = error(Msg),
            Result = error(Msg)
        )
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> pair/2 types.
%

pair_from_json(Value) = Result :-
    (
        Value = object(Object),
        ( if
            map.count(Object) = 2,
            map.search(Object, "fst", FstValue),
            map.search(Object, "snd", SndValue)
        then
            MaybeFst = from_json(FstValue),
            (
                MaybeFst = ok(Fst),
                MaybeSnd = from_json(SndValue),
                (
                    MaybeSnd = ok(Snd),
                    Pair = Fst - Snd,
                    Result = ok(Pair)
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
% JSON -> map/2 types.
%

map_from_json(Value) = Result :-
    (
        Value = array(_),
        MaybeKVs = from_json(Value),
        (
            MaybeKVs = ok(KVs),
            map.from_assoc_list(KVs, Map),
            Result = ok(Map)
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
        Result = error("expected JSON object for map/2")
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bimap/2 types.
%

bimap_from_json(Value) = Result :-
    (
        Value = array(_),
        MaybeKVs = from_json(Value),
        (
            MaybeKVs = ok(KVs),
            ( if bimap.from_assoc_list(KVs, Bimap)
            then Result = ok(Bimap)
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
:- end_module json.unmarshal.
%-----------------------------------------------------------------------------%
