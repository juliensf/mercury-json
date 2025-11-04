%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2020, 2025 Julien Fischer.
% See the file COPYING for license details.
%-----------------------------------------------------------------------------%
%
% This submodule implements unmarshaling of Mercury primitive and library types
% from JSON.
%
%-----------------------------------------------------------------------------%

:- module json.unmarshal.
:- interface.

%-----------------------------------------------------------------------------%

:- func bag_from_json(pointer, value) = from_json_result(bag(T))
    <= from_json(T).

:- func int_from_json(pointer, value) = from_json_result(int).

:- func int8_from_json(pointer, value) = from_json_result(int8).

:- func int16_from_json(pointer, value) = from_json_result(int16).

:- func int32_from_json(pointer, value) = from_json_result(int32).

:- func int64_from_json(pointer, value) = from_json_result(int64).

:- func uint8_from_json(pointer, value) = from_json_result(uint8).

:- func uint16_from_json(pointer, value) = from_json_result(uint16).

:- func uint64_from_json(pointer, value) = from_json_result(uint64).

:- func float_from_json(pointer, value) = from_json_result(float).

:- func char_from_json(pointer, value) = from_json_result(char).

:- func string_from_json(pointer, value) = from_json_result(string).

:- func bool_from_json(pointer, value) = from_json_result(bool).

:- func integer_from_json(pointer, value) = from_json_result(integer).

:- func kv_list_from_json(pointer, value) = from_json_result(kv_list(K, V))
    <= (from_json(K), from_json(V)).

:- func date_time_from_json(pointer, value) = from_json_result(date).

:- func duration_from_json(pointer, value) = from_json_result(duration).

:- func bitmap_from_json(pointer, value) = from_json_result(bitmap).

:- func one_or_more_from_json(pointer, value) = from_json_result(one_or_more(T))
    <= from_json(T).

:- func list_from_json(pointer, value) = from_json_result(list(T))
    <= from_json(T).

:- func cord_from_json(pointer, value) = from_json_result(cord(T))
    <= from_json(T).

:- func array_from_json(pointer, value) = from_json_result(array(T))
    <= from_json(T).

:- func array2d_from_json(pointer, value) = from_json_result(array2d(T))
    <= from_json(T).

:- func version_array_from_json(pointer, value) =
    from_json_result(version_array(T)) <= from_json(T).

:- func rational_from_json(pointer, value) =
    from_json_result(rational).

:- func set_ordlist_from_json(pointer, value) =
    from_json_result(set_ordlist(T)) <= from_json(T).

:- func set_unordlist_from_json(pointer, value) =
    from_json_result(set_unordlist(T)) <= from_json(T).

:- func set_tree234_from_json(pointer, value) =
    from_json_result(set_tree234(T)) <= from_json(T).

:- func set_ctree234_from_json(pointer, value) =
    from_json_result(set_ctree234(T)) <= from_json(T).

:- func set_bbbtree_from_json(pointer, value) =
    from_json_result(set_bbbtree(T)) <= from_json(T).

:- func pair_from_json(pointer, value) =
    from_json_result(pair(A, B)) <= (from_json(A), from_json(B)).

:- func maybe_from_json(pointer, value) = from_json_result(maybe(T))
    <= from_json(T).

:- func maybe_error_from_json(pointer, value) =
    from_json_result(maybe_error(T, E)) <= (from_json(T), from_json(E)).

:- func map_from_json(pointer, value) = from_json_result(map(K, V))
    <= (from_json(K), from_json(V)).

:- func rbtree_from_json(pointer, value) = from_json_result(rbtree(K, V))
    <= (from_json(K), from_json(V)).

:- func bimap_from_json(pointer, value) = from_json_result(bimap(K, V))
    <= (from_json(K), from_json(V)).

:- func unit_from_json(pointer, value) = from_json_result(unit).

:- func queue_from_json(pointer, value) = from_json_result(queue(T))
    <= from_json(T).

:- func pqueue_from_json(pointer, value) = from_json_result(pqueue(K, V))
    <= (from_json(K), from_json(V)).

:- func digraph_from_json(pointer, value) = from_json_result(digraph(T))
    <= from_json(T).

:- func json_pointer_from_json(pointer, value) =
    from_json_result(json.pointer).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.from_json_util.

:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module uint8.
:- import_module uint16.

%-----------------------------------------------------------------------------%

bag_from_json(Pointer, Value) = Result :-
    ( if Value = array(Elems) then
        unmarshal_list_of_pairs(Pointer, "value", "count", 0, Elems, [],
            MaybeVCs),
        (
            MaybeVCs = ok(VCs),
            list.length(VCs, NumVCs),
            add_values_and_counts(Pointer, VCs, NumVCs - 1, bag.init, Result)
        ;
            MaybeVCs = error(Msg),
            Result = error(Msg)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

:- pred add_values_and_counts(pointer::in, list(pair(T, int))::in, int::in,
    bag(T)::in, from_json_result(bag(T))::out) is det.

add_values_and_counts(_, [], _, Bag, ok(Bag)).
add_values_and_counts(Pointer, [VC | VCs], Index, !.Bag, Result) :-
    VC = Value - Count,
    ( if Count > 0 then
        bag.det_insert_duplicates(Count, Value, !Bag),
        add_values_and_counts(Pointer, VCs, Index - 1, !.Bag, Result)
    else
        VCPointer = append_token(append_int_token(Pointer, Index), "count"),
        Result = make_other_error(VCPointer, "count is less than 1")
    ).

%-----------------------------------------------------------------------------%

int_from_json(Pointer, JValue) = Result :-
    ( if JValue = number(Number) then
        ( if float.is_finite(Number) then
            % XXX check that Number does not have a fractional part.
            Result = ok(round_to_int(Number))
        else
            Result = make_non_finite_number_error(Pointer)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "number", JValue)
    ).

int8_from_json(Pointer, JValue) = Result :-
    ( if JValue = number(Number) then
        ( if float.is_finite(Number) then
            Int = truncate_to_int(Number),
            ( if int8.from_int(Int, Int8) then
                Result = ok(Int8)
            else
                Result = make_out_of_bounds_number_error(Pointer)
            )
        else
            Result = make_non_finite_number_error(Pointer)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "number", JValue)
    ).

int16_from_json(Pointer, JValue) = Result :-
    ( if JValue = number(Number) then
        ( if float.is_finite(Number) then
            Int = truncate_to_int(Number),
            ( if int16.from_int(Int, Int16) then
                Result = ok(Int16)
            else
                Result = make_out_of_bounds_number_error(Pointer)
            )
        else
            Result = make_non_finite_number_error(Pointer)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "number", JValue)
    ).

int32_from_json(Pointer, JValue) = Result :-
    ( if JValue = number(Number) then
        ( if float.is_finite(Number) then
            Int = truncate_to_int(Number),
            ( if int32.from_int(Int, Int32) then
                Result = ok(Int32)
            else
                Result = make_out_of_bounds_number_error(Pointer)
            )
        else
            Result = make_non_finite_number_error(Pointer)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "number", JValue)
    ).

int64_from_json(Pointer, JValue) = Result :-
    ( if JValue = string(NumberStr) then
        ( if integer.from_string(NumberStr, Number) then
            ( if integer.to_int64(Number, Int64) then
                Result = ok(Int64)
            else
                Result = make_out_of_bounds_number_error(Pointer)
            )
        else
            Result = make_other_error(Pointer,
                "cannot convert string to integer")
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "string", JValue)
    ).

uint8_from_json(Pointer, JValue) = Result :-
    ( if JValue = number(Number) then
        ( if float.is_finite(Number) then
            Int = truncate_to_int(Number),
            ( if uint8.from_int(Int, UInt8) then
                Result = ok(UInt8)
            else
                Result = make_out_of_bounds_number_error(Pointer)
            )
        else
            Result = make_non_finite_number_error(Pointer)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "number", JValue)
    ).

uint16_from_json(Pointer, JValue) = Result :-
    ( if JValue = number(Number) then
        ( if float.is_finite(Number) then
            Int = truncate_to_int(Number),
            ( if uint16.from_int(Int, UInt16) then
                Result = ok(UInt16)
            else
                Result = make_out_of_bounds_number_error(Pointer)
            )
        else
            Result = make_non_finite_number_error(Pointer)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "number", JValue)
    ).

uint64_from_json(Pointer, JValue) = Result :-
    ( if JValue = string(NumberStr) then
        ( if integer.from_string(NumberStr, Number) then
            ( if integer.to_uint64(Number, UInt64) then
                Result = ok(UInt64)
            else
                Result = make_out_of_bounds_number_error(Pointer)
            )
        else
            Result = make_other_error(Pointer,
                "cannot convert string to integer")
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "string", JValue)
    ).

float_from_json(Pointer, JValue) = Result :-
    ( if JValue = number(Number) then
        ( if float.is_finite(Number) then
            Result = ok(Number)
        else
            Result = make_non_finite_number_error(Pointer)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "number", JValue)
    ).

char_from_json(Pointer, JValue) = Result :-
    ( if JValue = string(String) then
        string.length(String, Length),
        ( if
            Length = 1,
            Char = String ^ elem(0)
        then
            Result = ok(Char)
        else
            % XXX ERROR
            string.format("has length %d, expected length 1",
                [i(Length)], Msg),
            Result = make_other_error(Pointer, Msg)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "string", JValue)
    ).

string_from_json(Pointer, JValue) = Result :-
    ( if JValue = string(String) then
        Result = ok(String)
    else
        Result = make_value_type_mismatch_error(Pointer, "string", JValue)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bool/0 type.
%

bool_from_json(Pointer, JValue) = Result :-
    ( if JValue = bool(Bool) then
        Result = ok(Bool)
    else
        Result = make_value_type_mismatch_error(Pointer, "Boolean", JValue)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> integer/0 type.
%

integer_from_json(Pointer, JValue) = Result :-
    ( if JValue = string(String) then
        ( if integer.from_string(String, Integer) then
            Result = ok(Integer)
        else
            Result = make_from_string_failed_error(Pointer, String)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "string", JValue)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> kv_list/2 type.
%

kv_list_from_json(Pointer, Value) = Result :-
    ( if Value = array(Elems) then
        unmarshal_list_of_pairs(Pointer, "key", "value", 0, Elems,
            [], MaybeKVs),
        (
            MaybeKVs = ok(RevKVs),
            list.reverse(RevKVs, KVs),
            KVList = assoc_list_to_kv_list(KVs),
            Result = ok(KVList)
        ;
            MaybeKVs = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> date/0 type.
%

date_time_from_json(Pointer, JValue) =
    string_value_to_type(Pointer, JValue, calendar.date_from_string).

%-----------------------------------------------------------------------------%
%
% JSON -> duration/0 types.
%

duration_from_json(Pointer, JValue) =
    string_value_to_type(Pointer, JValue, calendar.duration_from_string).

%-----------------------------------------------------------------------------%
%
% JSON -> bitmap/0 types.
%

bitmap_from_json(Pointer, JValue) =
    string_value_to_type(Pointer, JValue, bitmap.from_string).

%-----------------------------------------------------------------------------%
%
% JSON -> one_or_more/1 types.
%

one_or_more_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            ElemsList = cord.to_list(ElemsCord),
            ( if list_to_one_or_more(ElemsList, OneOrMore) then
                Result = ok(OneOrMore)
            else
                Result = make_other_error(Pointer, "expected non-empty array")
            )
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> list/1 types.
%

list_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            ElemsList = cord.to_list(ElemsCord),
            Result = ok(ElemsList)
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

:- pred unmarshal_list_elems(pointer::in, list(value)::in, int::in,
    cord(T)::in, from_json_result(cord(T))::out) is det <= from_json(T).

unmarshal_list_elems(_, [], _, Ts, ok(Ts)).
unmarshal_list_elems(Pointer, [V | Vs], Index, !.Ts, Result) :-
    MaybeT = from_json(append_int_token(Pointer, Index), V),
    (
        MaybeT = ok(T),
        cord.snoc(T, !Ts),
        unmarshal_list_elems(Pointer, Vs, Index + 1, !.Ts, Result)
    ;
        MaybeT = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> cord/1 types.
%

cord_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            Result = ok(ElemsCord)
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> array/1 types.
%

array_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            ElemsList = cord.to_list(ElemsCord),
            Array = array.from_list(ElemsList),
            Result = ok(Array)
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> array2d/1 types.
%

array2d_from_json(Pointer, Value) = Result :-
    ( if Value = array(RowValues) then
        (
            RowValues = [],
            Array2d = array2d.from_lists([]),
            Result = ok(Array2d)
        ;
            RowValues = [FirstRowValue | RestRowValues],
            list.length(RowValues, ExpectedNumRows),
            ( if FirstRowValue = array(FirstRowValues)  then
                (
                    FirstRowValues = [],
                    check_array2d_rows_are_empty(1, RestRowValues,
                        RowsAreEmptyResult),
                    (
                        RowsAreEmptyResult = crr_all_empty,
                        % NOTE: the bounds of array2d.from_lists([]) differ
                        % from those of array2d.from_lists([[],[])) etc. I'm
                        % not sure if this behaviour was intentional, but until
                        % it is clarified we reproduce it here as well, hence
                        % the following.
                        list.duplicate(ExpectedNumRows, [], NestedEmptyLists),
                        Array2d = array2d.from_lists(NestedEmptyLists),
                        Result = ok(Array2d)
                    ;
                        RowsAreEmptyResult = crr_non_empty(FirstNonEmptyRowNo,
                            FirstNonEmptyRowLength),
                        string.format(
                            "row 0 has length 0, row %d has length %d",
                            [i(FirstNonEmptyRowNo), i(FirstNonEmptyRowLength)],
                            Msg),
                        Result = make_other_error(Pointer, Msg)
                    ;
                        RowsAreEmptyResult = crr_bad_type(RowNo, RowValue),
                        string.format(
                            " row %d is %s, expected array",
                            [i(RowNo), s(to_value_desc(RowValue))], Msg),
                        Result = make_other_error(Pointer, Msg)
                    )
                ;
                    FirstRowValues = [FirstElemValue | OtherElemValues],
                    list.length(FirstRowValues, ExpectedNumCols),
                    FirstRowPointer = append_int_token(Pointer, 0),
                    FirstElemPointer = append_int_token(FirstRowPointer, 0),
                    FirstElemResult = from_json(FirstElemPointer, FirstElemValue),
                    (
                        FirstElemResult = ok(FirstElem),
                        some [!Array2d] (
                            !:Array2d = array2d.init(ExpectedNumRows,
                                ExpectedNumCols, FirstElem),
                            array2d_unmarshal_elems(FirstRowPointer,
                                0, % Row
                                1, % Col
                                ExpectedNumCols, OtherElemValues,
                                !Array2d, FirstRowResult),
                            (
                                FirstRowResult = ok,
                                array2d_unmarshal_rows(Pointer, 1,
                                    ExpectedNumRows, ExpectedNumCols,
                                    RestRowValues, !Array2d, RestRowsResult),
                                (
                                    RestRowsResult = ok,
                                    Result = ok(!.Array2d)
                                ;
                                    RestRowsResult = error(RestRowsError),
                                    Result = error(RestRowsError)
                                )
                            ;
                                FirstRowResult = error(Error),
                                Result = error(Error)
                            )
                        )
                    ;
                        FirstElemResult = error(FirstElemError),
                        Result = error(FirstElemError)
                    )
                )
            else
                FirstRowPointer = append_int_token(Pointer, 0),
                Result = make_value_type_mismatch_error(FirstRowPointer,
                    "array", FirstRowValue)
            )
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

:- type check_row_result
    --->    crr_all_empty
    ;       crr_non_empty(
                crr_ne_row_no :: int,
                crr_ne_length :: int
            )
    ;       crr_bad_type(
                crr_bt_row_no :: int,
                crr_bd_value  :: json.value
            ).

:- pred check_array2d_rows_are_empty(int::in, list(value)::in,
    check_row_result::out) is det.

check_array2d_rows_are_empty(_, [], crr_all_empty).
check_array2d_rows_are_empty(RowNo, [Value | Values], Result) :-
    (
        Value = array(Elems),
        (
            Elems = [],
            check_array2d_rows_are_empty(RowNo + 1, Values, Result)
        ;
            Elems = [_ | _],
            list.length(Elems, NumElems),
            Result = crr_non_empty(RowNo, NumElems)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        Result = crr_bad_type(RowNo, Value)
    ).

:- pred array2d_unmarshal_rows(pointer::in, int::in, int::in, int::in,
    list(value)::in, array2d(T)::array_di, array2d(T)::array_uo,
    from_json_res::out) is det <= from_json(T).

array2d_unmarshal_rows(Pointer, R, NumRows, NumCols, RowValues, !Array2d,
        Result) :-
    ( if R < NumRows then
        (
            RowValues = [],
            % This shouldn't occur since our caller checked the number of rows.
            unexpected($file, $pred, "too few rows")
        ;
            RowValues = [RowValue | RowValuesPrime],
            RowPointer = append_int_token(Pointer, R),
            (
                RowValue = array(ElemValues),
                array2d_unmarshal_elems(RowPointer, R, 0, NumCols, ElemValues,
                    !Array2d, RowResult),
                (
                    RowResult = ok,
                    array2d_unmarshal_rows(Pointer, R + 1, NumRows, NumCols,
                        RowValuesPrime, !Array2d, Result)
                ;
                    RowResult = error(Error),
                    Result = error(Error)
                )
            ;
                ( RowValue = null
                ; RowValue = bool(_)
                ; RowValue = string(_)
                ; RowValue = number(_)
                ; RowValue = object(_)
                ),
                TypeDesc = type_of(!.Array2d),
                ErrorDesc = value_type_mismatch(one_or_more("array", []),
                    to_value_desc(RowValue)),
                Error = from_json_error(Pointer, TypeDesc, ErrorDesc),
                Result = error(Error)
            )
        )
    else
        (
            RowValues = [],
            Result = ok
        ;
            RowValues = [_ | _],
            % This shouldn't occur since our caller checked the number of rows.
            unexpected($file, $pred, "too many rows")
        )
    ).

:- pred array2d_unmarshal_elems(pointer::in, int::in, int::in, int::in,
    list(value)::in, array2d(T)::array2d_di, array2d(T)::array2d_uo,
    from_json_res::out) is det <= from_json(T).

array2d_unmarshal_elems(Pointer, R, C, NumCols, RowValues, !Array2d, Result) :-
    ( if C < NumCols then
        (
            RowValues = [],
            TypeDesc = type_of(!.Array2d),
            string.format(
                "row %d has length %d, expected length %d",
                [i(R), i(C), i(NumCols)], ErrorMsg),
            Error = from_json_error(Pointer, TypeDesc, other(ErrorMsg)),
            Result = error(Error)
        ;
            RowValues = [RowValue | RowValuesPrime],
            ValuePointer = append_int_token(Pointer, C),
            ElemResult = from_json(ValuePointer, RowValue),
            (
                ElemResult = ok(Elem),
                % Safe since to reach this point we must be within the bounds
                % set when the array was created.
                array2d.unsafe_set(R, C, Elem, !Array2d),
                array2d_unmarshal_elems(Pointer, R, C + 1, NumCols,
                    RowValuesPrime, !Array2d, Result)
            ;
                ElemResult = error(ElemError),
                Result = error(ElemError)
            )
        )
    else
        (
            RowValues = [],
            Result = ok
        ;
            RowValues = [_ | _],
            TypeDesc = type_of(!.Array2d),
            list.length(RowValues, NumRemainingCols),
            string.format(
                "row %d has length %d, expected length %d",
                [i(R), i(C + NumRemainingCols), i(NumCols)],
                ErrorMsg),
            Error = from_json_error(Pointer, TypeDesc, other(ErrorMsg)),
            Result = error(Error)
        )
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> version_array/1 types.
%

version_array_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            ElemsList = cord.to_list(ElemsCord),
            Array = version_array.from_list(ElemsList),
            Result = ok(Array)
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> rational/0 types.
%

rational_from_json(Pointer, JValue) = Result :-
    ( if JValue = object(Object) then
        ( if
            map.search(Object, "numer", NumeratorValue)
        then
            ( if
                map.search(Object, "denom", DenominatorValue)
            then
                NumeratorPointer = append_token(Pointer, "numer"),
                MaybeNumerator = integer_from_json(NumeratorPointer,
                    NumeratorValue),
                (
                    MaybeNumerator = ok(Numerator),
                    DenominatorPointer = append_token(Pointer, "denom"),
                    MaybeDenominator = integer_from_json(DenominatorPointer,
                        DenominatorValue),
                    (
                        MaybeDenominator = ok(Denominator),
                        ( if integer.is_zero(Denominator) then
                            Result = make_other_error(Pointer,
                                "zero denominator")
                        else
                            Rational = rational.from_integers(Numerator,
                                Denominator),
                            Result = ok(Rational)
                        )
                    ;
                        MaybeDenominator = error(Error),
                        Result = error(Error)
                    )
                ;
                    MaybeNumerator = error(Error),
                    Result = error(Error)
                )
            else
                Result = make_missing_member_error(Pointer, "denom")
            )
        else
            Result = make_missing_member_error(Pointer, "numer")
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "object", JValue)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> set types.
%

set_ordlist_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        Result = unmarshal_set_ordlist_elems(Pointer, Values, 0,
            set_ordlist.init)
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

:- func unmarshal_set_ordlist_elems(json.pointer, list(json.value), int,
    set_ordlist(T)) = from_json_result(set_ordlist(T)) is det <= from_json(T).

unmarshal_set_ordlist_elems(_, [], _, Set) = ok(Set).
unmarshal_set_ordlist_elems(Pointer, [JValue | JValues], Index, !.Set)
        = Result :-
    MaybeElem = from_json(append_int_token(Pointer, Index), JValue),
    (
        MaybeElem = ok(Elem),
        set_ordlist.insert(Elem, !Set),
        Result = unmarshal_set_ordlist_elems(Pointer, JValues, Index + 1,
            !.Set)
    ;
        MaybeElem = error(Error),
        Result = error(Error)
    ).

%---------------------%

set_unordlist_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            ElemsList = cord.to_list(ElemsCord),
            set_unordlist.list_to_set(ElemsList, Set),
            Result = ok(Set)
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%---------------------%

set_tree234_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        Result = unmarshal_set_tree234_elems(Pointer, Values, 0,
            set_tree234.init)
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

:- func unmarshal_set_tree234_elems(json.pointer, list(json.value), int,
    set_tree234(T)) = from_json_result(set_tree234(T)) is det <= from_json(T).

unmarshal_set_tree234_elems(_, [], _, Set) = ok(Set).
unmarshal_set_tree234_elems(Pointer, [JValue | JValues], Index, !.Set)
        = Result :-
    MaybeElem = from_json(append_int_token(Pointer, Index), JValue),
    (
        MaybeElem = ok(Elem),
        set_tree234.insert(Elem, !Set),
        Result = unmarshal_set_tree234_elems(Pointer, JValues, Index + 1,
            !.Set)
    ;
        MaybeElem = error(Error),
        Result = error(Error)
    ).

%---------------------%

set_ctree234_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            ElemsList = cord.to_list(ElemsCord),
            Set = set_ctree234.list_to_set(ElemsList),
            Result = ok(Set)
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%---------------------%

set_bbbtree_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            ElemsList = cord.to_list(ElemsCord),
            set_bbbtree.list_to_set(ElemsList, Set),
            Result = ok(Set)
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> maybe/1 types.
%

maybe_from_json(Pointer, Value) = Result :-
    (
        Value = null,
        Result = ok(no)
    ;
        Value = object(Object),
        ( if
            map.search(Object, "yes", ArgValue)
        then
            YesPointer = append_token(Pointer, "yes"),
            MaybeArg = from_json(YesPointer, ArgValue),
            (
                MaybeArg = ok(Arg),
                Result = ok(yes(Arg))
            ;
                MaybeArg = error(Error),
                Result = error(Error)
            )
        else
            Result = make_missing_member_error(Pointer, "yes")
        )
    ;
        ( Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = array(_)
        ),
        Result = make_value_types_mismatch_error(Pointer,
            one_or_more("object", ["null"]), Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> maybe_error/2 types.
%

maybe_error_from_json(Pointer, Value) = Result :-
    ( if Value = object(Object) then
        ( if map.search(Object, "ok", OkValue) then
            MaybeOkValue = yes(OkValue)
        else
            MaybeOkValue = no
        ),
        ( if map.search(Object, "error", ErrorValue) then
            MaybeErrorValue = yes(ErrorValue)
        else
            MaybeErrorValue = no
        ),
        (
            MaybeOkValue = yes(_),
            MaybeErrorValue = yes(_),
            Result = make_conflicting_members_error(Pointer, "ok", "error")
        ;
            MaybeOkValue = yes(JOkValue),
            MaybeErrorValue = no,
            OkPointer = append_token(Pointer, "ok"),
            MaybeOkType = from_json(OkPointer, JOkValue),
            (
                MaybeOkType = ok(OkType),
                Result = ok(ok(OkType))
            ;
                MaybeOkType = error(Error),
                Result = error(Error)
            )
        ;
            MaybeOkValue = no,
            MaybeErrorValue = yes(JErrorValue),
            ErrorPointer = append_token(Pointer, "error"),
            MaybeErrorType = from_json(ErrorPointer, JErrorValue),
            (
                MaybeErrorType = ok(ErrorType),
                Result = ok(error(ErrorType))
            ;
                MaybeErrorType  = error(Error),
                Result = error(Error)
            )
        ;
            MaybeOkValue = no,
            MaybeErrorValue = no,
            % XXX ERROR
            Result = make_other_error(Pointer,
                "object is not a maybe_error/2 value")
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "object", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> pair/2 types.
%

pair_from_json(Pointer, JValue) =
    object_value_to_type2(Pointer, JValue, "fst", "snd", pair.pair).

%-----------------------------------------------------------------------------%
%
% JSON -> map/2 types.
%

map_from_json(Pointer, Value) = Result :-
    ( if Value = array(Elems) then
        unmarshal_list_of_pairs(Pointer, "key", "value", 0, Elems,
            [], MaybeKVs),
        (
            MaybeKVs = ok(KVs),
            map.from_assoc_list(KVs, Map),
            Result = ok(Map)
        ;
            MaybeKVs = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

:-pred unmarshal_list_of_pairs(pointer::in, string::in, string::in, int::in,
    list(value)::in, list(pair(K, V))::in,
    from_json_result(list(pair(K, V)))::out) is det
    <= (from_json(K), from_json(V)).

unmarshal_list_of_pairs(_, _, _, _, [], Pairs, ok(Pairs)).
unmarshal_list_of_pairs(Pointer, FstName, SndName, Index, [Value | Values],
        !.Pairs, Result) :-
    PairPointer = append_int_token(Pointer, Index),
    MaybePair = object_value_to_type2(PairPointer, Value, FstName, SndName,
        pair.pair),
    (
        MaybePair = ok(Pair),
        !:Pairs = [Pair | !.Pairs],
        unmarshal_list_of_pairs(Pointer, FstName, SndName, Index + 1, Values,
            !.Pairs, Result)
    ;
        MaybePair = error(Msg),
        Result = error(Msg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> rbtree/2 types.
%

rbtree_from_json(Pointer, Value) = Result :-
    ( if Value = array(Elems) then
        unmarshal_list_of_pairs(Pointer, "key", "value", 0, Elems, [],
            MaybeKVs),
        (
            MaybeKVs = ok(KVs),
            % NOTE: we cannot use rbtree.from_assoc_list/1 since that will
            % abort if there are duplicate keys.
            InsertPred = (pred((K - V)::in, !.Tree::in, !:Tree::out) is det :-
                rbtree.insert_duplicate(K, V, !Tree)
            ),
            list.foldl(InsertPred, KVs, rbtree.init, RBTree),
            Result = ok(RBTree)
        ;
            MaybeKVs = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bimap/2 types.
%

bimap_from_json(Pointer, Value) = Result :-
    ( if Value = array(Elems) then
        unmarshal_list_of_pairs(Pointer, "key", "value", 0, Elems, [], MaybeKVs),
        (
            MaybeKVs = ok(KVs),
            ( if bimap.from_assoc_list(KVs, Bimap) then
                Result = ok(Bimap)
            else
                Result = make_other_error(Pointer, "not a bijection")
            )
        ;
            MaybeKVs = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> unit/0 types.
%

unit_from_json(Pointer, JValue) =
    string_value_to_type(Pointer, JValue, string_to_unit).

:- pred string_to_unit(string::in, unit::out) is semidet.

string_to_unit("unit", unit).

%-----------------------------------------------------------------------------%
%
% JSON -> queue/1 types.
%

queue_from_json(Pointer, Value) = Result :-
    ( if Value = array(Values) then
        unmarshal_list_elems(Pointer, Values, 0, cord.empty, ElemsResult),
        (
            ElemsResult = ok(ElemsCord),
            ElemsList = cord.to_list(ElemsCord),
            Queue = queue.from_list(ElemsList),
            Result = ok(Queue)
        ;
            ElemsResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> pqueue/2 types.
%

pqueue_from_json(Pointer, Value) = Result :-
    ( if Value = array(Elems) then
        unmarshal_list_of_pairs(Pointer, "key", "value", 0, Elems, [], MaybeKVs),
        (
            MaybeKVs = ok(KVs),
            assoc_list_to_pqueue(KVs, PQueue),
            Result = ok(PQueue)
        ;
            MaybeKVs = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> digraph/1 types.
%

digraph_from_json(Pointer, Value) = Result :-
    ( if Value = object(Object) then
        Result = digraph_from_object(Pointer, Object)
    else
        Result = make_value_type_mismatch_error(Pointer, "object", Value)
    ).

:- func digraph_from_object(pointer, object) = from_json_result(digraph(T))
    <= from_json(T).

digraph_from_object(Pointer, Object) = Result :-
    ( if map.search(Object, "vertices", JVertices) then
        MaybeVertices = yes(JVertices)
    else
        MaybeVertices = no
    ),
    ( if map.search(Object, "edges", JEdges) then
        MaybeEdges = yes(JEdges)
    else
        MaybeEdges = no
    ),
    (
        MaybeVertices = no,
        MaybeEdges = no,
        Result = make_missing_members_error(Pointer,
            one_or_more("vertices", ["edges"]))
    ;
        MaybeVertices = no,
        MaybeEdges = yes(_),
        Result = make_missing_member_error(Pointer, "vertices")
    ;
        MaybeVertices = yes(_),
        MaybeEdges = no,
        Result = make_missing_member_error(Pointer, "edges")
    ;
        MaybeVertices = yes(Vertices),
        MaybeEdges = yes(Edges),
        Result = digraph_from_vertices_and_edges(Pointer, Vertices, Edges)
    ).

:- func digraph_from_vertices_and_edges(pointer, value, value) =
    from_json_result(digraph(T)) <= from_json(T).

digraph_from_vertices_and_edges(Pointer, JVertices, JEdges) = Result :-
    VerticesPointer = append_token(Pointer, "vertices"),
    MaybeVertexList = vertex_list_from_json(VerticesPointer, JVertices),
    (
        MaybeVertexList = ok(VertexList),
        EdgesPointer = append_token(Pointer, "edges"),
        MaybeEdgeList = edge_list_from_json(EdgesPointer, JEdges),
        (
            MaybeEdgeList = ok(EdgeList),
            Result = build_digraph(Pointer, VertexList, EdgeList)
        ;
            MaybeEdgeList = error(Error),
            Result = error(Error)
        )
    ;
        MaybeVertexList = error(Error),
        Result = error(Error)
    ).

:- func vertex_list_from_json(pointer, value) = from_json_result(list(T))
    <= from_json(T).

vertex_list_from_json(Pointer, Value) = Result :-
    ( if Value = array(Elems) then
        unmarshal_list_elems(Pointer, Elems, 0, cord.empty, VerticesResult),
        (
            VerticesResult = ok(VerticesCord),
            VerticesList = cord.to_list(VerticesCord),
            Result = ok(VerticesList)
        ;
            VerticesResult = error(Error),
            Result = error(Error)
        )
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

:- func edge_list_from_json(pointer, value) = from_json_result(list(pair(T)))
    <= from_json(T).

edge_list_from_json(Pointer, Value) = Result :-
    ( if Value = array(Elems) then
        unmarshal_list_of_pairs(Pointer, "source", "dest", 0, Elems, [], Result)
    else
        Result = make_value_type_mismatch_error(Pointer, "array", Value)
    ).

:- func build_digraph(pointer, list(T), list(pair(T)))
    = from_json_result(digraph(T)).

build_digraph(Pointer, Vertices, Edges) = Result :-
    some [!Digraph] (
        digraph.init(!:Digraph),
        AddVertex = (pred(V::in, !.DG::in, !:DG::out) is det :-
            digraph.add_vertex(V, _, !DG)
        ),
        list.foldl(AddVertex, Vertices, !Digraph),
        EdgesPointer = append_token(Pointer, "edges"),
        list.length(Edges, NumEdges),
        add_edges(EdgesPointer, NumEdges - 1, Edges, !.Digraph, Result)
    ).

:- pred add_edges(pointer::in, int::in, list(pair(T))::in, digraph(T)::in,
    from_json_result(digraph(T))::out) is det.

add_edges(_, _, [], Digraph, ok(Digraph)).
add_edges(Pointer, Index, [Edge | Edges], !.Digraph, Result) :-
    Edge = Src - Dst,
    ( if digraph.search_key(!.Digraph, Src, SrcKey) then
        ( if digraph.search_key(!.Digraph, Dst, DstKey) then
            digraph.add_edge(SrcKey, DstKey, !Digraph),
            add_edges(Pointer, Index - 1, Edges, !.Digraph, Result)
        else
            Msg = string.format("'%s' is not in vertex set", [s(string(Dst))]),
            DstPointer =
                append_token(append_int_token(Pointer, Index), "dest"),
            Result = make_other_error(DstPointer, Msg)
        )
    else
        SrcPointer =
            append_token(append_int_token(Pointer, Index), "source"),
        Msg = string.format("'%s' is not in vertex set", [s(string(Src))]),
        Result = make_other_error(SrcPointer, Msg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> JSON pointer.
%

json_pointer_from_json(Pointer, JValue) =
    string_value_to_type(Pointer, JValue, json.string_to_pointer).

%-----------------------------------------------------------------------------%
:- end_module json.unmarshal.
%-----------------------------------------------------------------------------%
