%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2016 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliens@gmail.com>
%
% This module implements unmarshaling of Mercury values from JSON.
%
%-----------------------------------------------------------------------------%

:- module json.unmarshal.
:- interface.

%-----------------------------------------------------------------------------%

:- func bag_from_json(value) = maybe_error(bag(T)) <= from_json(T).
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
:- func array2d_from_json(value) = maybe_error(array2d(T)) <= from_json(T).
:- func version_array_from_json(value) = maybe_error(version_array(T))
    <= from_json(T).
:- func rational_from_json(value) = maybe_error(rational).
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
:- func maybe_error_from_json(value) = maybe_error(maybe_error(T, E))
    <= (from_json(T), from_json(E)).
:- func map_from_json(value) = maybe_error(map(K, V))
    <= (from_json(K), from_json(V)).
:- func rbtree_from_json(value) = maybe_error(rbtree(K, V))
    <= (from_json(K), from_json(V)).
:- func bimap_from_json(value) = maybe_error(bimap(K, V))
    <= (from_json(K), from_json(V)).
:- func unit_from_json(value) = maybe_error(unit).
:- func queue_from_json(value) = maybe_error(queue(T)) <= from_json(T).
:- func pqueue_from_json(value) = maybe_error(pqueue(K, V))
    <= (from_json(K), from_json(V)).
:- func digraph_from_json(value) = maybe_error(digraph(T)) <= from_json(T).
:- func json_pointer_from_json(value) = maybe_error(json.pointer).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module type_desc.

%-----------------------------------------------------------------------------%

bag_from_json(Value) = Result :-
    (
        Value = array(Elems),
        unmarshal_list_of_pairs("value", "count", Elems, [], MaybeVCs),
        (
            MaybeVCs = ok(VCs),
            add_values_and_counts(VCs, bag.init, Result)
        ;
            MaybeVCs = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = string(_)
        ; Value = object(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
    ).

:- pred add_values_and_counts(list(pair(T, int))::in, bag(T)::in,
    maybe_error(bag(T))::out) is det.

add_values_and_counts([], Bag, ok(Bag)).
add_values_and_counts([VC | VCs], !.Bag, Result) :-
    VC = Value - Count,
    ( if Count > 0 then
        % XXX POST 14.01 -- we should use bag.insert_duplicates/4 here.
        add_value_n_times(Count, Value, !Bag),
        add_values_and_counts(VCs, !.Bag, Result)
    else
        string.format("value '%s' count is less than 1", [s(string(Value))], Msg),
        Result = error(Msg)
    ).

:- pred add_value_n_times(int::in, T::in, bag(T)::in, bag(T)::out) is det.

add_value_n_times(N, Value, !Bag) :-
    ( if N > 0 then
        bag.insert(Value, !Bag),
        add_value_n_times(N - 1, Value, !Bag)
    else
        true
    ).

%-----------------------------------------------------------------------------%

int_from_json(Value) = Result :-
    ( if Value = number(Number) then
        % XXX check that Number does not have a fractional part.
        Result = ok(round_to_int(Number))
    else
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "number"),
        Result = error(ErrorMsg)
    ).

float_from_json(Value) = Result :-
    ( if Value = number(Number) then
        Result = ok(Number)
    else
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "number"),
        Result = error(ErrorMsg)
    ).

char_from_json(Value) = Result :-
    ( if Value = string(String) then
        string.length(String, Length),
        ( if
            Length = 1,
            Char = String ^ elem(0)
        then
            Result = ok(Char)
        else
            TypeDesc = type_desc_from_result(Result),
            string.format("has length %d", [i(Length)], ArgDesc),
            ErrorMsg = make_structure_error_msg(TypeDesc, ArgDesc, "length 1"),
            Result = error(ErrorMsg)
        )
    else
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "string"),
        Result = error(ErrorMsg)
    ).

string_from_json(Value) = Result :-
    ( if Value = string(String) then
        Result = ok(String)
    else
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "string"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "Boolean"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> integer/0 type.
%

integer_from_json(Value) = Result :-
    (
        Value = string(String),
        ( if Integer : integer = integer.from_string(String) then
            Result = ok(Integer)
        else
            TypeDesc = type_desc_from_result(Result),
            ErrorMsg = make_string_conv_error_msg(TypeDesc, "integer"),
            Result = error(ErrorMsg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "string"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> date/0 type.
%

date_time_from_json(Value) = Result :-
    (
        Value = string(String),
        ( if calendar.date_from_string(String, Date) then
            Result = ok(Date)
        else
            TypeDesc = type_desc_from_result(Result),
            ErrorMsg = make_string_conv_error_msg(TypeDesc, "date"),
            Result = error(ErrorMsg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "string"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> duration/0 types.
%

duration_from_json(Value) = Result :-
    (
        Value = string(String),
        ( if calendar.duration_from_string(String, Duration) then
            Result = ok(Duration)
        else
            TypeDesc = type_desc_from_result(Result),
            ErrorMsg = make_string_conv_error_msg(TypeDesc, "duration"),
            Result = error(ErrorMsg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "string"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bitmap/0 types.
%

bitmap_from_json(Value) = Result :-
    (
        Value = string(String),
        ( if Bitmap = bitmap.from_string(String) then
            Result = ok(Bitmap)
        else
            TypeDesc = type_desc_from_result(Result),
            ErrorMsg = make_string_conv_error_msg(TypeDesc, "bitmap"),
            Result = error(ErrorMsg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = object(_)
        ; Value = array(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "string"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> array2d/1 types.
%

array2d_from_json(Value) = Result :-
    (
        Value = array(RowValues),
        (
            RowValues = [],
            Array2d = array2d.from_lists([]),
            Result = ok(Array2d)
        ;
            RowValues = [FirstRowValue | RestRowValues],
            list.length(RowValues, ExpectedNumRows),
            (
                FirstRowValue = array(FirstRowValues),
                (
                    FirstRowValues = [],
                    check_array2d_rows_are_empty(1, RestRowValues, RowsAreEmptyResult),
                    (
                        RowsAreEmptyResult = crr_all_empty,
                        % NOTE: the bounds of array2d.from_lists([]) differ
                        % from those of array2d.from_lists([[],[])) etc.  I'm
                        % not sure if this behaviour was intentional, but until
                        % it is clarified we reproduce it here as well, hence
                        % the following.
                        list.duplicate(ExpectedNumRows, [], NestedEmptyLists),
                        Array2d = array2d.from_lists(NestedEmptyLists),
                        Result = ok(Array2d)
                    ;
                        RowsAreEmptyResult = crr_non_empty(FirstNonEmptyRowNo,
                            FirstNonEmptyRowLength),
                        TypeDesc = type_desc_from_result(Result),
                        TypeName = type_name(TypeDesc),
                        string.format(
                            "conversion to %s: row 0 has length 0, row %d has length %d",
                            [s(TypeName), i(FirstNonEmptyRowNo), i(FirstNonEmptyRowLength)], Msg),
                        Result = error(Msg)
                    ;
                        RowsAreEmptyResult = crr_bad_type(RowNo, RowValue),
                        TypeDesc = type_desc_from_result(Result),
                        TypeName = type_name(TypeDesc),
                        RowValueDesc = value_desc(RowValue),
                        string.format(
                            "conversion to %s: row %d is %s, expected array",
                            [s(TypeName), i(RowNo), s(RowValueDesc)], Msg),
                        Result = error(Msg)
                    )
                ;
                    FirstRowValues = [FirstElemValue | OtherElemValues],
                    list.length(FirstRowValues, ExpectedNumCols),
                    FirstElemResult = from_json(FirstElemValue),
                    (
                        FirstElemResult = ok(FirstElem),
                        some [!Array2d] (
                            !:Array2d = array2d.init(ExpectedNumRows,
                                ExpectedNumCols, FirstElem),
                            array2d_unmarshal_elems(0/*Row*/, 1/*Col*/,
                                ExpectedNumCols, OtherElemValues, !Array2d, FirstRowResult),
                            (
                                FirstRowResult = ok,
                                array2d_unmarshal_rows(1, ExpectedNumRows, ExpectedNumCols,
                                    RestRowValues, !Array2d, RestRowsResult),
                                (
                                    RestRowsResult = ok,
                                    Result = ok(!.Array2d)
                                ;
                                    RestRowsResult = error(RestRowsError),
                                    Result = error(RestRowsError)
                                )
                            ;
                                FirstRowResult = error(Msg),
                                Result = error(Msg)
                            )
                        )
                    ;
                        FirstElemResult = error(FirstElemError),
                        Result = error(FirstElemError)
                    )
                )
            ;
                ( FirstRowValue = null
                ; FirstRowValue = bool(_)
                ; FirstRowValue = string(_)
                ; FirstRowValue = number(_)
                ; FirstRowValue = object(_)
                ),
                TypeDesc = type_desc_from_result(Result),
                TypeName = type_name(TypeDesc),
                FirstRowValueDesc = value_desc(FirstRowValue),
                string.format("conversion to %s: row 0 is %s, expected array",
                    [s(TypeName), s(FirstRowValueDesc)], ErrorMsg),
                Result = error(ErrorMsg)
            )
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
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

:- pred array2d_unmarshal_rows(int::in, int::in, int::in, list(value)::in,
    array2d(T)::array_di, array2d(T)::array_uo, maybe_error::out)
    is det <= from_json(T).

array2d_unmarshal_rows(R, NumRows, NumCols, RowValues, !Array2d, Result) :-
    ( if R < NumRows then
        (
            RowValues = [],
            % This shouldn't occur since our caller checked the number of rows.
            unexpected($file, $pred, "too few rows")
        ;
            RowValues = [RowValue | RowValuesPrime],
            (
                RowValue = array(ElemValues),
                array2d_unmarshal_elems(R, 0, NumCols, ElemValues, !Array2d,
                    RowResult),
                (
                    RowResult = ok,
                    array2d_unmarshal_rows(R + 1, NumRows, NumCols, RowValuesPrime,
                        !Array2d, Result)
                ;
                    RowResult = error(_),
                    Result = RowResult
                )
            ;
                ( RowValue = null
                ; RowValue = bool(_)
                ; RowValue = string(_)
                ; RowValue = number(_)
                ; RowValue = object(_)
                ),
                TypeDesc = type_of(!.Array2d),
                TypeName = type_name(TypeDesc),
                RowValueDesc = value_desc(RowValue),
                string.format("conversion to %s: row %d is %s, expected array",
                    [s(TypeName), i(R), s(RowValueDesc)], BadRowValueErrorMsg),
                Result = error(BadRowValueErrorMsg)
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

:- pred array2d_unmarshal_elems(int::in, int::in, int::in, list(value)::in,
    array2d(T)::array2d_di, array2d(T)::array2d_uo, maybe_error::out)
    is det <= from_json(T).

array2d_unmarshal_elems(R, C, NumCols, RowValues, !Array2d, Result) :-
    ( if C < NumCols then
        (
            RowValues = [],
            TypeDesc = type_of(!.Array2d),
            TypeName = type_name(TypeDesc),
            string.format(
                "conversion to %s: row %d has length %d, expected length %d",
                [s(TypeName), i(R), i(C), i(NumCols)], ErrorMsg),
            Result = error(ErrorMsg)
        ;
            RowValues = [RowValue | RowValuesPrime],
            ElemResult = from_json(RowValue),
            (
                ElemResult = ok(Elem),
                % Safe since to reach this point we must be within the bounds
                % set when the array was created.
                array2d.unsafe_set(R, C, Elem, !Array2d),
                array2d_unmarshal_elems(R, C + 1, NumCols, RowValuesPrime,
                    !Array2d, Result)
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
            TypeName = type_name(TypeDesc),
            list.length(RowValues, NumRemainingCols),
            string.format(
                "conversion to %s: row %d has length %d, expected length %d",
                [s(TypeName), i(R), i(C + NumRemainingCols), i(NumCols)],
                ErrorMsg),
            Result = error(ErrorMsg)
        )
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
            % XXX POST 14.01 -- use version_array.from_reverse_list/1 and avoid
            % the call to reverse above.
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> rational/0 types.
%

rational_from_json(Value) = Result :-
    (
        Value = object(Object),
        ( if
            map.count(Object) = 2,
            map.search(Object, "numer", NumeratorValue),
            map.search(Object, "denom", DenominatorValue)
        then
            MaybeNumerator = integer_from_json(NumeratorValue),
            (
                MaybeNumerator = ok(Numerator),
                MaybeDenominator = integer_from_json(DenominatorValue),
                (
                    MaybeDenominator = ok(Denominator),
                    % XXX POST 14.01 -- use integer.is_zero/1 instead.
                    ( if Denominator = integer.zero then
                        Result = error("rational/0 with zero denominator")
                    else
                        Rational = rational.from_integers(Numerator, Denominator),
                        Result = ok(Rational)
                    )
                ;
                    MaybeDenominator = error(ErrorMsg),
                    Result = error(ErrorMsg)
                )
            ;
                MaybeNumerator = error(ErrorMsg),
                Result = error(ErrorMsg)
            )
        else
            Result = error("object is not a rational/0")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = array(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "object"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
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
        Value = object(Object),
        ( if
            map.count(Object) = 1,
            map.search(Object, "yes", ArgValue)
        then
            MaybeArg = from_json(ArgValue),
            (
                MaybeArg = ok(Arg),
                Result = ok(yes(Arg))
            ;
                MaybeArg = error(Msg),
                Result = error(Msg)
            )
        else
            Result = error("object is not a maybe.yes/1")
        )
    ;
        ( Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = array(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "object"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> maybe_error/2 types.
%

maybe_error_from_json(Value) = Result :-
    (
        Value = object(Object),
        ( if
            map.count(Object) = 1
        then
            ( if
                map.search(Object, "ok", OkValue)
            then
                MaybeOk = from_json(OkValue),
                (
                    MaybeOk = ok(Ok),
                    Result = ok(ok(Ok))
                ;
                    MaybeOk = error(Msg),
                    Result = error(Msg)
                )
            else if
                map.search(Object, "error", ErrorValue)
            then
                MaybeError = from_json(ErrorValue),
                (
                    MaybeError = ok(Error),
                    Result = ok(error(Error))
                ;
                    MaybeError = error(Msg),
                    Result = error(Msg)
                )
            else
                Result = error("object is not a maybe_error/2 value")
            )
        else
            Result = error("object is not a maybe_error/2 value")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = array(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "object"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> pair/2 types.
%

pair_from_json(Value) = do_pair_from_json("fst", "snd", Value).

:- func do_pair_from_json(string, string, value) = maybe_error(pair(A, B)) <=
    (from_json(A), from_json(B)).

do_pair_from_json(FstName, SndName, Value) = Result :-
    (
        Value = object(Object),
        ( if
            map.count(Object) = 2,
            map.search(Object, FstName, FstValue),
            map.search(Object, SndName, SndValue)
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "object"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> map/2 types.
%

map_from_json(Value) = Result :-
    (
        Value = array(Elems),
        unmarshal_list_of_pairs("key", "value", Elems, [], MaybeKVs),
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "object"),
        Result = error(ErrorMsg)
    ).

:-pred unmarshal_list_of_pairs(string::in, string::in, list(value)::in,
    list(pair(K, V))::in, maybe_error(list(pair(K, V)))::out) is det
    <= (from_json(K), from_json(V)).

unmarshal_list_of_pairs(_, _, [], Pairs, ok(Pairs)).
unmarshal_list_of_pairs(FstName, SndName, [Value | Values], !.Pairs, Result) :-
    MaybePair = do_pair_from_json(FstName, SndName, Value),
    (
        MaybePair = ok(Pair),
        !:Pairs = [Pair | !.Pairs],
        unmarshal_list_of_pairs(FstName, SndName, Values, !.Pairs, Result)
    ;
        MaybePair = error(Msg),
        Result = error(Msg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> rbtree/2 types.
%

rbtree_from_json(Value) = Result :-
    (
        Value = array(Elems),
        unmarshal_list_of_pairs("key", "value", Elems, [], MaybeKVs),
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "object"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> bimap/2 types.
%

bimap_from_json(Value) = Result :-
    (
        Value = array(Elems),
        unmarshal_list_of_pairs("key", "value", Elems, [], MaybeKVs),
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
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "object"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> unit/0 types.
%

unit_from_json(Value) = Result :-
    (
        Value = string(UnitStr),
        ( if UnitStr = "unit"
        then Result = ok(unit)
        else Result = error("string is not a unit/0 value")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = array(_)
        ; Value = object(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "string"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> queue/1 types.
%

queue_from_json(Value) = Result :-
    (
        Value = array(Values),
        unmarshal_list_elems(Values, [], QueueElemsResult),
        (
            QueueElemsResult = ok(RevElems),
            list.reverse(RevElems, Elems),
            Queue = queue.from_list(Elems),
            Result = ok(Queue)
        ;
            QueueElemsResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> pqueue/2 types.
%

pqueue_from_json(Value) = Result :-
    (
        Value = array(Elems),
        unmarshal_list_of_pairs("key", "value", Elems, [], MaybeKVs),
        (
            MaybeKVs = ok(KVs),
            assoc_list_to_pqueue(KVs, PQueue),
            Result = ok(PQueue)
        ;
            MaybeKVs = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = object(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "array"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> digraph/1 types.
%

digraph_from_json(Value) = Result :-
    (
        Value = object(Object),
        ( if
            map.count(Object) = 2,
            map.search(Object, "vertices", VerticesValue),
            map.search(Object, "edges", EdgesValue)
        then
            MaybeVertices = vertex_list_from_json(VerticesValue),
            (
                MaybeVertices = ok(Vertices),
                MaybeEdges = edge_list_from_json(EdgesValue),
                (
                    MaybeEdges = ok(Edges),
                    some [!Digraph] (
                        digraph.init(!:Digraph),
                        AddVertex = (pred(V::in, !.DG::in, !:DG::out) is det :-
                            digraph.add_vertex(V, _, !DG)
                        ),
                        list.foldl(AddVertex, Vertices, !Digraph),
                        add_edges(Edges, !.Digraph, Result)
                    )
                ;
                    MaybeEdges = error(Msg),
                    Result = error(Msg)
                )
            ;
                MaybeVertices = error(Msg),
                Result = error(Msg)
            )
        else
            Result = error("object is not a digraph/1")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = string(_)
        ; Value = number(_)
        ; Value = array(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "object"),
        Result = error(ErrorMsg)
    ).

:- func vertex_list_from_json(value) = maybe_error(list(T)) <= from_json(T).

vertex_list_from_json(Value) = Result :-
    (
        Value = array(Elems),
        unmarshal_list_elems(Elems, [], RevVerticesResult),
        (
            RevVerticesResult = ok(RevVertices),
            list.reverse(RevVertices, Vertices),
            Result = ok(Vertices)
        ;
            RevVerticesResult = error(Msg),
            Result = error(Msg)
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = string(_)
        ; Value = object(_)
        ),
        Result = error("expected a JSON array as value of 'vertices' member")
    ).

:- func edge_list_from_json(value) = maybe_error(list(pair(T))) <= from_json(T).

edge_list_from_json(Value) = Result :-
    (
        Value = array(Elems),
        unmarshal_list_of_pairs("source", "dest", Elems, [], Result)
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = string(_)
        ; Value = object(_)
        ),
        Result = error("expected a JSON array as value of 'edges' member")
    ).

:- pred add_edges(list(pair(T))::in, digraph(T)::in, maybe_error(digraph(T))::out) is det.

add_edges([], Digraph, ok(Digraph)).
add_edges([Edge | Edges], !.Digraph, Result) :-
    Edge = Src - Dst,
    ( if digraph.search_key(!.Digraph, Src, SrcKey) then
        ( if digraph.search_key(!.Digraph, Dst, DstKey) then
            digraph.add_edge(SrcKey, DstKey, !Digraph),
            add_edges(Edges, !.Digraph, Result)
        else
            Msg = string.format("'%s' is not in vertex set", [s(string(Dst))]),
            Result = error(Msg)
        )
    else
        Msg = string.format("'%s' is not in vertex set", [s(string(Src))]),
        Result = error(Msg)
    ).

%-----------------------------------------------------------------------------%
%
% JSON -> JSON pointer.
%

json_pointer_from_json(Value) = Result :-
    (
        Value = string(PointerStr),
        ( if json.string_to_pointer(PointerStr, Pointer)
        then Result = ok(Pointer)
        else Result = error("string is not a JSON pointer")
        )
    ;
        ( Value = null
        ; Value = bool(_)
        ; Value = number(_)
        ; Value = array(_)
        ; Value = object(_)
        ),
        TypeDesc = type_desc_from_result(Result),
        ErrorMsg = make_conv_error_msg(TypeDesc, Value, "string"),
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%
%
% Extra error handling code.
%

:- func make_conv_error_msg(type_desc, json.value, string) = string.

make_conv_error_msg(TypeDesc, Value, Expected) = Msg :-
    TypeName = type_name(TypeDesc),
    ValueDesc = value_desc(Value),
    string.format(
        "conversion to %s: argument is %s, expected %s",
        [s(TypeName), s(ValueDesc), s(Expected)], Msg).

:- func make_string_conv_error_msg(type_desc, string) = string.

make_string_conv_error_msg(TypeDesc, TargetType) = Msg :-
    TypeName = type_name(TypeDesc),
    string.format(
        "conversion to %s: cannot convert string to %s",
        [s(TypeName), s(TargetType)], Msg).

:- func make_structure_error_msg(type_desc, string, string) = string.

make_structure_error_msg(TypeDesc, ArgDesc, ExpectedDesc) = Msg :-
    TypeName = type_name(TypeDesc),
    string.format("conversion to %s: argument %s, expected %s",
        [s(TypeName), s(ArgDesc), s(ExpectedDesc)], Msg).

:- func type_desc_from_result(maybe_error(T)::unused) = (type_desc::out).

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

%-----------------------------------------------------------------------------%
:- end_module json.unmarshal.
%-----------------------------------------------------------------------------%
