%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2016, 2018-2020, 2025 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Further tests of JSON->Mercury conversion.
%
%-----------------------------------------------------------------------------%

:- module test_unmarshal.
:- interface.

:- import_module io.

:- pred test_unmarshaling(io.text_output_stream::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module array.
:- import_module array2d.
:- import_module bimap.
:- import_module bitmap.
:- import_module bool.
:- import_module digraph.
:- import_module float.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module rbtree.
:- import_module type_desc.

%-----------------------------------------------------------------------------%

test_unmarshaling(File, !IO) :-
    test_unmarshal_ints(File, !IO),
    test_unmarshal_int8s(File, !IO),
    test_unmarshal_int16s(File, !IO),
    test_unmarshal_int32s(File, !IO),
    test_unmarshal_int64s(File, !IO),
    test_unmarshal_uint8s(File, !IO),
    test_unmarshal_uint16s(File, !IO),
    test_unmarshal_uint64s(File, !IO),
    test_unmarshal_floats(File, !IO),
    test_unmarshal_chars(File, !IO),
    test_unmarshal_strings(File, !IO),
    test_unmarshal_bools(File, !IO),
    test_unmarshal_integers(File, !IO),
    test_unmarshal_bitmaps(File, !IO),
    test_unmarshal_lists(File, !IO),
    test_unmarshal_arrays(File, !IO),
    test_unmarshal_array2ds(File, !IO),
    test_unmarshal_digraphs(File, !IO),
    test_unmarshal_maps(File, !IO),
    test_unmarshal_bimaps(File, !IO),
    test_unmarshal_rbtrees(File, !IO),
    test_unmarshal_one_or_mores(File, !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_ints(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_ints(File, !IO) :-
    do_unmarshal_test(File, null, _ : int, !IO),
    do_unmarshal_test(File, string("Hello"), _ : int, !IO).

:- pred test_unmarshal_int8s(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_int8s(File, !IO) :-
    do_unmarshal_test(File, null, _ : int8, !IO),
    do_unmarshal_test(File, string("Hello"), _ : int8, !IO),
    do_unmarshal_test(File, number(-129.0), _ : int8, !IO),
    do_unmarshal_test(File, number(-128.0), _ : int8, !IO),
    do_unmarshal_test(File, number(0.0), _ : int8, !IO),
    do_unmarshal_test(File, number(127.0), _ : int8, !IO),
    do_unmarshal_test(File, number(128.0), _ : int8, !IO),
    do_unmarshal_test(File, array([number(-1.0), number(0.0), number(1.0)]),
        _ : list(int8), !IO).

:- pred test_unmarshal_int16s(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_int16s(File, !IO) :-
    do_unmarshal_test(File, null, _ : int16, !IO),
    do_unmarshal_test(File, string("Hello"), _ : int16, !IO),
    do_unmarshal_test(File, number(-32769.0), _ : int16, !IO),
    do_unmarshal_test(File, number(-32768.0), _ : int16, !IO),
    do_unmarshal_test(File, number(32767.0), _ : int16, !IO),
    do_unmarshal_test(File, number(32768.0), _ : int16, !IO),
    do_unmarshal_test(File, array([number(-1.0), number(0.0), number(1.0)]),
        _ : list(int16), !IO).

:- pred test_unmarshal_int32s(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_int32s(File, !IO) :-
    do_unmarshal_test(File, null, _ : int32, !IO),
    do_unmarshal_test(File, string("Hello"), _ : int32, !IO),
    do_unmarshal_test(File, number(-2_147_483_649.0), _ : int32, !IO),
    do_unmarshal_test(File, number(-2_147_483_648.0), _ : int32, !IO),
    do_unmarshal_test(File, number(2_147_483_647.0), _ : int32, !IO),
    do_unmarshal_test(File, number(2_147_483_648.0), _ : int32, !IO),
    do_unmarshal_test(File, array([number(-1.0), number(0.0), number(1.0)]),
        _ : list(int32), !IO).

:- pred test_unmarshal_int64s(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_int64s(File, !IO) :-
    do_unmarshal_test(File, null, _ : int64, !IO),
    do_unmarshal_test(File, number(561.0), _ : int64, !IO),
    do_unmarshal_test(File, string("Hello"), _ : int64, !IO),
    do_unmarshal_test(File, string("-9223372036854775809"), _ : int64, !IO),
    do_unmarshal_test(File, string("-9223372036854775808"), _ : int64, !IO),
    do_unmarshal_test(File, string("9223372036854775807"), _ : int64, !IO),
    do_unmarshal_test(File, string("9223372036854775808"), _ : int64, !IO),
    do_unmarshal_test(File, array([string("-1"), string("0"), string("1")]),
        _ : list(int64), !IO).

:- pred test_unmarshal_uint8s(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_uint8s(File, !IO) :-
    do_unmarshal_test(File, null, _ : uint8, !IO),
    do_unmarshal_test(File, string("Hello"), _ : uint8, !IO),
    do_unmarshal_test(File, number(-1.0), _ : uint8, !IO),
    do_unmarshal_test(File, number(0.0), _ : uint8, !IO),
    do_unmarshal_test(File, number(255.0), _ : uint8, !IO),
    do_unmarshal_test(File, number(256.0), _ : uint8, !IO),
    do_unmarshal_test(File, array([number(0.0), number(1.0), number(255.0)]),
        _ : list(uint8), !IO).

:- pred test_unmarshal_uint16s(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_uint16s(File, !IO) :-
    do_unmarshal_test(File, null, _ : uint16, !IO),
    do_unmarshal_test(File, string("Hello"), _ : uint16, !IO),
    do_unmarshal_test(File, number(-1.0), _ : uint16, !IO),
    do_unmarshal_test(File, number(0.0), _ : uint16, !IO),
    do_unmarshal_test(File, number(32767.0), _ : uint16, !IO),
    do_unmarshal_test(File, number(32768.0), _ : uint16, !IO),
    do_unmarshal_test(File, array([number(0.0), number(1.0), number(32768.0)]),
        _ : list(uint16), !IO).

:- pred test_unmarshal_uint64s(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_uint64s(File, !IO) :-
    do_unmarshal_test(File, null, _ : uint64, !IO),
    do_unmarshal_test(File, number(561.0), _ : uint64, !IO),
    do_unmarshal_test(File, string("Hello"), _ : uint64, !IO),
    do_unmarshal_test(File, string("-1"), _ : uint64, !IO),
    do_unmarshal_test(File, string("0"), _ : uint64, !IO),
    do_unmarshal_test(File, string("18446744073709551615"), _ : uint64, !IO),
    do_unmarshal_test(File, string("18446744073709551616"), _ : uint64, !IO),
    do_unmarshal_test(File, array([string("0"), string("1")]),
        _ : list(uint64), !IO).

:- pred test_unmarshal_floats(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_floats(File, !IO) :-
    do_unmarshal_test(File, null, _ : float, !IO),
    do_unmarshal_test(File, string("5.61"), _ : float, !IO),
    do_unmarshal_test(File, number(float.infinity), _ : float, !IO).

:- pred test_unmarshal_chars(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_chars(File, !IO) :-
    do_unmarshal_test(File, null, _ : character, !IO),
    do_unmarshal_test(File, string("AA"), _ : character, !IO),
    do_unmarshal_test(File, string(""), _ : character, !IO).

:- pred test_unmarshal_strings(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_strings(File, !IO) :-
    do_unmarshal_test(File, null, _ : string, !IO),
    do_unmarshal_test(File, number(5.61), _ : string, !IO),
    do_unmarshal_test(File, array([]), _ : string, !IO),
    do_unmarshal_test(File, string(""), _ : string, !IO),
    do_unmarshal_test(File, string("Hello"), _ : string, !IO).

:- pred test_unmarshal_bools(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_bools(File, !IO) :-
    do_unmarshal_test(File, null, _ : bool, !IO),
    do_unmarshal_test(File, string("Hello"), _ : bool, !IO),
    do_unmarshal_test(File, number(5.61),  _ : bool, !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_integers(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_integers(File, !IO) :-
    do_unmarshal_test(File, null, _ : integer, !IO),
    do_unmarshal_test(File, string("Hello"), _ : integer, !IO),
    do_unmarshal_test(File, string("12345678912345678912346"), _ : integer,
        !IO),
    do_unmarshal_test(File, string("-123456789123456789123456"), _ : integer,
        !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_bitmaps(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_bitmaps(File, !IO) :-
    do_unmarshal_test(File, null, _ : bitmap, !IO),
    do_unmarshal_test(File, string(""), _ : bitmap, !IO),
    do_unmarshal_test(File, string("---"), _ : bitmap, !IO),
    do_unmarshal_test(File, string("<24:10AFBD>"), _ : bitmap, !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_lists(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_lists(File, !IO) :-
    do_unmarshal_test(File, null, _ : list(int), !IO),
    do_unmarshal_test(File, array([]), _ : list(int), !IO),
    do_unmarshal_test(File, array([int(1)]), _ : list(int), !IO),
    do_unmarshal_test(File, array([int(1), int(2)]), _ : list(int), !IO),
    do_unmarshal_test(File, array([int(10), null]), _ : list(int), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_arrays(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_arrays(File, !IO) :-

    % Test non-array value.
    %
    do_unmarshal_test(File, null, _ : array(int), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_array2ds(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_array2ds(File, !IO) :-

    % Test non-array value.
    %
    do_unmarshal_test(File,
        null, _ : array2d(int), !IO),

    % Test no rows.
    %
    do_unmarshal_test(File,
        array([]), _ : array2d(int), !IO),

    % Test two empty rows.
    %
    do_unmarshal_test(File,
        array([array([]), array([])]), _ : array2d(int), !IO),

    % Test first row empty, second row not an array.
    %
    do_unmarshal_test(File,
        array([array([]), null]), _ : array2d(int), !IO),

    % Test first row empty, second row not empty.
    %
    do_unmarshal_test(File,
        array([array([]), array([int(3)])]), _ : array2d(int), !IO),

    % Test first row not an array.
    %
    do_unmarshal_test(File,
        array([bool(no), array([int(3)])]), _ : array2d(int), !IO),

    % Test 3 x 3 array2d.
    %
    do_unmarshal_test(File,
        array([
            array([int(1), int(2), int(3)]),
            array([int(4), int(5), int(6)]),
            array([int(7), int(8), int(9)])
        ]), _ : array2d(int), !IO),

    % Test first and second rows have different (non-zero) lengths.
    %
    do_unmarshal_test(File,
        array([
            array([int(1)]),
            array([int(2), int(3)])
        ]), _ : array2d(int), !IO),

    % Test incorrect row type.
    %
    do_unmarshal_test(File,
        array([
            array([int(1), int(2), int(3)]),
            bool(yes),
            array([int(7), int(8), int(9)])
        ]), _ : array2d(int), !IO),

    % Test incorrect element type.
    %
    do_unmarshal_test(File,
        array([
            array([int(1), int(2), int(3)]),
            array([int(4), null, int(6)]),
            array([int(7), int(8), int(9)])
        ]), _ : array2d(int), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_digraphs(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_digraphs(File, !IO) :-

    % Test empty graph.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([]),
            "edges"    - array([])
        ]), _ : digraph(string), !IO),

    % Edge contains src not in vertex set.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([string("A"), string("B")]),
            "edges"    - array([
                            det_make_object([
                                "source" - string("C"),
                                "dest"   - string("A")
                            ])
                        ])
        ]), _ : digraph(string), !IO),

    % Edge contains dst not in vertex set.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([string("A"), string("B")]),
            "edges"    - array([
                            det_make_object([
                                "source" - string("A"),
                                "dest"   - string("C")
                            ])
                        ])
        ]), _ : digraph(string), !IO),

    % Missing 'edges' member.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([string("A"), string("B")])
        ]), _ : digraph(string), !IO),

    % Missing 'vertices' member.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "edges"    - array([
                            det_make_object([
                                "source" - string("A"),
                                "dest"   - string("C")
                            ])
                        ])
            ]), _ : digraph(string), !IO),

    % Value is not an object.
    %
    do_unmarshal_test(File,
        null, _ : digraph(string), !IO),

    % Vertex value has wrong type.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([null, string("B")]),
            "edges"    - array([])
        ]), _ : digraph(string), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_maps(io.text_output_stream::in, io::di, io::uo) is det.

test_unmarshal_maps(File, !IO) :-

    % Test empty map.
    %
    do_unmarshal_test(File, array([]), _ : map(int, int), !IO),

    % Regression test for github issue #4: the type of the expected value in
    % the error message was incorrectly reported to be "object" when it should
    % have read array.
    do_unmarshal_test(File,
        json.det_make_object([
            "foo" - array([string("a"), string("b")]),
            "bar" - array([string("c")])
        ]), _ : map(string, list(string)), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_bimaps(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_bimaps(File, !IO) :-

    % Test empty bimap.
    %
    do_unmarshal_test(File, array([]), _ : bimap(int, int), !IO),

    % Regression test for github issue #4: the type of the expected value in
    % the error message was incorrectly reported to be "object" when it should
    % have read array.
    do_unmarshal_test(File,
        json.det_make_object([
            "foo" - array([string("a"), string("b")]),
            "bar" - array([string("c")])
        ]), _ : bimap(string, list(string)), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_rbtrees(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_rbtrees(File, !IO) :-

    % Test empty rbtree.
    %
    do_unmarshal_test(File, array([]), _ : rbtree(int, int), !IO),

    % Regression test for github issue #4: the type of the expected value in
    % the error message was incorrectly reported to be "object" when it should
    % have read array.
    do_unmarshal_test(File,
        json.det_make_object([
            "foo" - array([string("a"), string("b")]),
            "bar" - array([string("c")])
        ]), _ : rbtree(string, list(string)), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_one_or_mores(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_one_or_mores(File, !IO) :-

    % Test empty array.
    %
    do_unmarshal_test(File, array([]), _ : one_or_more(string), !IO),

    % Test singleton one_or_more.
    %
    do_unmarshal_test(File, array([string("a")]), _ : one_or_more(string),
        !IO),

    % Test one_or_more with > 1 items.
    %
    do_unmarshal_test(File, array([string("a"), string("b"), string("c")]),
        _ : one_or_more(string), !IO).

%-----------------------------------------------------------------------------%

:- pred do_unmarshal_test(io.text_output_stream::in, json.value::in,
    T::unused, io::di, io::uo) is det <= from_json(T).

do_unmarshal_test(File, Value, Type, !IO) :-
    io.write_string(File, "JSON: ", !IO),
    ( if
        Value = number(N),
        is_infinite(N)
    then
        io.write_string(File, "infinity", !IO)
    else
        json.write_compact(File, Value, !IO)
    ),
    io.nl(File, !IO),
    io.write_string(File, "TYPE: ", !IO),
    TypeDesc = type_of(Type),
    io.print_line(File, TypeDesc, !IO),
    Result = json.to_type(Value),
    (
        Result = ok(Term : T),
        io.write_string(File, "RESULT: OK: ", !IO),
        io.print_line(File, Term, !IO)
    ;
        Result = error(Error),
        ErrorMsg = from_json_error_to_string(Error),
        io.write_string(File, "RESULT: ERROR: ", !IO),
        io.print_line(File, ErrorMsg, !IO)
    ),
    io.nl(File, !IO).

%-----------------------------------------------------------------------------%
:- end_module test_unmarshal.
%-----------------------------------------------------------------------------%
